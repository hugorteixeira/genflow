#!/usr/bin/env python3
"""Optional local speech-to-text bridge for genflow.

The bridge deliberately communicates through one JSON file. R does not import
Python or own the model process, so users can point GENFLOW_PYTHON at an
isolated PyTorch/Transformers environment (including a ROCm environment).
"""

from __future__ import annotations

import argparse
import inspect
import json
import os
import shlex
import subprocess
import sys
import traceback
from collections.abc import Mapping
from importlib import metadata
from pathlib import Path
from typing import Any


MOSS_HELPERS_REVISION = "9990574e6ac62390a21bcce25a914d66ac92c25e"
MOSS_HELPERS_URL = (
    "https://github.com/OpenMOSS/MOSS-Transcribe-Diarize/archive/"
    f"{MOSS_HELPERS_REVISION}.zip"
)
MOSS_TRANSFORMERS_SPEC = ">=5.6.0,<6.0.0"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="genflow local STT bridge")
    parser.add_argument("--audio", required=True)
    parser.add_argument("--model", required=True)
    parser.add_argument(
        "--revision",
        help="Optional Hugging Face branch, tag, or immutable commit SHA.",
    )
    parser.add_argument(
        "--profile", choices=("transformers", "moss"), default="transformers"
    )
    parser.add_argument("--device", default="auto")
    parser.add_argument(
        "--dtype",
        choices=("auto", "float32", "float16", "bfloat16"),
        default="auto",
    )
    parser.add_argument("--language")
    parser.add_argument("--prompt")
    parser.add_argument("--trust-remote-code", action="store_true")
    parser.add_argument("--chunk-length-s", type=float)
    parser.add_argument(
        "--return-timestamps", choices=("none", "true", "word"), default="none"
    )
    parser.add_argument("--max-new-tokens", type=int)
    parser.add_argument("--output", required=True)
    return parser.parse_args()


def json_safe(value: Any) -> Any:
    if value is None or isinstance(value, (str, bool, int, float)):
        return value
    if isinstance(value, Mapping):
        return {str(key): json_safe(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [json_safe(item) for item in value]
    if hasattr(value, "item"):
        try:
            return json_safe(value.item())
        except (TypeError, ValueError):
            pass
    if hasattr(value, "__dict__"):
        return {
            str(key): json_safe(item)
            for key, item in vars(value).items()
            if not str(key).startswith("_")
        }
    return str(value)


def write_payload(path: str, payload: dict[str, Any]) -> None:
    output = Path(path)
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_suffix(output.suffix + ".tmp")
    temporary.write_text(
        json.dumps(json_safe(payload), ensure_ascii=False), encoding="utf-8"
    )
    temporary.replace(output)


def moss_install_command(python: str | None = None) -> str:
    requirement = f"moss-transcribe-diarize @ {MOSS_HELPERS_URL}"
    arguments = [python or sys.executable, "-m", "pip", "install", requirement]
    if os.name == "nt":
        return subprocess.list2cmdline(arguments)
    return shlex.join(arguments)


def moss_transformers_spec() -> str:
    """Return the requirement declared by the installed helper when available."""
    try:
        from packaging.requirements import Requirement

        for raw_requirement in metadata.requires("moss-transcribe-diarize") or []:
            requirement = Requirement(raw_requirement)
            normalized_name = requirement.name.lower().replace("_", "-")
            if normalized_name == "transformers" and str(requirement.specifier):
                return str(requirement.specifier)
    except (metadata.PackageNotFoundError, ImportError, ValueError):
        pass
    return MOSS_TRANSFORMERS_SPEC


def ensure_moss_transformers_compatibility(version: str) -> None:
    try:
        from packaging.specifiers import SpecifierSet
        from packaging.version import InvalidVersion, Version
    except ImportError as exc:
        raise RuntimeError(
            f"Could not import packaging to validate Transformers: {exc}"
        ) from exc

    try:
        spec = moss_transformers_spec()
        compatible = SpecifierSet(spec).contains(Version(version), prereleases=True)
    except (InvalidVersion, ValueError) as exc:
        raise RuntimeError(
            f"Could not validate the installed Transformers version: {exc}"
        ) from exc
    if not compatible:
        raise RuntimeError(
            f"MOSS requires Transformers {spec}, but {version} is installed."
        )


def dependency_hint(exc: BaseException, profile: str) -> str:
    missing = getattr(exc, "name", "") if isinstance(exc, ModuleNotFoundError) else ""
    if missing in {"torch", "transformers"}:
        return (
            "Install PyTorch and Transformers in the Python environment selected "
            "by `python` or GENFLOW_PYTHON."
        )
    if missing and missing.split(".", 1)[0] == "moss_transcribe_diarize":
        return (
            "The Hugging Face model files do not include the separately packaged "
            "MOSS inference helpers. Install the pinned official GitHub source "
            f"into this exact interpreter with: {moss_install_command()}"
        )
    if profile == "moss":
        return (
            "MOSS requires its official GitHub helper, a compatible PyTorch "
            f"build, and Transformers {MOSS_TRANSFORMERS_SPEC}. Install the "
            f"helper with: {moss_install_command()}"
        )
    return (
        "Check the selected Python environment, model files, audio decoder, and "
        "PyTorch accelerator build."
    )


def resolve_device(torch: Any, requested: str) -> tuple[Any, str]:
    requested = requested.lower().strip()
    rocm_requested = requested in {"rocm", "hip"}
    if rocm_requested:
        requested = "cuda:0"

    if requested == "auto":
        if torch.cuda.is_available():
            requested = "cuda:0"
        elif (
            hasattr(torch.backends, "mps")
            and torch.backends.mps.is_available()
        ):
            requested = "mps"
        else:
            requested = "cpu"

    device = torch.device(requested)
    if device.type == "cuda":
        if not torch.cuda.is_available():
            raise RuntimeError(
                "A CUDA/ROCm device was requested, but this PyTorch build reports "
                "torch.cuda.is_available() == False."
            )
        is_rocm = bool(getattr(torch.version, "hip", None))
        if rocm_requested and not is_rocm:
            raise RuntimeError(
                "`device = \"rocm\"` was requested, but the selected PyTorch "
                "environment is not a ROCm build."
            )
        accelerator = "rocm" if is_rocm else "cuda"
    elif device.type == "mps":
        available = (
            hasattr(torch.backends, "mps")
            and torch.backends.mps.is_available()
        )
        if not available:
            raise RuntimeError(
                "An MPS device was requested, but MPS is not available."
            )
        accelerator = "mps"
    elif device.type == "cpu":
        accelerator = "cpu"
    else:
        raise ValueError(f"Unsupported local STT device: {device}")

    return device, accelerator


def resolve_dtype(
    torch: Any, requested: str, device: Any, profile: str, warnings: list[str]
) -> tuple[Any, str]:
    dtype_name = requested
    if requested == "auto":
        if device.type == "cpu":
            dtype_name = "float32"
        elif profile == "moss" and device.type == "cuda":
            supports_bf16 = bool(
                getattr(torch.cuda, "is_bf16_supported", lambda: False)()
            )
            if supports_bf16:
                dtype_name = "bfloat16"
            else:
                dtype_name = "float16"
                warnings.append(
                    "bfloat16 is unavailable on this accelerator; using float16."
                )
        else:
            dtype_name = "float16"

    table = {
        "float32": torch.float32,
        "float16": torch.float16,
        "bfloat16": torch.bfloat16,
    }
    if dtype_name == "bfloat16" and device.type == "cuda":
        supports_bf16 = bool(
            getattr(torch.cuda, "is_bf16_supported", lambda: False)()
        )
        if not supports_bf16:
            raise RuntimeError(
                "bfloat16 was requested, but the selected accelerator does not "
                "report bfloat16 support."
            )
    return table[dtype_name], dtype_name


def normalize_pipeline_result(result: Any) -> tuple[str, dict[str, Any]]:
    if isinstance(result, str):
        return result, {}
    if isinstance(result, Mapping):
        text = result.get("text") or result.get("transcription")
        if not isinstance(text, str) or not text.strip():
            raise RuntimeError("Transformers returned an empty transcript.")
        metadata = {
            str(key): json_safe(value)
            for key, value in result.items()
            if key not in {"text", "transcription"}
        }
        return text, metadata
    if isinstance(result, list) and result:
        first = result[0]
        if isinstance(first, Mapping):
            return normalize_pipeline_result(first)
    raise RuntimeError(
        f"Unsupported Transformers ASR result type: {type(result).__name__}"
    )


def run_transformers(args: argparse.Namespace) -> dict[str, Any]:
    import torch
    from transformers import pipeline

    warnings: list[str] = []
    device, accelerator = resolve_device(torch, args.device)
    dtype, dtype_name = resolve_dtype(
        torch, args.dtype, device, "transformers", warnings
    )

    pipeline_kwargs: dict[str, Any] = {
        "task": "automatic-speech-recognition",
        "model": args.model,
        "device": device,
        "trust_remote_code": args.trust_remote_code,
    }
    if args.revision:
        pipeline_kwargs["revision"] = args.revision
    precision_argument = (
        "dtype" if "dtype" in inspect.signature(pipeline).parameters
        else "torch_dtype"
    )
    pipeline_kwargs[precision_argument] = dtype
    transcriber = pipeline(**pipeline_kwargs)

    call_kwargs: dict[str, Any] = {}
    if args.chunk_length_s is not None:
        call_kwargs["chunk_length_s"] = args.chunk_length_s
    if args.return_timestamps == "true":
        call_kwargs["return_timestamps"] = True
    elif args.return_timestamps == "word":
        call_kwargs["return_timestamps"] = "word"

    generation_kwargs: dict[str, Any] = {}
    model_type = str(getattr(transcriber.model.config, "model_type", "")).lower()
    if args.language:
        if model_type == "whisper":
            generation_kwargs["language"] = args.language
        else:
            warnings.append(
                "The language hint is only forwarded automatically to Whisper "
                "models; it was ignored for this model."
            )
    if args.prompt:
        tokenizer = getattr(transcriber, "tokenizer", None)
        get_prompt_ids = getattr(tokenizer, "get_prompt_ids", None)
        if model_type == "whisper" and callable(get_prompt_ids):
            try:
                prompt_ids = get_prompt_ids(args.prompt, return_tensors="pt")
            except TypeError:
                prompt_ids = get_prompt_ids(args.prompt)
            if hasattr(prompt_ids, "to"):
                prompt_ids = prompt_ids.to(device)
            generation_kwargs["prompt_ids"] = prompt_ids
        else:
            warnings.append(
                "This model does not expose Whisper prompt ids; the prompt was "
                "ignored."
            )
    if args.max_new_tokens is not None:
        can_generate = getattr(transcriber.model, "can_generate", lambda: True)
        if can_generate():
            generation_kwargs["max_new_tokens"] = args.max_new_tokens
        else:
            warnings.append(
                "This ASR model is not generative; max_new_tokens was ignored."
            )
    if generation_kwargs:
        call_kwargs["generate_kwargs"] = generation_kwargs

    result = transcriber(args.audio, **call_kwargs)
    text, result_metadata = normalize_pipeline_result(result)
    return {
        "ok": True,
        "text": text,
        "backend": "transformers",
        "profile": "transformers",
        "model": args.model,
        "revision": args.revision,
        "device": str(device),
        "accelerator": accelerator,
        "dtype": dtype_name,
        "warnings": warnings,
        **result_metadata,
    }


def moss_segments(parse_transcript: Any, text: str) -> list[dict[str, Any]]:
    segments = []
    for segment in parse_transcript(text):
        segments.append(
            {
                "start": getattr(segment, "start", None),
                "end": getattr(segment, "end", None),
                "speaker": getattr(segment, "speaker", None),
                "text": getattr(segment, "text", None),
            }
        )
    return segments


def run_moss(args: argparse.Namespace) -> dict[str, Any]:
    if not args.trust_remote_code:
        raise ValueError(
            "The MOSS profile requires --trust-remote-code for its official "
            "Transformers implementation."
        )

    import torch
    import transformers
    from transformers import AutoModelForCausalLM, AutoProcessor

    ensure_moss_transformers_compatibility(transformers.__version__)

    from moss_transcribe_diarize import parse_transcript
    from moss_transcribe_diarize.inference_utils import (
        build_transcription_messages,
        generate_transcription,
    )

    warnings: list[str] = []
    device, accelerator = resolve_device(torch, args.device)
    dtype, dtype_name = resolve_dtype(torch, args.dtype, device, "moss", warnings)
    if args.language:
        warnings.append(
            "MOSS performs language detection internally; the separate language "
            "hint was ignored."
        )
    if args.chunk_length_s is not None:
        warnings.append(
            "MOSS manages long-form audio internally; chunk_length_s was ignored."
        )
    if args.return_timestamps == "word":
        warnings.append(
            "MOSS returns speaker-segment timestamps, not word timestamps."
        )

    revision_kwargs = {"revision": args.revision} if args.revision else {}
    model = AutoModelForCausalLM.from_pretrained(
        args.model,
        trust_remote_code=True,
        dtype="auto",
        **revision_kwargs,
    ).to(dtype=dtype).to(device).eval()
    processor = AutoProcessor.from_pretrained(
        args.model,
        trust_remote_code=True,
        **revision_kwargs,
    )
    messages = (
        build_transcription_messages(args.audio, prompt=args.prompt)
        if args.prompt
        else build_transcription_messages(args.audio)
    )
    generated = generate_transcription(
        model,
        processor,
        messages,
        max_new_tokens=args.max_new_tokens or 2048,
        do_sample=False,
        device=device,
        dtype=dtype,
    )
    text = generated.get("text") if isinstance(generated, Mapping) else None
    if not isinstance(text, str) or not text.strip():
        raise RuntimeError("MOSS returned an empty transcript.")

    extra = {
        str(key): json_safe(value)
        for key, value in generated.items()
        if key != "text"
    }
    return {
        "ok": True,
        "text": text,
        "backend": "transformers",
        "profile": "moss",
        "model": args.model,
        "revision": args.revision,
        "device": str(device),
        "accelerator": accelerator,
        "dtype": dtype_name,
        "warnings": warnings,
        "segments": moss_segments(parse_transcript, text),
        **extra,
    }


def main() -> int:
    args = parse_args()
    try:
        payload = run_moss(args) if args.profile == "moss" else run_transformers(args)
        write_payload(args.output, payload)
        return 0
    except BaseException as exc:  # structured diagnostics must survive Python exit
        payload = {
            "ok": False,
            "error_type": type(exc).__name__,
            "error": str(exc),
            "hint": dependency_hint(exc, args.profile),
            "profile": args.profile,
            "model": args.model,
            "revision": args.revision,
        }
        if os.getenv("GENFLOW_STT_DEBUG", "").lower() in {"1", "true", "yes"}:
            payload["traceback"] = traceback.format_exc()
        write_payload(args.output, payload)
        print(f"genflow local STT error: {type(exc).__name__}: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
