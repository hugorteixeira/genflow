#!/usr/bin/env python3
"""Dependency-free tests for Hugging Face revision propagation."""

from __future__ import annotations

import argparse
import importlib.util
import sys
import types
import unittest
from pathlib import Path
from unittest.mock import patch


sys.dont_write_bytecode = True
BRIDGE_PATH = Path(sys.argv[1]).resolve()
sys.argv = [sys.argv[0]]
SPEC = importlib.util.spec_from_file_location("genflow_stt_bridge", BRIDGE_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"Could not load bridge from {BRIDGE_PATH}")
BRIDGE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BRIDGE)


class FakeDevice:
    def __init__(self, value: str):
        self.value = value
        self.type = value.split(":", 1)[0]

    def __str__(self) -> str:
        return self.value


def fake_torch_module() -> types.ModuleType:
    module = types.ModuleType("torch")
    module.device = FakeDevice
    module.float32 = "float32"
    module.float16 = "float16"
    module.bfloat16 = "bfloat16"
    module.cuda = types.SimpleNamespace(
        is_available=lambda: False,
        is_bf16_supported=lambda: False,
    )
    module.backends = types.SimpleNamespace(
        mps=types.SimpleNamespace(is_available=lambda: False)
    )
    module.version = types.SimpleNamespace(hip=None)
    return module


def bridge_args(profile: str) -> argparse.Namespace:
    return argparse.Namespace(
        audio="audio.wav",
        model="owner/model",
        revision="reviewed-commit",
        profile=profile,
        device="cpu",
        dtype="float32",
        language=None,
        prompt=None,
        trust_remote_code=profile == "moss",
        chunk_length_s=None,
        return_timestamps="none",
        max_new_tokens=None,
        output="unused.json",
    )


class RevisionPropagationTests(unittest.TestCase):
    def test_transformers_pipeline_receives_revision(self) -> None:
        captured: dict[str, object] = {}

        class FakeTranscriber:
            model = types.SimpleNamespace(
                config=types.SimpleNamespace(model_type="ctc"),
                can_generate=lambda: False,
            )

            def __call__(self, audio: str, **kwargs: object) -> dict[str, str]:
                return {"text": "generic transcript"}

        def pipeline(
            *,
            task: str,
            model: str,
            device: FakeDevice,
            trust_remote_code: bool,
            revision: str,
            dtype: object,
        ) -> FakeTranscriber:
            captured.update(
                task=task,
                model=model,
                revision=revision,
                trust_remote_code=trust_remote_code,
            )
            return FakeTranscriber()

        transformers = types.ModuleType("transformers")
        transformers.pipeline = pipeline
        with patch.dict(
            sys.modules,
            {"torch": fake_torch_module(), "transformers": transformers},
        ):
            result = BRIDGE.run_transformers(bridge_args("transformers"))

        self.assertEqual(captured["revision"], "reviewed-commit")
        self.assertEqual(result["revision"], "reviewed-commit")

    def test_moss_model_and_processor_receive_revision(self) -> None:
        captured: dict[str, dict[str, object]] = {}

        class FakeModel:
            @classmethod
            def from_pretrained(
                cls, model: str, **kwargs: object
            ) -> "FakeModel":
                captured["model"] = dict(kwargs)
                return cls()

            def to(self, *args: object, **kwargs: object) -> "FakeModel":
                return self

            def eval(self) -> "FakeModel":
                return self

        class FakeProcessor:
            @classmethod
            def from_pretrained(
                cls, model: str, **kwargs: object
            ) -> "FakeProcessor":
                captured["processor"] = dict(kwargs)
                return cls()

        transformers = types.ModuleType("transformers")
        transformers.__version__ = "5.6.0"
        transformers.AutoModelForCausalLM = FakeModel
        transformers.AutoProcessor = FakeProcessor

        moss = types.ModuleType("moss_transcribe_diarize")
        moss.__path__ = []
        moss.parse_transcript = lambda text: []
        inference = types.ModuleType("moss_transcribe_diarize.inference_utils")
        inference.build_transcription_messages = lambda audio, **kwargs: ["message"]
        inference.generate_transcription = lambda *args, **kwargs: {
            "text": "moss transcript"
        }

        with patch.dict(
            sys.modules,
            {
                "torch": fake_torch_module(),
                "transformers": transformers,
                "moss_transcribe_diarize": moss,
                "moss_transcribe_diarize.inference_utils": inference,
            },
        ):
            result = BRIDGE.run_moss(bridge_args("moss"))

        self.assertEqual(captured["model"]["revision"], "reviewed-commit")
        self.assertEqual(captured["processor"]["revision"], "reviewed-commit")
        self.assertEqual(result["revision"], "reviewed-commit")


class MossDependencyTests(unittest.TestCase):
    def test_missing_helper_hint_uses_selected_python_and_pinned_github_source(
        self,
    ) -> None:
        error = ModuleNotFoundError(
            "No module named 'moss_transcribe_diarize'",
            name="moss_transcribe_diarize",
        )

        hint = BRIDGE.dependency_hint(error, "moss")

        self.assertIn(sys.executable, hint)
        self.assertIn("-m pip install", hint)
        self.assertIn(
            "github.com/OpenMOSS/MOSS-Transcribe-Diarize/archive/", hint
        )
        self.assertIn(BRIDGE.MOSS_HELPERS_REVISION, hint)

    def test_moss_transformers_range_is_enforced(self) -> None:
        cases = {
            "5.5.4": False,
            "5.6.0": True,
            "5.99.0": True,
            "6.0.0": False,
        }

        with patch.object(
            BRIDGE,
            "moss_transformers_spec",
            return_value=">=5.6.0,<6.0.0",
        ):
            for version, compatible in cases.items():
                with self.subTest(version=version):
                    if compatible:
                        BRIDGE.ensure_moss_transformers_compatibility(version)
                    else:
                        with self.assertRaisesRegex(
                            RuntimeError, "MOSS requires Transformers"
                        ):
                            BRIDGE.ensure_moss_transformers_compatibility(version)


if __name__ == "__main__":
    unittest.main()
