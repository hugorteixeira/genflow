# Versioned service and model parameter schemas

Block contracts still own the generator signature, common defaults, input ports,
runtime restrictions and diagnostics. A parameter schema describes additional
service/model constraints and defaults. Specific provider options belong in the
closed `model_options` object; they do not become arbitrary generator arguments.

## Storage and public API

The existing provider JSON, model CSVs, favorites and local runtime configuration
are unchanged. `parameter-schemas.json` is a sidecar in `genflow.models_dir`
(or the existing `agent_models` user data directory). Writes reuse the file lock
and atomic replacement helpers. No new database or migration is required.

| Function | Purpose |
|---|---|
| `gen_parameter_schemas()` | List latest registrations; `history = TRUE` includes every revision |
| `gen_save_parameter_schema()` | Validate and append an immutable revision with optimistic concurrency |
| `gen_resolve_parameter_schema()` | Resolve service/model layers into a self-contained snapshot |
| `gen_validate_parameters()` | Apply omitted defaults and return all value errors without executing anything |

A registration is identified by `(service, model, block)`. Service aliases are
normalized; model IDs are exact and case-sensitive. `model = NULL` is the common
service layer. A generator block, such as `gen_tts`, identifies generation
parameters. `block = NULL` identifies a configuration schema: it can validate
configuration data with `gen_validate_parameters()`, but does not change a
provider URL, credentials or local runtime. Those settings retain their existing
management APIs. Listing with NULL filters includes all matching registrations.

`expected_revision = 0` creates a registration. Updating requires the revision
read by the caller. Identical retries of the last successful write are harmless;
stale, different writes fail. Previous revisions are never edited or removed.
The registry has a 16 MiB limit and corruption blocks reads/writes rather than
resetting it. The existing configuration ZIP export includes this sidecar; ZIP
import validates its contents before copying files. Importing a workflow does
**not** install its schemas in the global catalog.

## Example: native voice CFG

This registration constrains the supported native option without changing the
normal TTS block. Registration and resolution perform no inference.

```r
library(genflow)
schema <- list(
  type = "object", additionalProperties = FALSE,
  properties = list(model_options = list(
    type = "object", additionalProperties = FALSE,
    properties = list(cfg = list(type = "number", minimum = 0, default = 2))
  ))
)
record <- gen_save_parameter_schema(
  "local-native", schema, model = "voxcpm2-q8_0.gguf", block = "gen_tts"
)
profile <- gen_resolve_parameter_schema(
  "local-native", "voxcpm2-q8_0.gguf", "gen_tts"
)
checked <- gen_validate_call("gen_tts", list(
  text = "Olá, Mimi!", service = "local-native", model = "voxcpm2-q8_0.gguf",
  parameter_schema = profile, model_options = list(cfg = 1)
), version = 3)
stopifnot(checked$valid)
# checked$args can be saved as JSON or used for an explicit generation call:
# speech <- do.call(gen_tts, c(checked$args, list(persist = FALSE)))
```

Use the returned `record$revision` as `expected_revision` when saving a changed
schema. To resolve an older revision, supply `revisions = list(model = 1)`.
Each omitted layer chooses its latest revision; revision 0 excludes a layer.

## Example: reusable voice descriptions

VoxCPM2 supports `instructions` as a voice description, not a named `voice` ID.
Its installed capability schema (`inst/adapter-schemas/voxcpm2-tts.json`) is shared
by validation and form projection. The adapter owns the actual text-prefix
translation; registered schemas cannot invent a transport or enable unsupported
voice IDs. No new block contract is required to use this existing argument.

A service/model registration can constrain `instructions`, change its label and
help text, set a default, or offer descriptions as choices:

```json
{
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "instructions": {
      "type": ["string", "null"],
      "title": "Estilo da personagem",
      "description": "Descrições de voz desta história; use referência para manter o timbre.",
      "enum": [null, "Voz feminina, jovem e alegre", "Voz masculina, suave e calma"],
      "default": null
    }
  }
}
```

Save this schema for `service = "local-native"`, `block = "gen_tts"` and the exact
model ID. These are user-defined descriptions, not built-in speaker identities.
Omit `enum` to keep free-form text. Studio reads these controls through
`gen_block_configuration()`; the saved workflow keeps its schema snapshot until
explicitly refreshed. NULL overrides a registered default.

## Defaults, composition and validation

The supported vocabulary is the same as [block validation](block-contracts.md).
Each schema node has an explicit type. Objects are closed with
`additionalProperties = FALSE`. Root declarations describe properties, required
names and optional descriptive metadata; root defaults or enums are rejected
because layer composition cannot preserve those restrictions. Invalid defaults,
unsupported keywords and reserved fields are errors at registration, not silently
ignored. R character vectors for `required` and `enum` become JSON arrays even
when they contain one element; empty properties become `{}`.

Service properties are combined with exact-model properties. A model replaces a
complete property definition. For `model_options`, composition happens at the
named-option level: an option replaces its whole definition, while other service
options remain available. Required names accumulate. Parameter **values** are
never recursively merged between setups, agents and call overrides.

An explicit argument, including NULL, wins over defaults and must validate.
Omitted fields use the selected profile's defaults, then the common block
defaults. Registered canonical defaults must fit at least one retained block
version; execution always checks the explicitly selected version. This keeps old
profiles readable when a new block changes its signature. The common block also
validates the result; a profile cannot make an invalid canonical argument valid. Each option under `model_options` must be
declared, have the right type and satisfy its bounds/enums.

With no profile and no options, existing direct calls retain their behavior and
do not read the registry. Supplying nonempty `model_options` without a profile
opts into catalog resolution. The validator returns the resulting snapshot in
`args$parameter_schema`; persist these prepared arguments. To opt into profile
defaults without explicit options, resolve and supply `parameter_schema`.

`gen_validate_parameters()` checks only the declared data rules. For an actual
block, use `gen_validate_call()` or `gen_validate_structure()` as well: they check
the common contract and installed transport capabilities. Internal generator
validation follows the same path before contacting the provider. Batch workers
use a frozen registry snapshot so mid-batch catalog changes cannot affect them.
`gen_stt_signature()` uses the same effective values, distinguishing omission from
explicit NULL. Changing only schema metadata does not invalidate a transcript cache.

## Portable workflows

Current contracts supporting profiles are `gen_txt@2`, `gen_img@3`, `gen_tts@3`,
`gen_stt@2`, `gen_sfx@2` and `gen_music@2`. Old versions remain immutable; upgrading
an existing block is explicit. Profiles are ordinary JSON data inside the agent's
`parameters`, alongside `model_options` and canonical arguments.

The snapshot contains its format version, service/model/block identity, selected
service and model revisions, effective schema and SHA-256 digest. It is validated
without looking up its revisions on the destination machine. The digest detects
content changes; it is **not** a signature or administrator authorization. A
different service/model/block requires a newly resolved snapshot. Schema edits
create new catalog revisions rather than modifying an embedded hash manually.

The shared agent/workflow validators return the prepared document with snapshots
included. Studio persists newly resolved snapshots when saving an agent. Export,
import and materialization preserve that data. Import never executes generators,
changes credentials or registers a schema globally. Incomplete drafts remain
editable with their original invalid values and canonical pt-BR diagnostics.

These schemas contain neither secrets nor executable code. Portable generation
profiles cannot configure URLs, credentials, shell commands, environment
variables, tools or consent attestations. Service configuration remains a separate
management operation controlled by the calling application.

## Options must reach an implemented transport

`gen_capabilities(service)$model_options` reports the implemented option transport
per generator. A schema declares data rules; it cannot create a new adapter or
make an unsupported engine flag functional.

| Generator/service | Current option transport |
|---|---|
| `gen_txt`: OpenAI, OpenRouter, llama.cpp, registered compatible adapters and custom OpenAI-compatible providers | Validated additional JSON payload fields |
| `gen_img`: Replicate | Existing Replicate input object; do not combine nonempty `replicate_input` and `model_options` |
| `gen_tts`: local-native / VoxCPM2 | `cfg` through the engine's child-process environment |
| Other combinations | Canonical parameter constraints/defaults work; nonempty extra options fail before generation |

Options cannot shadow the canonical prompt, model, references, token controls or
other constructed payload fields. Native CFG must be a finite nonnegative number
and defaults to 2. No shell flag is inferred from an option name. The parent
environment is unchanged. Enabling another model-specific option requires an
adapter that sends it, in addition to its schema.

`gen_txt@2` accepts nonblank reasoning names so a model profile can define its own
enum. That is not evidence that a provider honors every name; configure the enum
for the selected model and check actual provider support. Existing unsupported
reasoning behavior remains owned by each adapter.

The management UI and its administrator permissions are separate work. Both
Shiny and Studio can consume this public API; neither needs its own schema engine.


## Configuração de formulários

`gen_block_configuration(block, service, model, version, parameters, resolve)`
projeta o contrato fixado, o snapshot de parâmetros e as restrições do adaptador
para controles de edição. Retorna `arguments`, `parameter_schema`, `valid` e
`errors`; `x-ui.disabled` e `x-ui.reason` explicam ajustes indisponíveis. É uma
consulta local: não executa modelos e não modifica o cadastro.

Use `resolve = TRUE` em uma seleção nova ou atualização explícita do cadastro.
Um snapshot fornecido sempre é preservado. Use `resolve = FALSE` para reabrir
configurações históricas, mantendo o comportamento anterior sem perfil. A
resolução usa os mesmos padrões de serviço/modelo do validador. Os campos não
citados pelo perfil herdam o contrato comum; limites e escolhas são restringidos,
nunca ampliados sobre as capacidades do adaptador.

Essa projeção serve aos controles, não substitui `gen_validate_call()` ou
`gen_validate_agent_definition()`: regras entre campos, entradas conectadas e
padrões de texto continuam sendo validados pela biblioteca. Objetos complexos
podem ser editados como JSON. Nenhum valor inválido é apagado para satisfazer um
schema, e nenhum schema cadastrado pode habilitar um transporte inexistente.
