# Versioned block contracts

Genflow packages block contracts in `inst/contracts/<block>/<version>.json`.
The installed location is `system.file("contracts", package = "genflow")`.
A contract owns argument names, required fields, types, bounds, defaults, typed
ports, and optional presentation annotations. No database or model is needed to
load or validate a contract. The catalog is cached per R session.

```r
library(genflow)
gen_block_contracts("gen_img", version = 1)
checked <- gen_validate_call("gen_img", list(prompt = "A quiet garden"))
stopifnot(checked$valid)
checked$args$steps # 18, from the contract
```

## Defaults and execution

Omitted arguments receive the contract's defaults. Explicit invalid values fail;
`steps = "18"`, `steps = NULL`, and `steps = 0` are not omission. A nullable field
can accept NULL. Service aliases and declared service-dependent model defaults
are resolved centrally; they do not query a remote model catalog. The returned
`defaulted` names identify inserted fields, and `args` contains the prepared
copy. Caller data is not mutated.

`gen_txt()`, `gen_img()`, `gen_stt()`, `gen_tts()`, and `gen_vote()` enter the same
validator before provider dispatch, audio preparation, or output directory
creation. Invalid direct calls raise `genflow_validation_error`; inspect
`condition$errors` for every independently detected argument error. Existing R
input forms such as supported data frames and tool functions remain direct-call
features, not portable JSON values.

`gen_validate_call(..., bound = c("prompt"))` marks arguments that an upstream
block will supply. Bound arguments receive no default and appear in `pending`.
Specifying both a literal and a binding is an error. The concrete value must be
validated again when executing the downstream operation. Empty strings are not
substitutes for required content.

Environment-dependent local models, credentials, executable availability,
provider-specific model support, file contents and output quality cannot be
established by static validation. Existing runtime resolution remains responsible
for those checks. A valid report is not a promise that remote inference will
succeed. `check_capabilities = FALSE` only skips the offline provider/modality
check; it does not disable argument validation.

## Portable workflow format

```json
{
  "schema_version": 1,
  "nodes": [
    {
      "id": "words",
      "block": "gen_txt",
      "contract_version": 1,
      "args": {"context": "Write one short line welcoming the audience."}
    },
    {
      "id": "voice",
      "block": "gen_tts",
      "contract_version": 1,
      "args": {"service": "openai", "speed": 1},
      "inputs": {"text": {"node": "words", "port": "content"}}
    }
  ],
  "outputs": {"audio": {"node": "voice", "port": "content"}}
}
```

Pass the JSON text or a parsed R list to `gen_validate_structure(document,
require_output = TRUE)`. Single nodes and arrays of nodes are also accepted for
validating fragments. The function validates data, never R source code; strings
are not interpreted as file paths or URLs. It does not generate media, import a
workflow into an application, or schedule execution.

The report contains `valid`, `errors`, a prepared `document`, typed `ports`, a
topological `order`, and `pending` bindings. Each error includes a stable `code`,
a JSON Pointer-style `path` and a human-readable `message`; node errors include
`node_id`. Independent errors across arguments, nodes and connections accumulate.
Malformed JSON cannot be traversed and returns a parse error. Dependent checks
are skipped when an invalid prerequisite prevents a meaningful check.

The validator checks exact contract versions, argument types and required
values, unsupported arguments, duplicate node IDs, connection references, port
types, dependency cycles, final outputs and declared subworkflows. Connections
use IDs, never canvas coordinates. Optional `label` and `metadata` are descriptive.
No arbitrary expression evaluation is supported.

A named `workflows` map may contain additional complete documents. A
`subworkflow` or `foreach` block refers to a map key through
`args.workflow_id`; its interface is derived from `input` and `output`
blocks (or the document's `outputs`). All supplied definitions are checked,
including unused ones. Recursive definitions are rejected. Limits are 10,000
nodes per graph, 64 definitions and 8 levels of subworkflow references.
Applications that already resolve immutable workflow versions may instead supply
`{ "external": true, "interface": {...}, "exports": {...} }` with declared port
types. This validates the declared interface only; the application must validate
the referenced implementation and permissions separately.

`workflow_id` is a stable workflow identity in an application and a resource key
inside a portable bundle. It is not a model identifier or a node ID. Since
0.0.23 it replaces the development field `workflow_version_id` in authored
`subworkflow` and `foreach` configurations. The library does not query a database
or choose the latest publication. Applications resolve that identity and freeze
the validated dependency graph for execution.

### Reusable selections

`gen_extract_structure(document, node_ids = NULL)` accepts a valid Studio body
or complete definition bundle and returns the ordinary extracted `document`, a
validation report and `boundary` mappings back to original source ports.
Selecting nodes preserves exactly those nodes. Incoming edges from unselected
nodes become typed `input` nodes, with their JSON schemas; shared origins remain
shared. Outgoing edges and terminal results become named public outputs. A
NULL selection extracts the whole workflow. Only referenced resources and asset
descriptors are retained. Unsupported boundary types return canonical diagnostics.

Extraction does not replace the original selection, create files, generate media
or start inference. `gen_materialize_structure()` remains responsible for prefixing
and mounting documents; its selection includes ancestors instead.

`gen_definition_references(body, kind)` reports declared workflow, agent, prompt
and asset references with JSON pointers. `gen_remap_definition(body, kind,
resources, assets)` remaps those fields only, preserving free text and arbitrary
JSON. Missing mappings are errors. Workflow mappings use stable identities;
agent and prompt mappings use their published version identities.

## Complete definition bundles

Since 0.0.12, the same validator also accepts the existing
`genflow-studio-definition` version 1 envelope. This is the portable export of a
workflow together with its agent, prompt and subworkflow definitions:

```json
{
  "format": "genflow-studio-definition", "schema_version": 1,
  "root": {"kind": "workflow", "name": "A short story", "body": {
    "schema_version": 1, "nodes": [
      {"id": "brief", "type": "input", "contract_version": 2, "config": {"key": "brief"}},
      {"id": "write", "type": "content", "contract_version": 1,
       "config": {"content_type": "text", "source": "ai", "ai": {
         "service": "openai", "model": "gpt-5-mini", "contract_version": 2,
         "parameters": {}, "content": [{"text": "Write about: "}, {"input": "brief"}]}},
       "inputs": {"brief": {"node": "brief", "port": "asset"}}},
      {"id": "out", "type": "output", "inputs": {"story": {"node": "write", "port": "asset"}}}
    ]}}, "resources": [], "assets": []
}
```

`root.kind` can also be `agent` or `prompt`. Each resource has a unique local
`key`; an exported version UUID is just one possible key. References bind only to
declared resources of the expected kind. Every resource is checked, including
unused ones. Cycles, missing resources, invalid prompts or agent parameters,
missing prompt bindings, incompatible kinds/schemas, and graph errors are
reported together. A bare Studio workflow body is accepted if self-contained;
references to external definitions require the complete bundle.

The adapter translates `type`/`config` to `block`/`args` internally and reuses the
same contract and typed-port validator. It returns `document` in the original
format with a prepared copy of each body, preserving IDs, metadata and optional
canvas coordinates. Paths point into the supplied document, for example
`/resources/0/body/parameters/temp`. `resource_order` contains keys in dependency
order for a consuming importer; validation itself does not import anything.

The v1 envelope is bounded to 3 MiB, 3,300 resources, 3,200 asset descriptors,
100 nodes per workflow, 64 subworkflow resources and 8 nested workflow levels.
A manifest asset is `{ "key": "picture", "kind": "image", "sha256": "..." }`
with a lowercase 64-digit hash. Bytes are not embedded or read. The report marks
`assets_verified: false`; the application verifies actual content, access and
runtime availability. A valid reserved operation remains unavailable for
execution until its adapter exists. Future connection values are checked at
execution, not invented by validation.

R callers may use `list()` for an empty argument object. JSON must use `{}` for
objects and `[]` for arrays; incompatible document shapes are rejected.

`check_capabilities = FALSE` is an R-side policy for isolated fixture tests,
including fixture agent controls. It is not part of the serialized block
contract and cannot be enabled by a document. Applications own that policy.

## Public API and application boundary

- `gen_block_contracts()` returns the installed catalog keyed by `block@version`.
- `gen_validate_call()` prepares defaults and checks one call without executing it.
- `gen_validate_structure()` checks a graph, fragment or complete definition bundle.
- `gen_materialize_structure()` prepares a Studio body or bundle for composition,
  selecting root nodes plus ancestors, pruning unused resources, and applying an
  optional prefix to root IDs, root interface names and dependency keys. Compact
  `block/args` graphs are not materializable Studio definitions. The report uses
  the same validators and adds `node_map`, `input_map`, `resource_sources` and
  `asset_sources`. It preserves authored argument omissions; effective defaults
  remain available in the graph metadata. Invalid input returns diagnostics.
- `gen_validate_agent_definition()` prepares a portable agent body using the same
  checks as a complete bundle, without resolving its prompt reference.
- `gen_block_ports()` checks typed connections of an already prepared block.
- `gen_check_schema()` checks the supported schema vocabulary and returns its
  `keywords`; `gen_validate_schema()` checks data without inserting defaults.

The catalog includes generation/voting functions and existing workflow control
blocks (input, lists, templates, selection, subworkflows and outputs). Control
contracts describe compatible composition; the R library does not implement a
new durable scheduler. Studio retains authentication, resource resolution,
version storage, jobs and UI layout. It consumes only exported APIs from an
installed Genflow package.

For example, `gen_materialize_structure(bundle, prefix = "scene1",
node_ids = list("out"))` includes `out` and every ancestor. The application can
show the returned document, resolve declared external inputs, and persist it in
one revision-checked transaction. The function itself does not read referenced
files, allocate IDs in a database, change a running graph or execute blocks.
Prefixes never truncate identifiers: a resulting name beyond the existing
format limits is a validation error. Nested workflows keep their own node and
interface scopes. An empty prefix preserves identifiers for full-document edits.

## Supported schema vocabulary

This is a deliberately bounded subset of JSON Schema, not a complete JSON Schema
implementation. Supported validation keywords are `type` (including type
arrays), `properties`, `required`, boolean `additionalProperties`, `items`,
`enum`, `minimum`, `maximum`, `exclusiveMinimum`, `exclusiveMaximum`, `minLength`,
`maxLength`, `pattern`, `minItems`, `maxItems`, and `uniqueItems`.

`default`, `title`, `description`, and `$comment` are annotations. Defaults are
inserted by call preparation, not by `gen_validate_schema()`. Unknown keywords
are rejected, including `$ref`, `allOf`, `anyOf`, `oneOf` and `format`. R callers
use named lists for objects and unnamed lists for arrays. An empty R `list()` is
accepted as the top-level argument map in `gen_validate_call()` for ordinary
call ergonomics. JSON documents preserve the difference between `[]` and `{}`;
schema data validation requires a named list for an object, including
`structure(list(), names = character())` for `{}`.

Genflow annotations are `x-ui` (presentation only), `x-owner` (runtime/caller
ownership of parameters in portable agent definitions),
`x-content-kind` (connection kinds), `x-r-types` (direct-R input extensions),
`x-default-by` (a declared field-dependent default/compatibility mapping) and
`x-runtime-only` (defaults excluded from portable configurations, such as an API
key resolved at runtime). Saved portable configurations reject credentials and
executable R values. Cross-field constraints and dynamic port rules remain small,
explicit library functions rather than a new expression language in JSON.

## Maintaining a contract

Version 1 is the first catalog version. Omitting a version selects 1;
prepared documents make it explicit. Unknown versions fail. Treat published
contracts and their defaults as immutable: a breaking argument/default change
requires a new contract version, corresponding execution support, and an
explicit migration when desired. Adding a JSON file alone does not implement a
new provider or a new operation.

Keep schema/default changes and the runtime boundary in the same package
release. Tests compare generator formals against contracts and verify failure
before side effects. Applications must update their installed package and start
a new R process to load a changed release; editing a source checkout does not
change a running installed namespace.

## Prepared graph metadata and drafts

Complete bundles return `graph` for the root workflow and `plans` keyed by
resource key for nested workflows. Each contains `order`, `ports`, `schemas`,
`interface`, `exports`, `height`, and `derived` static configuration. Consumers
can attach runtime identities and environment settings to that analysis instead
of reimplementing graph rules. `derived` fields are separate from the editable
`document`; they must not be written into authored node arguments.

`valid` and `errors` still describe full static validity. `bundle_errors` contains
blocking envelope, resource-reference and dependency diagnostics. Applications
may store/copy an incomplete root as a draft only after checking those diagnostics
and their own safe draft shape. They must retain the original invalid root values
and require complete validation before publishing or executing. A successful draft
import does not change `valid = FALSE` into permission to execute.

JSON comparisons retain property names, ignore object-key order, preserve array
order, and compare numbers exactly (R integer/double storage is equivalent).
Omitted counts may be inferred from agents or a versioned default; explicit null
counts are invalid and are never replaced by a default.

Catalog entries are keyed by `(id, version)`, with one supported version per ID.
An omitted version selects the current contract; preparation records it in the
document. An explicit unsupported version is an error, never an automatic upgrade.
`examples/definition-parity.json` provides shared cases for validity, preparation,
ports and schemas without provider calls.

## Image references

`gen_img()` executes its current contract (`gen_img@3`). The optional `add_img`
port accepts `asset:image|list:image`. Model mappings and reference limits are
exposed by `gen_capabilities()` and validated without provider calls.

### Native voice

`gen_tts@4` supports optional `reference_audio: asset:audio` and native controls.
Validation checks provider combinations before execution, which additionally
verifies files, GGUF architecture, engine capabilities and decoded output.

### Service/model parameter profiles

`gen_txt@2`, `gen_img@3`, `gen_tts@4`, `gen_stt@2`, `gen_sfx@2` and `gen_music@2`
add `model_options` and `parameter_schema`. Common types, runtime-owned fields
and hard adapter restrictions remain in the immutable block contracts. A selected
service/model profile adds narrower rules and omitted defaults; options must have
an implemented transport and never silently disappear. `gen_txt@2` moves the
allowed reasoning names from a global enum to model-specific profiles.

The public [parameter schema API](parameter-schemas.md) stores revisions beside
the existing catalogs. Prepared agent definitions and workflows carry their
effective schema, revisions and content hash; validation on import does not need
the original catalog. Old contracts and workflows retain their versions.

## Incomplete authoring documents

`gen_validate_structure(document, purpose = "authoring")` separates structural
`errors` from configuration `requirements`. `valid` means the structure can be
edited/materialized; `ready` also requires no pending configuration. Execution
and publication must use the default `purpose = "execution"`.
`gen_extract_structure()` and `gen_materialize_structure()` accept the same policy.
In a portable bundle, authoring applies to the root. Resources represent published
dependencies and remain complete, so validation and materialization agree.

Content, judge and transcribe nodes keep a single `config.ai` object. It contains
service, model, function contract version, parameters and ordered primary values.
Each value is exactly one of `{ "text": "literal" }`, `{ "asset": "key" }` or
`{ "input": "port" }`. Strings never interpolate variables. Parameters reuse the
public agent/function validator and preserve pinned service/model schemas.

`content.config.content_type` declares text, image, voice, music, sfx, audio or
video; its descriptor limits `source` to inline, asset and/or AI. All produce one
`asset`. Video/audio currently accept files only. Text inline/asset can use JSON
format with `output_schema`. `additions` declare a role and value, for example
`context_image`, `reference_image`, `reference_audio` or `lyrics`. Unused inputs,
unsupported roles and conflicting literal/function parameters are errors.

`text.join@2` accepts ordered `parts` (literal, asset or input) and a separator,
empty by default. It expands one layer of text/JSON collections. `list@1` combines
homogeneous items/lists in declared order. Judges and selections accept that list
directly. `asset:ballot` and `list:ballot` designate receipts whose provenance the
runtime must verify; writing a JSON object does not create a valid ballot.

## Unsupported documents

Retired piece types and contract versions are not converted or executed. Import,
materialization and normal editing use the same current validator. Applications
must remove incompatible development data before activating this catalog.
