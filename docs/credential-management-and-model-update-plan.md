# Credential Management and Model Update Plan

## Goal

Make API credentials manageable from the genflow RStudio/Shiny interface, and make
model catalog updates credential-aware. The UI should not report a successful
provider update when the provider failed because an API key is missing.

## Current Behavior

- Runtime providers read credentials directly from environment variables with
  `Sys.getenv()`.
- The documented setup path asks the user to edit `.Renviron` manually.
- The model update UI calls `gen_update_models()` and shows success when the
  function returns, but `gen_update_models()` catches provider errors internally.
  Missing keys therefore print warnings in the RStudio console while the UI can
  still say the update succeeded.
- Custom providers already expose API key environment-variable names, but there
  is no UI for creating, editing, importing, or deleting those variables.

## Design Decisions

1. Keep `Sys.getenv()` as the runtime contract.
   Provider implementations already use this path, so credentials written by the
   UI should become normal environment variables in the current R session and in
   the user's `.Renviron`.

2. Use `.Renviron` as the writable store for credentials and connection
   overrides.
   This matches R conventions and keeps secrets out of genflow cache files,
   agents, model CSVs, and exported bundles.

3. Detect, but do not silently copy, credentials from other files.
   The UI may scan the current R session, user `.Renviron`, project `.Renviron`,
   project `.env`, `~/.bashrc`, and `~/.zshrc` for simple `KEY=value` entries.
   It should show masked values and let the user explicitly import them into the
   writable `.Renviron`.

4. Mask secrets in the UI.
   Sensitive values should never be displayed in full. Editing a secret should
   use a password input; blank sensitive fields mean "leave unchanged".

5. Treat models as catalog/setup choices, not credentials.
   The credential UI should not ask for `*_MODEL` variables. The user should
   set credentials, update the provider catalog, and then choose a model from
   the setup/agent controls.

6. Back up `.Renviron` before edits.
   Add, modify, import, and delete operations should copy the existing file to a
   timestamped backup before replacing credential lines.

7. Preflight model updates.
   Before updating a provider whose catalog endpoint requires a key, the UI
   should check whether the required environment variable or alias group is
   active in the current R session. If missing, it should offer to add/import the
   credential before the update runs.

8. Make update failures observable.
   `gen_update_models()` should support a checked mode for UI callers, so a
   provider failure can become a real error instead of only a console warning.

## Implementation Phases

### Phase 1 - Credential metadata and helpers

- Add an internal credential helper module.
- Define provider credential specs for built-in runtime/update providers:
  OpenAI, OpenRouter, Anthropic/Claude, Groq, Cerebras, Together, SambaNova,
  Nebius, DeepSeek, Perplexity, Fireworks, DeepInfra, Hyperbolic, Gemini, Fal,
  Replicate, Hugging Face, AssemblyAI, Cloudflare, Voicegain, Ollama, and
  llama-cpp.
- Merge custom provider specs from the existing custom-provider registry.
- Add helpers to:
  - parse simple environment assignment files;
  - detect candidate credentials from known user/project files;
  - mask sensitive values;
  - summarize provider credential status;
  - identify missing required credentials for model updates;
  - save/update credentials in `.Renviron`;
  - delete credentials from `.Renviron` and the current session.
- Do not expose model-selection environment variables in the credential UI.

### Phase 2 - UI integration

- Add a Credentials panel to the existing Models tab.
- Provide provider selection, credential status, detected-source summary, and
  buttons for:
  - Add/edit credentials;
  - Import detected values;
  - Delete credentials;
  - Refresh status.
- Use modal dialogs for credential edits, imports, and delete confirmation.
- Set variables in the current R session immediately after saving/importing so
  the user does not need to restart R before updating models.

### Phase 3 - Model update preflight

- For "Update selected provider", check the selected provider before calling the
  updater. If credentials are missing, open the credential dialog instead of
  calling the provider API.
- For "Update all", check all selectable providers. If some require missing
  credentials, offer to configure a missing provider or skip missing providers
  and update only providers that are ready.
- After updates, report exactly which providers updated, failed, or were skipped.

### Phase 4 - Checked update mode

- Extend `gen_update_models()` with an additive `fail_on_error` argument.
- Preserve the default CLI behavior (`fail_on_error = FALSE`) for compatibility.
- Have the UI call `gen_update_models(..., fail_on_error = TRUE)` so provider
  failures become visible to the UI.

### Phase 5 - Documentation

- Update README setup instructions to mention the Credentials panel.
- Update `gen_update_models` documentation for the new checked mode.

### Phase 6 - Verification

- Run helper-level smoke tests with temporary `.Renviron` files.
- Verify missing OpenAI credentials are detected before the updater tries the
  OpenAI API.
- Run package load or source-level checks where feasible.
- Run `git diff --check`.

## Non-Goals

- Do not store secrets in genflow setup, agent, content, or model catalog files.
- Do not implement OS keychain/keyring storage in this pass.
- Do not rewrite provider runtime credential lookup away from `Sys.getenv()`.
- Do not use the credential UI as a model picker.
- Do not silently parse complex shell expressions such as command substitution.
