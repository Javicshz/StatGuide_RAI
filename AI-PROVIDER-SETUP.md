# StatGuide R AI Provider Setup

This app can work without AI, with local CLI tools, with a local Ollama model, or with online providers through environment variables.

Do not paste raw API keys into the app UI or commit them to the repository. The app expects the name of an environment variable, such as `OPENAI_API_KEY` or `ANTHROPIC_API_KEY`. The example keys below are placeholders only.

## Manual / Prompt Only

No setup required.

Use this when you want the app to prepare a structured prompt that you copy into another tool yourself.

## Ollama Local Model

1. Install and start Ollama.
2. Pull a model from Terminal:

```bash
ollama pull llama3.1
```

3. Open the app.
4. Go to **AI Settings**.
5. Choose **Ollama local model**.
6. Keep the endpoint as:

```text
http://localhost:11434
```

7. Click **Refresh Ollama Models**.
8. Choose an installed model from the dropdown.
9. Go to **Ask AI**, enter a request, and click **Ask AI**.

Model management should be done directly in Ollama. This app lists and uses installed models, but it does not provide destructive model-delete controls.

## Codex CLI (Local)

Use this when Codex already works from Terminal through the Codex CLI.

No OpenAI API key is entered in the app. The app calls local `codex exec`, which uses your existing Codex CLI login/session.

In the app:

1. Go to **AI Settings**.
2. Choose **Codex CLI (local)**.
3. Leave the command blank to use the bundled Codex CLI when available, or enter:

```text
/Applications/Codex.app/Contents/Resources/codex
```

4. Optionally enter a model override.
5. Click **Review Settings**.
6. Go to **Ask AI**, enter a request, and click **Ask AI**.

The app runs Codex CLI in non-interactive read-only mode for this workflow.

## Claude Code CLI (Local)

Use this when Claude Code already works from Terminal.

No Anthropic API key is entered in the app. The app calls local Claude Code CLI, which uses your existing Claude Code login/session.

In the app:

1. Go to **AI Settings**.
2. Choose **Claude Code CLI (local)**.
3. Enter the command name or path, usually:

```text
claude
```

4. Optionally enter a model override.
5. Click **Review Settings**.
6. Go to **Ask AI**, enter a request, and click **Ask AI**.

If the app reports `cli_not_found`, install Claude Code or enter the full path to its executable.

## OpenAI-Compatible API

Set an environment variable outside the app. For persistent local setup, use your user `.Renviron`:

```r
usethis::edit_r_environ()
```

Add:

```text
OPENAI_API_KEY=your-key-here
```

Restart RStudio.

In the app:

1. Go to **AI Settings**.
2. Choose **OpenAI-compatible API**.
3. Enter a model name.
4. Leave the endpoint blank to use:

```text
https://api.openai.com/v1
```

5. Enter:

```text
OPENAI_API_KEY
```

6. Click **Review Settings** to test `/models`.
7. In **Ask AI**, check the external-send approval box before sending.

## Anthropic / Claude API

Set an environment variable outside the app. For persistent local setup, use your user `.Renviron`:

```text
ANTHROPIC_API_KEY=your-key-here
```

Restart RStudio.

In the app:

1. Go to **AI Settings**.
2. Choose **Claude / Anthropic API**.
3. Enter a Claude model name.
4. Leave the endpoint blank to use:

```text
https://api.anthropic.com/v1
```

5. Enter:

```text
ANTHROPIC_API_KEY
```

6. Click **Review Settings** to test `/models`.
7. In **Ask AI**, check the external-send approval box before sending.

## Generic OpenAI-Compatible Endpoint

Use this for providers that support OpenAI-style chat completions.

In **AI Settings**, provide:

- model name
- endpoint URL, such as `https://provider.example.com/v1`
- API key environment variable name

Then check external-send approval in **Ask AI** before sending.

## Privacy Checklist

Before sending to an online provider:

- Remove private data, secrets, API keys, tokens, and regulated information.
- Prefer summaries or small examples instead of full datasets.
- Review generated R code before running it.
- Verify statistical assumptions and calculations.
- Keep raw data local unless you have permission to share it.
