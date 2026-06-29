source(file.path("R", "ai_providers.R"))

check <- function(label, condition) {
  if (!isTRUE(condition)) {
    stop(paste("AI provider check failed:", label), call. = FALSE)
  }
}

prompt <- build_ai_prompt(
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "lm(y ~ x, data = df)",
  provider = "mock",
  allow_external = FALSE
)

check("prompt includes task", grepl("Debug R code", prompt, fixed = TRUE))
check("prompt includes question", grepl("object not found", prompt, fixed = TRUE))
check("manual provider exists", "none" %in% unname(ai_provider_choices))
check("mock provider exists", "mock" %in% unname(ai_provider_choices))
check("codex cli provider exists", "codex_cli" %in% unname(ai_provider_choices))
check("claude code cli provider exists", "claude_code_cli" %in% unname(ai_provider_choices))
check("codex cli uses current approval config flag", grepl("approval_policy", paste(deparse(ai_codex_cli_generate), collapse = "\n"), fixed = TRUE))
check("codex cli avoids removed ask-for-approval flag", !grepl("--ask-for-approval", paste(deparse(ai_codex_cli_generate), collapse = "\n"), fixed = TRUE))

manual_result <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("none", "", "", "", "", FALSE)
)
check("manual mode does not pretend to answer", identical(manual_result$ok, FALSE))
check("manual status", identical(manual_result$status, "prompt_only"))

mock_result <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("mock", "", "", "", "", FALSE)
)
check("mock mode returns ok", identical(mock_result$ok, TRUE))
check("mock mode status", identical(mock_result$status, "mock_response"))
check("mock response labels itself", grepl("Mock AI response", mock_result$message, fixed = TRUE))

check("ollama endpoint default", identical(ai_ollama_endpoint(""), "http://localhost:11434"))
check("ollama endpoint trims slash", identical(ai_ollama_endpoint("http://localhost:11434/"), "http://localhost:11434"))

ollama_missing_model <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("ollama", "", "", "", "", FALSE)
)
check("ollama requires model", identical(ollama_missing_model$status, "ollama_missing_model"))

ollama_delete_missing_model <- ai_ollama_delete_model("", "")
check("ollama delete requires model", identical(ollama_delete_missing_model$status, "ollama_delete_missing_model"))
check("openai endpoint default", identical(ai_openai_endpoint("", "openai"), "https://api.openai.com/v1"))
check("openai endpoint trims slash", identical(ai_openai_endpoint("https://example.com/v1/", "generic_openai"), "https://example.com/v1"))
check("generic endpoint requires explicit endpoint", identical(ai_openai_endpoint("", "generic_openai"), ""))

openai_missing_model <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("openai", "", "", "OPENAI_API_KEY", "", TRUE)
)
check("openai-compatible requires model", identical(openai_missing_model$status, "missing_model"))

generic_missing_endpoint <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("generic_openai", "model", "", "OPENAI_API_KEY", "", TRUE)
)
check("generic openai-compatible requires endpoint", identical(generic_missing_endpoint$status, "missing_endpoint"))

openai_missing_credentials <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("openai", "model", "", "CODEX_TEST_MISSING_KEY", "", TRUE)
)
check("openai-compatible requires credentials", identical(openai_missing_credentials$status, "missing_credentials"))
check("anthropic endpoint default", identical(ai_anthropic_endpoint(""), "https://api.anthropic.com/v1"))
check("anthropic endpoint trims slash", identical(ai_anthropic_endpoint("https://api.anthropic.com/v1/"), "https://api.anthropic.com/v1"))
check("anthropic key env default", identical(ai_anthropic_key_env(""), "ANTHROPIC_API_KEY"))

anthropic_missing_model <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("anthropic", "", "", "ANTHROPIC_API_KEY", "", TRUE)
)
check("anthropic requires model", identical(anthropic_missing_model$status, "missing_model"))

anthropic_missing_credentials <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("anthropic", "claude-sonnet-4-20250514", "", "CODEX_TEST_MISSING_ANTHROPIC_KEY", "", TRUE)
)
check("anthropic requires credentials", identical(anthropic_missing_credentials$status, "missing_credentials"))

missing_codex_cli <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("codex_cli", "", "", "", "CODEX_TEST_MISSING_CLI", FALSE)
)
check("missing codex cli command is reported", identical(missing_codex_cli$status, "cli_not_found"))

missing_claude_code_cli <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("claude_code_cli", "", "", "", "CODEX_TEST_MISSING_CLAUDE_CODE_CLI", FALSE)
)
check("missing claude code cli command is reported", identical(missing_claude_code_cli$status, "cli_not_found"))

blocked_result <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("openai", "model", "", "OPENAI_API_KEY", "", FALSE)
)
check("online provider blocked without approval", identical(blocked_result$status, "blocked_external_send"))

missing_input <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "",
  context = "",
  settings = ai_provider_settings("mock", "", "", "", "", FALSE)
)
check("missing input is rejected", identical(missing_input$status, "missing_input"))

cat("AI provider checks passed\n")
