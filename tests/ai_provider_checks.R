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
  provider = "none",
  allow_external = FALSE
)

check("prompt includes task", grepl("Debug R code", prompt, fixed = TRUE))
check("prompt includes question", grepl("object not found", prompt, fixed = TRUE))
check("prompt includes app role", grepl("statistics calculator, guide, and R helper", prompt, fixed = TRUE))
check("debug prompt asks for root cause", grepl("most likely cause", prompt, fixed = TRUE))
check("debug context checklist mentions str output", grepl("str() output", prompt, fixed = TRUE))

method_prompt <- build_ai_prompt(
  task = "method",
  question = "I have two independent groups and a numeric response.",
  context = "",
  provider = "none",
  allow_external = FALSE
)
check("method prompt asks variable types", grepl("Classify each variable", method_prompt, fixed = TRUE))
check("method prompt asks paired vs independent", grepl("paired samples", method_prompt, fixed = TRUE))

generate_prompt <- build_ai_prompt(
  task = "generate_r",
  question = "Write code to summarize sales by region.",
  context = "data frame: sales",
  provider = "none",
  allow_external = FALSE
)
check("generate r prompt asks runnable code", grepl("Provide runnable R code first", generate_prompt, fixed = TRUE))
check("generate r prompt discourages package install", grepl("Avoid installing packages", generate_prompt, fixed = TRUE))

interpret_prompt <- build_ai_prompt(
  task = "interpret",
  question = "Interpret this p-value.",
  context = "p-value = 0.03, alpha = 0.05",
  provider = "none",
  allow_external = FALSE
)
check("interpret prompt asks plain english", grepl("plain English", interpret_prompt, fixed = TRUE))
check("interpret prompt warns non significant", grepl("non-significant result proves no effect", interpret_prompt, fixed = TRUE))

check("task requirements returns multiple lines", length(ai_task_answer_requirements("method")) >= 6)
check("context prompt returns task text", grepl("response variable", ai_task_context_prompt("method"), fixed = TRUE))
check("manual provider exists", "none" %in% unname(ai_provider_choices))
check("mock provider removed from public choices", !"mock" %in% unname(ai_provider_choices))
check("student pathway task removed from public choices", !"student_path" %in% unname(ai_task_choices))
check("data engineering task removed from public choices", !"data_engineering" %in% unname(ai_task_choices))
check("student portfolio labels removed from task choices",
      !any(grepl("student|portfolio|learning path", names(ai_task_choices), ignore.case = TRUE)))
check("database pipeline labels removed from task choices",
      !any(grepl("data pipelines|data engineering", names(ai_task_choices), ignore.case = TRUE)))
check("codex cli provider exists", "codex_cli" %in% unname(ai_provider_choices))
check("claude code cli provider exists", "claude_code_cli" %in% unname(ai_provider_choices))
check("codex cli uses current approval config flag", grepl("approval_policy", paste(deparse(ai_codex_cli_generate), collapse = "\n"), fixed = TRUE))
check("codex cli avoids removed ask-for-approval flag", !grepl("--ask-for-approval", paste(deparse(ai_codex_cli_generate), collapse = "\n"), fixed = TRUE))
server_ai_source <- paste(readLines(file.path("R", "server_ai.R"), warn = FALSE), collapse = "\n")
check("ollama delete UI removed", !grepl("Delete Selected Ollama Model", server_ai_source, fixed = TRUE))
check("ollama delete observer removed", !grepl("ai_delete_ollama_model", server_ai_source, fixed = TRUE))
check("mock provider UI removed", !grepl("mock", server_ai_source, fixed = TRUE))
check("ollama delete helper removed", !exists("ai_ollama_delete_model"))
check("mock answer helper removed", !exists("ai_mock_answer"))

manual_result <- send_ai_request(
  prompt = prompt,
  task = "debug_r",
  question = "Why do I get object not found?",
  context = "",
  settings = ai_provider_settings("none", "", "", "", "", FALSE)
)
check("manual mode does not pretend to answer", identical(manual_result$ok, FALSE))
check("manual status", identical(manual_result$status, "prompt_only"))

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
  settings = ai_provider_settings("codex_cli", "", "", "", "CODEX_TEST_MISSING_CLI", FALSE)
)
check("missing input is rejected", identical(missing_input$status, "missing_input"))

check("localhost endpoint is local", ai_is_local_endpoint("http://localhost:1234/v1"))
check("127.0.0.1 endpoint is local", ai_is_local_endpoint("http://127.0.0.1:8080/v1"))
check("remote endpoint is not local", !ai_is_local_endpoint("https://localhost.example.com/v1"))
local_openai <- send_ai_request("p", "debug_r", "q", "", ai_provider_settings("generic_openai", "m", "http://127.0.0.1:9/v1", "STATGUIDE_TEST_NO_KEY", "", FALSE))
check("local OpenAI-compatible server skips send approval and API key", identical(local_openai$status, "online_provider_unreachable"))

if (.Platform$OS.type == "unix") {
  fake_cli <- tempfile("fake-cli-")
  writeLines(c("#!/bin/sh", "for a in \"$@\"; do echo \"ARG[$a]\"; done", "cat"), fake_cli)
  Sys.chmod(fake_cli, "755")
  marker <- tempfile("injected-")
  cli <- ai_claude_code_generate(paste0("hi; touch ", marker, " \"q\" $HOME"), list(local_app = fake_cli, model = "x y"))
  check("CLI run succeeds with shell metacharacters", isTRUE(cli$ok))
  check("CLI prompt is not executed by the shell", !file.exists(marker))
  check("CLI prompt arrives on stdin unchanged", grepl(paste0("hi; touch ", marker, " \"q\" $HOME"), cli$message, fixed = TRUE))
  check("CLI args stay intact", grepl("ARG[x y]", cli$message, fixed = TRUE))
}

check("user-info trick is not local", !ai_is_local_endpoint("https://localhost:123@attacker.example/v1"))
check("IPv6 loopback with port is local", ai_is_local_endpoint("http://[::1]:8080/v1"))
check("non-URL is not local", !ai_is_local_endpoint("localhost:1234"))
remote_ollama <- send_ai_request("p", "concept", "q", "", ai_provider_settings("ollama", "m", "http://remote.example:11434", "", "", FALSE))
check("remote Ollama needs send approval", identical(remote_ollama$status, "blocked_external_send"))

if (.Platform$OS.type == "unix") {
  failing_cli <- tempfile("failing-cli-")
  writeLines(c("#!/bin/sh", "exit 1"), failing_cli)
  Sys.chmod(failing_cli, "755")
  check("CLI check reports a failing command", identical(ai_cli_check(failing_cli, "Test CLI")$status, "cli_check_failed"))
  tilde_dir <- file.path(path.expand("~"), paste0(".statguide-test-", Sys.getpid()))
  dir.create(tilde_dir)
  writeLines(c("#!/bin/sh", "echo ran"), file.path(tilde_dir, "cli"))
  Sys.chmod(file.path(tilde_dir, "cli"), "755")
  tilde_result <- ai_cli_run(file.path("~", basename(tilde_dir), "cli"), "x")
  unlink(tilde_dir, recursive = TRUE)
  check("CLI path starting with ~ runs", isTRUE(tilde_result$ok))
}

cat("AI provider checks passed\n")
