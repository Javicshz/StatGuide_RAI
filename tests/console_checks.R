# Checks for RStudio Console helpers.
# These do not call external AI tools; they only verify local wiring.

source(file.path("R", "ai_providers.R"))
source(file.path("R", "console_ai.R"))
source(file.path("R", "content_data.R"))
source(file.path("R", "calculator_reference.R"))
source(file.path("R", "console_menu.R"))

check <- function(label, condition) {
  if (!isTRUE(condition)) {
    stop(paste("Console check failed:", label), call. = FALSE)
  }
}

check("codex helper exists", is.function(codex))
check("askcodex helper exists", is.function(askcodex))
check("generic ai helper exists", is.function(askai))
check("ollama helper exists", is.function(ollama))
check("capitalized ollama helper exists", is.function(Ollama))
check("claude helper exists", is.function(claude))
check("openai helper exists", is.function(openai))
check("anthropic helper exists", is.function(anthropic))
check("openai compatible helper exists", is.function(openai_compatible))
check("ai setup help exists", is.function(ai_setup_help))
check("statguide console menu exists", is.function(statguide_console))
check("calculator console menu exists", is.function(statguide_console_calculators))
check("console loader exists", file.exists("run_console.R"))
check("console loader parses", length(parse("run_console.R")) > 0)
check("ai-only loader exists", file.exists("run_ai.R"))
check("ai-only loader parses", length(parse("run_ai.R")) > 0)
check("gui app entry exists", file.exists("app.R"))

prompt <- statguide_console_prompt(
  "Provide R code for a Poisson probability problem with lambda 2 and x 3.",
  task = "generate_r"
)

check("console prompt includes question", grepl("Poisson probability", prompt, fixed = TRUE))
check("console prompt uses Codex provider mode", grepl("Provider mode: Codex CLI", prompt, fixed = TRUE))
check("console prompt uses R code task", grepl("Task: Generate R code", prompt, fixed = TRUE))
check("console prompt asks for runnable R", grepl("Provide runnable R code first", prompt, fixed = TRUE))

fallback_prompt <- statguide_console_prompt(
  "Which statistical test should I use for two independent numeric groups?",
  task = "not_a_real_task"
)

check("unknown console task falls back to generate R", grepl("Task: Generate R code", fallback_prompt, fixed = TRUE))

empty_error <- tryCatch(
  {
    statguide_console_prompt("")
    FALSE
  },
  error = function(e) grepl("Enter a question for AI", conditionMessage(e), fixed = TRUE)
)
check("empty AI question errors clearly", empty_error)

ollama_prompt <- statguide_console_prompt(
  "Explain Poisson lambda.",
  task = "concept",
  provider = "ollama"
)
check("console prompt can use ollama provider", grepl("Provider mode: Ollama local model", ollama_prompt, fixed = TRUE))

openai_blocked <- openai(
  "Explain a one-sample t interval.",
  model = "example-model",
  allow_external = FALSE
)
check("online helper blocks without explicit approval", identical(openai_blocked$status, "blocked_external_send"))

setup_text <- paste(capture.output(ai_setup_help()), collapse = "\n")
check("setup help says keys are environment variables", grepl("environment variables", setup_text, fixed = TRUE))
check("setup help says app does not store raw keys", grepl("does not store raw API keys", setup_text, fixed = TRUE))

ollama_settings <- statguide_ai_prepare_settings(
  provider = "ollama",
  model = "",
  endpoint = "",
  interactive_setup = FALSE
)
check("noninteractive ollama setup uses default endpoint",
      identical(ollama_settings$endpoint, ""))

online_settings <- statguide_ai_prepare_settings(
  provider = "openai",
  model = "",
  key_env = "",
  allow_external = FALSE,
  interactive_setup = FALSE
)
check("noninteractive online setup does not approve external send",
      identical(online_settings$allow_external, FALSE))

isolated_env <- new.env(parent = globalenv())
source(file.path("R", "console_ai.R"), local = isolated_env)
isolated_prompt <- isolated_env$statguide_console_prompt(
  "Provide R code for a one sample t confidence interval.",
  task = "generate_r"
)
check("console helper can load ai providers when sourced alone",
      grepl("one sample t confidence interval", isolated_prompt, fixed = TRUE))

groups <- statguide_console_calculator_groups()
check("console calculator groups created", length(groups) >= 5)
check("console calculator groups cover all choices",
      sum(lengths(groups)) == length(function_choices))

poisson_matches <- statguide_console_search_matches("poisson average rate calls per hour")
check("console search returns matches", nrow(poisson_matches) > 0)
check("console search finds poisson", any(poisson_matches$fn == "poissonDistribution"))

check("formula mapping works for poisson",
      identical(statguide_formula_topic_for_fn("poissonDistribution"), "s7_5"))

calculator_output <- paste(capture.output(
  statguide_console_show_calculator("poissonDistribution", "Poisson test")
), collapse = "\n")
check("console calculator detail prints use text",
      grepl("Function ID: poissonDistribution", calculator_output, fixed = TRUE))
check("console calculator detail prints related formula",
      grepl("Related formula topic:", calculator_output, fixed = TRUE))

source("run_console.R")
check("run console loads statguide_console", is.function(statguide_console))
check("run console loads codex", is.function(codex))

ai_env <- new.env(parent = globalenv())
source("run_ai.R", local = ai_env)
check("run ai loads codex", is.function(codex))
check("run ai does not require menu to be used", is.function(askcodex))
check("run ai loads ollama", is.function(ollama))
check("run ai loads generic askai", is.function(askai))

cat("Console checks passed\n")
