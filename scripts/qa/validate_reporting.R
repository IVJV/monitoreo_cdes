# ==============================================================================
# Integrated reporting quality gate
# ==============================================================================
# Run from the repository root:
#   Rscript scripts/qa/validate_reporting.R
#   Rscript scripts/qa/validate_reporting.R data/raw/ENEMDU_YYYY_MM.sav
#
# Fail-fast order: prerequisites -> unit/contract tests -> both Quarto renders ->
# rendered HTML structure -> live cross-report links.
# All report generation continues to use the existing production runner.

qa_main <- function() {

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1L)
    stop("Usage: Rscript scripts/qa/validate_reporting.R [ENEMDU_SAV_PATH]")

  # The project uses relative paths and Quarto execute-dir: project.
  required_root_files <- c(
    "_quarto.yml",
    "scripts/reporting/render_reports.R",
    "R/report_context.R"
  )
  missing_root_files <- required_root_files[!file.exists(required_root_files)]
  if (length(missing_root_files) > 0L)
    stop(paste(
      "Run this command from the RAP project root; missing:",
      paste(missing_root_files, collapse = ", ")
    ))

  input_file <- if (length(args) == 1L) args[[1L]] else
    "data/raw/ENEMDU_2008_12.sav"
  if (!file.exists(input_file))
    stop(paste("ENEMDU input file not found:", input_file))
  if (dir.exists(input_file))
    stop(paste("ENEMDU input path is a directory:", input_file))

  # Resolve an executable from the active R installation; avoid assuming that
  # the shell PATH contains a second, different version of R.
  rscript_bin <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  if (!file.exists(rscript_bin))
    rscript_bin <- Sys.which("Rscript")
  if (!nzchar(rscript_bin) || !file.exists(rscript_bin))
    stop("Unable to locate Rscript in the active R installation.")

  # Explicit test registry prevents silently omitting newly added tests.
  unit_tests <- c(
    "tests/test_p1a_contract.R",
    "tests/test_p2a_contract.R",
    "tests/test_p3a_contract.R",
    "tests/test_p4a_contract.R",
    "tests/test_stage_assurance.R",
    "tests/test_report_table_ids.R"
  )
  integration_tests <- c(
    "tests/test_report_html_ids.R",
    "tests/test_report_crosslinks.R"
  )
  registered_tests <- c(unit_tests, integration_tests)

  missing_tests <- registered_tests[!file.exists(registered_tests)]
  if (length(missing_tests) > 0L)
    stop(paste("Missing required QA tests:",
               paste(missing_tests, collapse = ", ")))

  discovered_tests <- list.files(
    "tests", pattern = "^test_.*\\.R$", full.names = TRUE
  )
  unregistered_tests <- setdiff(discovered_tests, registered_tests)
  if (length(unregistered_tests) > 0L)
    stop(paste(
      "Unregistered tests found. Assign them to the QA gate before proceeding:",
      paste(unregistered_tests, collapse = ", ")
    ))

  # Quarto is a hard prerequisite, not an optional reporting dependency.
  if (!nzchar(Sys.which("quarto")))
    stop("Quarto was not found on PATH.")

  shell_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"

  run_rscript <- function(label, script, arguments = character()) {
    if (!file.exists(script))
      stop(paste("Required script not found:", script))

    cat("\n===", label, "===\n")
    flush.console()

    # system2() receives individual arguments; quote values that may contain
    # whitespace, especially a Windows data path.
    command_args <- c(
      shQuote(script, type = shell_type),
      if (length(arguments) > 0L)
        vapply(arguments, shQuote, character(1), type = shell_type)
    )

    status <- system2(
      command = rscript_bin,
      args = command_args,
      stdout = "",
      stderr = ""
    )

    if (length(status) != 1L || is.na(status) || status != 0L)
      stop(paste("QA step failed:", label, "(exit code:", status, ")"))

    cat("PASS:", label, "\n")
    invisible(NULL)
  }

  cat("RAP REPORTING QA — START\n")
  cat("Source:", normalizePath(input_file, winslash = "/"), "\n")

  for (test in unit_tests)
    run_rscript(paste("Contract test:", test), test)

  # Both reports must be regenerated successfully BEFORE any HTML test runs.
  for (report_set in c("technical", "audit"))
    run_rscript(
      paste("Render:", report_set),
      "scripts/reporting/render_reports.R",
      c(input_file, report_set)
    )

  for (test in integration_tests)
    run_rscript(paste("Rendered HTML test:", test), test)

  cat("\nPASS: RAP REPORTING QA — ALL CHECKS COMPLETED\n")
  invisible(TRUE)
}

tryCatch(
  qa_main(),
  error = function(err) {
    message("\nFAIL: RAP REPORTING QA — ", conditionMessage(err))
    quit(save = "no", status = 1L, runLast = FALSE)
  }
)
