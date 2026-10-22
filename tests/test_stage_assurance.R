# ==============================================================================
# Stage-assurance component contract tests (no ENEMDU source file needed)
# ==============================================================================

source("R/reporting_stage_assurance.R")

make_stage <- function(passed) {
  list(
    stage_id = "A01_ingestion",
    validation_evidence = data.frame(
      stage_id = "A01_ingestion",
      check_id = "required_variables_present",
      passed = passed,
      stringsAsFactors = FALSE
    ),
    stage_passed = passed
  )
}

href <- "../../audit/reporting/reporte_controles_internos.html#etapa-A01-controles-integridad"

# Passing controls: green state with the exact stage-specific audit destination.
ok <- render_stage_assurance(make_stage(TRUE), href)
stopifnot(
  grepl('class="rap-stage-link"', ok, fixed = TRUE),
  grepl('class="rap-stage-assurance passed"', ok, fixed = TRUE),
  grepl("Controles superados", ok, fixed = TRUE),
  grepl(paste0('href="', href, '"'), ok, fixed = TRUE)
)

# Failed controls: unsuccessful state is never rendered as a success.
failed <- render_stage_assurance(make_stage(FALSE), href)
stopifnot(
  grepl('class="rap-stage-assurance failed"', failed, fixed = TRUE),
  grepl("Controles no superados", failed, fixed = TRUE),
  !grepl('class="rap-stage-assurance passed"', failed, fixed = TRUE)
)

# Controls without a dedicated audit section: status but no artificial link.
unlinked <- render_stage_assurance(make_stage(TRUE))
stopifnot(
  grepl("Controles superados", unlinked, fixed = TRUE),
  !grepl("<a ", unlinked, fixed = TRUE)
)

# Functional stages with no executed controls must not display a badge.
functional_stage <- list(
  stage_id = "A05_survey_design",
  validation_evidence = NULL,
  stage_passed = NULL
)
stopifnot(identical(render_stage_assurance(functional_stage), ""))

# Incomplete evidence must not silently be interpreted as a successful stage.
invalid_stage <- make_stage(TRUE)
invalid_stage$stage_passed <- NULL
err <- tryCatch(
  render_stage_assurance(invalid_stage),
  error = function(e) e
)
stopifnot(inherits(err, "error"))

cat("PASS: stage assurance (linked, unlinked, failed, absent, invalid)\n")

# Audit-to-technical link returns to the matching stage without repeating status.
technical_href <- "../../technical/reporting/reporte_tecnico_resultados.html#etapa-A01"
return_link <- render_stage_return_link(technical_href)
stopifnot(
  grepl('class="rap-stage-return-link"', return_link, fixed = TRUE),
  grepl(paste0('href="', technical_href, '"'), return_link, fixed = TRUE),
  grepl("← Ver resultados técnicos", return_link, fixed = TRUE),
  !grepl("Controles superados", return_link, fixed = TRUE)
)

# Escaping of URL attributes and rejection of missing or invalid destinations.
escaped_return <- render_stage_return_link('report.html?x=1&y="2"#etapa-A01')
stopifnot(grepl('x=1&amp;y=&quot;2&quot;', escaped_return, fixed = TRUE))
for (invalid_href in list(NULL, "", " ", NA_character_, character(0))) {
  invalid_result <- tryCatch(
    render_stage_return_link(invalid_href),
    error = function(e) e
  )
  stopifnot(inherits(invalid_result, "error"))
}

# The audit stage's reference and heading anchor must target the correct RT.
audit_qmd <- paste(
  readLines("reporting/audit/etapa_A01_RA.qmd", warn = FALSE),
  collapse = "\n"
)
stopifnot(
  grepl("render_stage_return_link", audit_qmd, fixed = TRUE),
  grepl(technical_href, audit_qmd, fixed = TRUE),
  grepl("{#etapa-A01 .rap-return-heading}", audit_qmd, fixed = TRUE)
)

cat("PASS: stage A01 audit-to-technical return link\n")
