# ==============================================================================
# Unique GT column IDs: unit contract (no ENEMDU data required)
# ==============================================================================

source("R/reporting_helpers.R")

make_diagnostics <- function() {
  data.frame(
    Control = c("Estructura", "Factor de expansión"),
    `Verificaciones incluidas` = c("Variables presentes", "Factor positivo"),
    Resultado = c("OK", "OK"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

original <- make_diagnostics()
stages <- c("01a_ingestion", "02a_identifiers", "03a_ingpc_treatment")
html_tables <- vapply(stages, function(stage_id) {
  rendered <- render_audit_diagnostics(original, stage_id)
  as.character(gt::as_raw_html(rendered))
}, character(1))

# Column identifiers must be stage-specific even when visible labels are shared.
for (i in seq_along(stages)) {
  prefix <- paste0("audit_", stages[i], "_")
  stopifnot(
    grepl(paste0('id="', prefix, 'control"'), html_tables[i], fixed = TRUE),
    grepl(paste0('id="', prefix, 'checks"'), html_tables[i], fixed = TRUE),
    grepl(paste0('id="', prefix, 'result"'), html_tables[i], fixed = TRUE),
    grepl(">Verificaciones incluidas<", html_tables[i], fixed = TRUE)
  )
}
stopifnot(identical(original, make_diagnostics()))

bad <- tryCatch(
  render_audit_diagnostics(original, "bad id"),
  error = function(e) e
)
stopifnot(inherits(bad, "error"))

cat("PASS: unique GT audit header IDs (01A, 02A, 03A)\n")
