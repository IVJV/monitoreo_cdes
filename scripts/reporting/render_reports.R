# ==============================================================================
# Reporting runner
# ==============================================================================
# Transitional development runner used until pipeline orchestration is migrated
# to targets. It builds one reporting context and passes it to the requested
# report without moving processing logic into the QMD files.


# ==============================================================================
# 1. Read command-line arguments
# ==============================================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1L || length(args) > 2L)
  stop(
    paste(
      "Uso:",
      "Rscript scripts/reporting/render_reports.R",
      "data/raw/ENEMDU_YYYY_MM.sav [technical|audit]"
    )
  )

input_file <- args[[1]]
report_set <- if (length(args) == 2L) args[[2]] else "technical"

report_config <- list(
  technical = list(
    input = "reporting/reporte_tecnico_resultados.qmd",
    profile = "technical"
  ),
  audit = list(
    input = "reporting/reporte_controles_internos.qmd",
    profile = "audit"
  )
)

if (!(report_set %in% names(report_config)))
  stop(
    paste(
      "Tipo de reporte no disponible:",
      report_set,
      "— use technical o audit."
    )
  )

selected_report <- report_config[[report_set]]


# ==============================================================================
# 2. Load current production and reporting-context dependencies
# ==============================================================================
source("R/validation_errors.R")
source("R/01A_ingesta_enemdu.R")
source("R/02A_identificadores_enemdu.R")
source("R/03A_tratamiento_ingpc.R")
source("R/04A_deciles_ingpc.R")
source("R/05A_diseno_muestral_enemdu.R")
source("R/06A_media_ingpc_por_decil_ajustada_por_diseno_muestral.R")
source("R/control_catalog.R")
source("R/report_context.R")


# ==============================================================================
# 3. Build one reusable reporting context
# ==============================================================================
report_context <- build_report_context(input_file)

context_file <- tempfile(
  pattern = "report_context_",
  fileext = ".rds"
)

saveRDS(
  report_context,
  context_file
)

on.exit(
  unlink(context_file),
  add = TRUE
)


# ==============================================================================
# 4. Prepare transient Quarto parameters
# ==============================================================================
params_file <- tempfile(
  pattern = "report_params_",
  fileext = ".yml"
)

on.exit(
  unlink(params_file),
  add = TRUE
)

context_file_quarto <- normalizePath(
  context_file,
  winslash = "/",
  mustWork = TRUE
)

writeLines(
  c(
    paste0(
      'context_file: "',
      context_file_quarto,
      '"'
    ),
    paste0(
      'review_date: "',
      format(Sys.Date(), "%d/%m/%Y"),
      '"'
    )
  ),
  params_file
)


# ==============================================================================
# 5. Render requested report
# ==============================================================================
quarto_bin <- Sys.which("quarto")

if (!nzchar(quarto_bin))
  stop("No se encontró Quarto en PATH.")

render_status <- system2(
  quarto_bin,
  args = c(
    "render",
    selected_report$input,
    "--profile",
    selected_report$profile,
    "--execute-params",
    params_file
  )
)

if (!identical(render_status, 0L))
  stop(
    paste(
      "Quarto finalizó con código de salida:",
      render_status
    )
  )
