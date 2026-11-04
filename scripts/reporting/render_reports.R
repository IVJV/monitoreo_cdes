# ==============================================================================
# Reporting runner
# ==============================================================================
# Transitional development runner used until pipeline orchestration is migrated
# to targets. It builds one reporting context and passes it to the requested
# report(s) without moving processing logic into the QMD files.


# ==============================================================================
# 1. Read command-line arguments
# ==============================================================================
reporting_main <- function() {
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1L || length(args) > 2L)
  stop(
    paste(
      "Uso:",
      "Rscript scripts/reporting/render_reports.R",
      "data/raw/ENEMDU_YYYY_MM.sav [technical|audit|both]"
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

if (!(report_set %in% c(names(report_config), "both")))
  stop(
    paste(
      "Tipo de reporte no disponible:",
      report_set,
      "— use technical, audit o both."
    )
  )

selected_reports <- if (identical(report_set, "both"))
  c("technical", "audit") else report_set


# ==============================================================================
# 2. Load current production and reporting-context dependencies
# ==============================================================================
source("R/validation_errors.R")
source("R/A01_ingesta_enemdu.R")
source("R/A02_identificadores_enemdu.R")
source("R/A03_tratamiento_ingpc.R")
source("R/A04_deciles_ingpc.R")
source("R/A05_diseno_muestral_enemdu.R")
source("R/A06_media_ingpc_por_decil_ajustada_por_diseno_muestral.R")
source("R/B01_validacion_exploracion_condactn.R")
source("R/B02_clasificacion_poblacion.R")
source("R/C01_recepcion_entrada.R")
source("R/C02_distribucion_poblacion_por_decil.R")
source("R/C03_concentracion_ingreso_por_decil.R")
source("R/C04_verificacion_tablas.R")
source("R/control_catalog.R")
source("R/report_context.R")


# ==============================================================================
# 3. Build one reusable reporting context
# ==============================================================================
report_context <- build_report_context(input_file)

c04_rejected <- !isTRUE(report_context$stage_C04$stage_passed) ||
  !isTRUE(report_context$stage_C04$verification_approved)

if (c04_rejected) {
  # Render the rejection in RA, but never refresh RT or approved outputs.
  # A non-zero final exit status prevents the QA gate from reporting success.
  cat("C04 NO APROBADO: no se generan publicaciones ni RT.\n")
  selected_reports <- "audit"
}

# Publication is separate from Quarto and only runs when the technical
# report is requested (alone or together with the audit report).
# Release occurs after C04 approval; the QMD merely displays and links outputs.
if (!c04_rejected && "technical" %in% selected_reports) {
  source("R/reporting_helpers.R")
  source("R/reporting_publication_format.R")
  source("R/C04_publicacion_resultados.R")

  stage_c_publication <- publish_stage_c_results(report_context)
  report_context$stage_C04$publication <- stage_c_publication$files
}

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

for (selected_set in selected_reports) {
  selected_report <- report_config[[selected_set]]

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
        "Quarto finalizó con código de salida para",
        selected_set, ":", render_status
      )
    )

  # The technical report no longer embeds the final published tables or download
  # links. Verify the four approved publication files on disk instead.
  if (identical(selected_set, "technical")) {
    expected_keys <- c("table1_html", "table1_xlsx", "table2_html", "table2_xlsx")
    published_files <- stage_c_publication$files[expected_keys]
    file_names <- unlist(published_files, use.names = FALSE)

    if (length(file_names) != 4L ||
        !is.character(file_names) || anyNA(file_names) ||
        any(!nzchar(file_names)) || anyDuplicated(file_names))
      stop("La publicación C04 no identifica los cuatro productos esperados.")

    published_paths <- file.path(stage_c_publication$directory, file_names)
    if (!dir.exists(stage_c_publication$directory) ||
        !all(file.exists(published_paths)) ||
        any(file.info(published_paths)$size <= 0))
      stop("Falta uno o más archivos de la publicación aprobada del Bloque C.")

    cat("PUBLICACIÓN BLOQUE C:", stage_c_publication$directory, "\n")
    cat("ARCHIVOS PUBLICADOS: dos HTML y dos XLSX comprobados", "\n")
  }
}

if (c04_rejected)
  stop("C04 no aprobado. Se generó el RA con el diagnóstico; publicación bloqueada.")
}

reporting_main()
