# ==============================================================================
# Stage 04C — Controlled release of the two final Stage C tables
# ==============================================================================
# Owns the release gate, filenames and file I/O. Statistical values are reused
# unchanged from the reporting context. Appearance is defined exclusively in
# R/reporting_publication_format.R and R/reporting_helpers.R.

publish_stage_c_results <- function(context,
                                    output_root = "output/publications") {

  if (!is.list(context) || !is.list(context$stage_01c) ||
      !is.list(context$stage_02c) || !is.list(context$stage_03c) ||
      !is.list(context$stage_04c))
    stop("Faltan etapas requeridas para la publicación del Bloque C.")

  approved <- context$stage_04c
  evidence <- approved$validation_evidence
  expected_checks <- c(
    "table1_total_consistency", "table2_total_consistency",
    "table1_decile_consistency", "table2_decile_consistency"
  )
  if (!isTRUE(approved$verification_approved) ||
      !isTRUE(approved$stage_passed) ||
      !isTRUE(approved$total_discrepancies == 0L) ||
      !is.data.frame(evidence) || nrow(evidence) != 4L ||
      !all(c("stage_id", "check_id", "passed") %in% names(evidence)) ||
      !identical(sort(evidence$check_id), sort(expected_checks)) ||
      !all(evidence$stage_id == approved$stage_id) ||
      !is.logical(evidence$passed) || anyNA(evidence$passed) ||
      !all(evidence$passed))
    stop("Publicación no autorizada: la Etapa 04C no ha superado sus controles.")

  period <- context$stage_01c$period_file
  period_title <- context$stage_01c$period_title
  if (!is.character(period) || length(period) != 1L ||
      is.na(period) || !grepl("^[0-9]{4}_[0-9]{2}$", period) ||
      !is.character(period_title) || length(period_title) != 1L ||
      is.na(period_title) || !nzchar(period_title))
    stop("Metadatos del período incompletos para la publicación.")

  publication_files <- list(
    table1_html = paste0("Tabla_1_Distribucion_ingpc_", period, ".html"),
    table1_xlsx = paste0("Tabla_1_Distribucion_ingpc_", period, ".xlsx"),
    table2_html = paste0("Tabla_2_Concentracion_ingpc_", period, ".html"),
    table2_xlsx = paste0("Tabla_2_Concentracion_ingpc_", period, ".xlsx")
  )

  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile(paste0(".stage_04c_", period, "_"), tmpdir = output_root)
  if (!dir.create(staging))
    stop("No se pudo crear la carpeta temporal de publicación.")
  on.exit(if (dir.exists(staging)) unlink(staging, recursive = TRUE), add = TRUE)

  for (table_number in c(1L, 2L)) {
    data <- if (table_number == 1L)
      context$stage_02c$population_distribution_table_num else
      context$stage_03c$income_concentration_table_num

    html <- build_stage_c_publication_html(
      data, approved, table_number, period_title
    )
    html_path <- file.path(staging, publication_files[[paste0("table", table_number, "_html")]])
    gt::gtsave(html, filename = html_path)
    merge_stage_c_published_html(html_path)

    workbook <- build_stage_c_publication_excel(data, table_number)
    excel_path <- file.path(staging, publication_files[[paste0("table", table_number, "_xlsx")]])
    openxlsx::saveWorkbook(workbook, excel_path, overwrite = TRUE)
  }

  staged_paths <- file.path(staging, unlist(publication_files, use.names = FALSE))
  if (!all(file.exists(staged_paths)) || any(file.info(staged_paths)$size <= 0))
    stop("La publicación no generó los cuatro productos esperados.")

  final_dir <- file.path(output_root, period)
  backup <- NULL
  if (dir.exists(final_dir)) {
    backup <- tempfile(paste0(".previous_", period, "_"), tmpdir = output_root)
    if (!file.rename(final_dir, backup))
      stop("No se pudo preparar el reemplazo de la publicación anterior.")
  }
  if (!file.rename(staging, final_dir)) {
    if (!is.null(backup))
      file.rename(backup, final_dir)
    stop("No se pudo finalizar la publicación validada.")
  }
  if (!is.null(backup)) unlink(backup, recursive = TRUE)

  list(
    period_file = period,
    directory = final_dir,
    files = publication_files
  )
}

# Verify that the rendered technical report links directly to the approved
# publication files. No workbooks are copied or installed under reports/.
verify_stage_c_publication_links <- function(publication,
    report_directory = "output/reports/technical/reporting") {
  if (!is.list(publication) ||
      !is.character(publication$directory) ||
      length(publication$directory) != 1L ||
      is.na(publication$directory) ||
      !dir.exists(publication$directory))
    stop("No existe la publicación validada del Bloque C.")

  filenames <- unlist(
    publication$files[c("table1_xlsx", "table2_xlsx")],
    use.names = FALSE
  )
  if (!is.character(filenames) || length(filenames) != 2L ||
      anyNA(filenames) || anyDuplicated(filenames))
    stop("Los dos archivos Excel publicados no están identificados.")

  report_html <- file.path(report_directory, "reporte_tecnico_resultados.html")
  if (!file.exists(report_html))
    stop("No se encontró el HTML técnico en la ruta de salida esperada.")

  published_paths <- file.path(publication$directory, filenames)
  if (!all(file.exists(published_paths)) ||
      any(file.info(published_paths)$size <= 0))
    stop("Falta uno o más Excel en la publicación validada del Bloque C.")

  html_doc <- xml2::read_html(report_html)
  links <- xml2::xml_find_all(
    html_doc, "//a[contains(concat(' ', normalize-space(@class), ' '), ' rap-excel-download ')]"
  )
  hrefs <- xml2::xml_attr(links, "href")
  download_names <- xml2::xml_attr(links, "download")
  if (length(hrefs) != 2L || anyNA(hrefs) ||
      anyNA(download_names) ||
      !setequal(download_names, filenames) ||
      anyDuplicated(download_names))
    stop("El RT no contiene los dos enlaces Excel esperados.")

  for (i in seq_along(hrefs)) {
    # Only local, relative paths are valid: never accept a detached URL.
    if (grepl("^(/|[A-Za-z][A-Za-z0-9+.-]*:)", hrefs[[i]]))
      stop("El RT debe enlazar archivos Excel locales mediante rutas relativas.")
    target <- normalizePath(
      file.path(report_directory, hrefs[[i]]),
      winslash = "/", mustWork = TRUE
    )
    expected <- normalizePath(
      file.path(publication$directory, download_names[[i]]),
      winslash = "/", mustWork = TRUE
    )
    if (!identical(target, expected))
      stop(paste("Enlace Excel fuera de la publicación validada:",
                 download_names[[i]]))
  }

  invisible(TRUE)
}
