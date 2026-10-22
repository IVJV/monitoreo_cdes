# ==============================================================================
# HTML structure: unique IDs and working stage A01 cross-report anchors
# Run after rendering both technical and audit reports.
# ==============================================================================

if (!requireNamespace("xml2", quietly = TRUE))
  stop("The xml2 package is required to validate rendered HTML IDs.")

files <- c(
  technical = "output/reports/technical/reporting/reporte_tecnico_resultados.html",
  audit = "output/reports/audit/reporting/reporte_controles_internos.html"
)

for (report_name in names(files)) {
  html_file <- files[[report_name]]
  if (!file.exists(html_file))
    stop(paste("HTML report not found:", html_file))

  document <- xml2::read_html(html_file)
  elements <- xml2::xml_find_all(document, "//*[@id]")
  ids <- xml2::xml_attr(elements, "id")
  duplicates <- unique(ids[duplicated(ids)])

  if (length(duplicates) > 0L)
    stop(paste(report_name, "has duplicate HTML IDs:",
               paste(duplicates, collapse = ", ")))

  expected_anchor <- if (report_name == "technical")
    "etapa-A01" else "etapa-A01-controles-integridad"
  stopifnot(expected_anchor %in% ids)

  # A04 uses semantic markers for both explicit Total rows, not their position.
  if (report_name == "technical") {
    total_markers <- xml2::xml_find_all(
      document,
      paste0(
        '//*[@id="etapa-A04"]//span[',
        'contains(concat(" ", normalize-space(@class), " "),',
        ' " rap-total-row-marker ")]'
      )
    )
    stopifnot(
      length(total_markers) == 2L,
      all(xml2::xml_text(total_markers) == "Total")
    )
  }

  cat("PASS:", report_name, "HTML IDs unique; stage anchor present\n")
}

cat("PASS: technical/audit HTML structure\n")
