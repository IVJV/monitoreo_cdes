# ==============================================================================
# Rendered cross-report navigation: link destinations and target anchors
# ==============================================================================
# Run from the repository root, after rendering both reports.
# Links are validated using their actual href values, not hard-coded URL copies.

if (!requireNamespace("xml2", quietly = TRUE))
  stop("The xml2 package is required to validate report navigation.")

reports <- c(
  technical = "output/reports/technical/reporting/reporte_tecnico_resultados.html",
  audit = "output/reports/audit/reporting/reporte_controles_internos.html"
)

if (!all(file.exists(reports)))
  stop(paste("Report HTML files missing:",
             paste(reports[!file.exists(reports)], collapse = ", ")))

for (report_name in names(reports)) {
  source_file <- normalizePath(reports[[report_name]], winslash = "/", mustWork = TRUE)
  target_report_name <- if (report_name == "technical") "audit" else "technical"
  expected_target_file <- normalizePath(
    reports[[target_report_name]], winslash = "/", mustWork = TRUE
  )

  document <- xml2::read_html(source_file)
  css_class <- if (report_name == "technical")
    "rap-stage-link" else "rap-stage-return-link"

  # Use the same shared navigation classes as the reporting component.
  selector <- paste0(
    '//a[contains(concat(" ", normalize-space(@class), " "), " ',
    css_class, ' ")]'
  )
  links <- xml2::xml_find_all(document, selector)

  if (length(links) == 0L)
    stop(paste("No", css_class, "links found in", report_name, "HTML."))

  for (link in links) {
    href <- xml2::xml_attr(link, "href")
    if (is.na(href) || !grepl("^[^#]+#[^#]+$", href))
      stop(paste("Malformed cross-report link in", report_name, ":", href))

    parts <- strsplit(href, "#", fixed = TRUE)[[1L]]
    linked_file <- file.path(dirname(source_file), parts[[1L]])
    if (!file.exists(linked_file))
      stop(paste("Cross-report destination does not exist:", linked_file))

    linked_file <- normalizePath(linked_file, winslash = "/", mustWork = TRUE)
    if (!identical(linked_file, expected_target_file))
      stop(paste("Cross-report link points to the wrong report:", href))

    linked_document <- xml2::read_html(linked_file)
    ids <- xml2::xml_attr(
      xml2::xml_find_all(linked_document, "//*[@id]"), "id"
    )
    if (!(parts[[2L]] %in% ids))
      stop(paste("Cross-report destination anchor does not exist:", href))
  }

  cat("PASS:", report_name, "->", target_report_name,
      "links and target anchors (", length(links), ")\n", sep = "")
}

cat("PASS: bidirectional report navigation\n")
