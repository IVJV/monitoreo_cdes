# ==============================================================================
# Rendered cross-report navigation: link destinations and target anchors
# ==============================================================================
# Run from the repository root, after rendering both reports.
# Links are validated using their actual href values, not hard-coded URL copies.
# Stage-specific contracts prevent a valid link from pointing to the wrong stage.

if (!requireNamespace("xml2", quietly = TRUE))
  stop("The xml2 package is required to validate report navigation.")

reports <- c(
  technical = "output/reports/technical/reporting/reporte_tecnico_resultados.html",
  audit = "output/reports/audit/reporting/reporte_controles_internos.html"
)

# Explicit contracts for stages whose bidirectional navigation is implemented.
# Names identify the source stage; values identify its required destination anchor.
# This is not a hard-coded copy of href paths: paths are read from rendered links.
required_stage_targets <- list(
  technical = c(
    "etapa-01a" = "etapa-01a-controles-integridad",
    "etapa-02a" = "etapa-02a-controles-identificadores",
    "etapa-03a" = "etapa-03a-construccion-consistencia-universo-analitico-ingpc",
    "etapa-04a" = "etapa-04a-controles-deciles"
  ),
  audit = c(
    "etapa-01a" = "etapa-01a",
    "etapa-02a" = "etapa-02a",
    "etapa-03a" = "etapa-03a",
    "etapa-04a" = "etapa-04a"
  )
)

if (!all(file.exists(reports)))
  stop(paste("Report HTML files missing:",
             paste(reports[!file.exists(reports)], collapse = ", ")))

# A link must reside in a unique top-level stage section and return to that
# same stage in the destination document, not merely to an existing anchor.
stage_xpath <- paste0(
  'ancestor-or-self::section[',
  'contains(concat(" ", normalize-space(@class), " "), " level1 ")',
  '][1]'
)

for (report_name in names(reports)) {
  source_file <- normalizePath(reports[[report_name]], winslash = "/", mustWork = TRUE)
  target_report_name <- if (report_name == "technical") "audit" else "technical"
  expected_target_file <- normalizePath(
    reports[[target_report_name]], winslash = "/", mustWork = TRUE
  )

  document <- xml2::read_html(source_file)
  target_document <- xml2::read_html(expected_target_file)
  target_ids <- xml2::xml_attr(
    xml2::xml_find_all(target_document, "//*[@id]"), "id"
  )
  required_targets <- required_stage_targets[[report_name]]

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

  observed_stages <- character()

  for (link in links) {
    source_stage <- xml2::xml_find_first(link, stage_xpath)
    source_stage_id <- xml2::xml_attr(source_stage, "id")
    if (is.na(source_stage_id) || !nzchar(source_stage_id))
      stop(paste("Cross-report link outside a named stage in", report_name))

    if (source_stage_id %in% observed_stages)
      stop(paste("Multiple cross-report links assigned to", source_stage_id,
                 "in", report_name))
    observed_stages <- c(observed_stages, source_stage_id)

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

    target_anchor <- parts[[2L]]
    if (!(target_anchor %in% target_ids))
      stop(paste("Cross-report destination anchor does not exist:", href))

    # A real anchor in the wrong stage is still an invalid cross-report link.
    target_node <- xml2::xml_find_first(
      target_document,
      paste0('//*[@id="', target_anchor, '"]')
    )
    destination_stage <- xml2::xml_find_first(target_node, stage_xpath)
    destination_stage_id <- xml2::xml_attr(destination_stage, "id")
    if (is.na(destination_stage_id) ||
        !identical(destination_stage_id, source_stage_id))
      stop(paste("Cross-report link targets a different stage:",
                 source_stage_id, "->", href))

    # 01A, 02A, 03A and 04A have exact, distinct destinations; no silent cross-wiring.
    if (source_stage_id %in% names(required_targets) &&
        !identical(target_anchor, unname(required_targets[[source_stage_id]])))
      stop(paste("Incorrect destination anchor for", source_stage_id,
                 "in", report_name, ":", target_anchor))
  }

  # A generic link count is insufficient: require exactly one link per stage.
  missing_stages <- setdiff(names(required_targets), observed_stages)
  if (length(missing_stages) > 0L)
    stop(paste("Required stage navigation missing from", report_name, ":",
               paste(missing_stages, collapse = ", ")))

  cat("PASS:", report_name, "->", target_report_name,
      "links and target anchors (", length(links), ")\n", sep = "")
  cat("PASS:", report_name, "stage-specific destinations:",
      paste(names(required_targets), collapse = ", "), "\n")
}

cat("PASS: bidirectional report navigation\n")
