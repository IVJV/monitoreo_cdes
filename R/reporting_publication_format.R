# ==============================================================================
# Publication formatting for validated Stage C tables (HTML / Excel)
# ==============================================================================
# Display-only transformations and workbook styling. Analytical calculations,
# independent verification and the publication gate are defined elsewhere.

# Single source of presentation labels used by standalone HTML and XLSX.
stage_c_publication_labels <- function(table_number) {
  if (!(table_number %in% c(1L, 2L)))
    stop("Número de tabla no disponible.")

  if (table_number == 1L) {
    list(
      title = "DISTRIBUCIÓN DE LA POBLACIÓN POR DECILES DE INGRESO PER CÁPITA",
      subtitle = "Población y porcentaje de la población"
    )
  } else {
    list(
      title = "CONCENTRACIÓN DEL INGRESO PER CÁPITA POR DECILES",
      subtitle = "Ingreso per cápita (millones de USD) y participación (%)"
    )
  }
}

build_stage_c_publication_html <- function(data, stage_C04, table_number,
                                           period_title) {
  if (!(table_number %in% c(1L, 2L)))
    stop("Número de tabla no disponible.")

  table_gt <- if (table_number == 1L) {
    render_stage_c_decile_matrix(
      data, "Population", "Percentage", value_digits = 0L,
      stage_prefix = "s02c", stage_C04 = stage_C04
    )
  } else {
    render_stage_c_decile_matrix(
      data, "Income_Millions", "Participation", value_digits = 2L,
      stage_prefix = "s03c", stage_C04 = stage_C04
    )
  }

  labels <- stage_c_publication_labels(table_number)

  table_gt |>
    gt::tab_header(
      title = gt::md(paste0("**", labels$title, " — ", period_title, "**")),
      subtitle = gt::md(paste0("*", labels$subtitle, "*"))
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(columns = tidyselect::everything())
    )
}

# Numeric workbook view equivalent to historical Sections C02.4 and C03.4.
# Percentage/participation cells are Excel fractions; no numeric value is
# rounded or converted into text before writing the workbook.
build_stage_c_publication_excel <- function(data, table_number) {
  if (!(table_number %in% c(1L, 2L)))
    stop("Número de tabla no disponible.")

  value_column <- if (table_number == 1L) "Population" else "Income_Millions"
  percentage_column <- if (table_number == 1L) "Percentage" else "Participation"
  required <- c("Periodo", "Variable", "Indicator", "Decile",
                value_column, percentage_column)
  if (!is.data.frame(data) || !all(required %in% names(data)) ||
      nrow(data) != 176L || anyNA(data$Periodo) ||
      length(unique(data$Periodo)) != 1L ||
      anyDuplicated(paste(data$Variable, data$Decile, sep = "|")))
    stop("La tabla numérica de publicación no tiene la estructura esperada.")

  view <- data |>
    dplyr::select(tidyselect::all_of(required)) |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(c(value_column, percentage_column)),
      names_to = ".measure", values_to = ".value"
    ) |>
    dplyr::mutate(
      .measure = factor(.measure, levels = c(value_column, percentage_column)),
      .value = dplyr::if_else(.measure == percentage_column,
                              .value / 100, .value),
      Indicator = dplyr::if_else(.measure == value_column, Indicator, "")
    ) |>
    dplyr::select(Periodo, Variable, .measure, Indicator, Decile, .value) |>
    tidyr::pivot_wider(names_from = Decile, values_from = .value) |>
    dplyr::arrange(Variable, .measure)

  deciles <- c("Total", paste("Decil", seq_len(10L)))
  if (nrow(view) != 32L || !all(deciles %in% names(view)) ||
      !identical(as.character(view$.measure[seq(1L, 32L, by = 2L)]),
                 rep(value_column, 16L)))
    stop("La publicación requiere 16 pares de filas por indicador.")

  first_period <- as.character(view$Periodo[[1L]])
  numeric_values <- view |>
    dplyr::select(tidyselect::all_of(deciles))
  excel_data <- data.frame(
    Periodo = c(first_period, rep("", 31L)),
    Indicador = as.character(view$Indicator),
    numeric_values,
    check.names = FALSE, stringsAsFactors = FALSE
  )

  wb <- openxlsx::createWorkbook()
  sheet <- paste("Tabla", table_number)
  openxlsx::addWorksheet(wb, sheet)

  labels <- stage_c_publication_labels(table_number)

  openxlsx::writeData(wb, sheet, labels$title, startRow = 1L, startCol = 1L)
  openxlsx::writeData(wb, sheet, labels$subtitle, startRow = 2L, startCol = 1L)
  openxlsx::mergeCells(wb, sheet, cols = 1:13, rows = 1L)
  openxlsx::mergeCells(wb, sheet, cols = 1:13, rows = 2L)
  openxlsx::writeData(wb, sheet, excel_data, startRow = 4L, startCol = 1L)
  openxlsx::mergeCells(wb, sheet, cols = 1L, rows = 5:36)
  first_rows <- seq(5L, 35L, by = 2L)
  for (i in first_rows)
    openxlsx::mergeCells(wb, sheet, cols = 2L, rows = i:(i + 1L))

  title_style <- openxlsx::createStyle(
    fontSize = 14, textDecoration = "bold", halign = "center", valign = "center"
  )
  subtitle_style <- openxlsx::createStyle(
    fontSize = 11, textDecoration = "italic", halign = "center", valign = "center"
  )
  header_style <- openxlsx::createStyle(
    textDecoration = "bold", halign = "center", valign = "center", border = "Bottom"
  )
  period_style <- openxlsx::createStyle(halign = "center", valign = "center")
  indicator_style <- openxlsx::createStyle(
    halign = "left", valign = "center", wrapText = TRUE
  )
  numeric_style <- openxlsx::createStyle(
    numFmt = if (table_number == 1L) "#,##0" else "#,##0.00"
  )
  percentage_style <- openxlsx::createStyle(numFmt = "0.0%")

  openxlsx::addStyle(wb, sheet, title_style, rows = 1L, cols = 1:13,
                     gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, subtitle_style, rows = 2L, cols = 1:13,
                     gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, header_style, rows = 4L, cols = 1:13,
                     gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, period_style, rows = 5:36, cols = 1L,
                     gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, indicator_style, rows = 5:36, cols = 2L,
                     gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, numeric_style, rows = first_rows,
                     cols = 3:13, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, percentage_style, rows = first_rows + 1L,
                     cols = 3:13, gridExpand = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = 1L, widths = 12)
  openxlsx::setColWidths(wb, sheet, cols = 2L, widths = 42)
  openxlsx::setColWidths(wb, sheet, cols = 3:13, widths = 12)

  wb
}

# Preserve the historical presentation of standalone HTML tables: merge each
# indicator cell vertically across its absolute-value / percentage pair.
merge_stage_c_published_html <- function(path) {
  html_doc <- xml2::read_html(path)
  rows <- xml2::xml_find_all(
    html_doc, "//tbody[contains(@class, 'gt_table_body')]/tr"
  )
  if (length(rows) != 32L)
    stop("El HTML independiente requiere 32 filas de resultados.")

  for (i in seq(1L, 31L, by = 2L)) {
    indicator_cell <- xml2::xml_find_first(rows[[i]], "./td[1]")
    empty_cell <- xml2::xml_find_first(rows[[i + 1L]], "./td[1]")
    if (inherits(indicator_cell, "xml_missing") ||
        inherits(empty_cell, "xml_missing"))
      stop("La estructura HTML no permite fusionar el indicador.")

    xml2::xml_set_attr(indicator_cell, "rowspan", "2")
    style <- xml2::xml_attr(indicator_cell, "style")
    if (is.na(style)) style <- ""
    xml2::xml_set_attr(indicator_cell, "style",
                       paste0(style, "; vertical-align: middle;"))
    xml2::xml_remove(empty_cell)
  }
  xml2::write_html(html_doc, path)
  invisible(path)
}


# ==============================================================================
# Stage C04 — Repair and verify the OOXML package of generated XLSX workbooks
# ==============================================================================
# Some versions of openxlsx emit worksheet relationships to drawing1.xml and
# vmlDrawing1.vml even when no drawings exist. Such relationship targets are
# missing from the archive, and an orphan drawing Override may remain in
# [Content_Types].xml. Excel can report that the workbook needs repair.
# This routine removes ONLY unused references to missing drawing parts.
# Referenced or unrelated missing parts cause an error (fail closed).
# It never modifies worksheet values, formatting, formulas or merges.

stage_c_xlsx_package_check <- function(path, repair = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !file.exists(path) || file.info(path)$size <= 0L)
    stop("No existe un libro XLSX válido para verificar.")

  if (!requireNamespace("xml2", quietly = TRUE))
    stop("Falta la dependencia xml2 para verificar el paquete XLSX.")
  if (repair && !requireNamespace("zip", quietly = TRUE))
    stop("Falta la dependencia zip para reparar el paquete XLSX.")

  temp_dir <- tempfile("c04_xlsx_package_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = temp_dir)
  content_types <- file.path(temp_dir, "[Content_Types].xml")
  if (!file.exists(content_types))
    stop("XLSX incompleto: no contiene [Content_Types].xml.")

  changed <- FALSE
  rel_dir <- file.path(temp_dir, "xl", "worksheets", "_rels")
  rel_files <- if (dir.exists(rel_dir))
    list.files(rel_dir, pattern = "[.]xml[.]rels$", full.names = TRUE) else
    character()

  for (rel_path in rel_files) {
    sheet_path <- file.path(dirname(dirname(rel_path)),
                            sub("[.]rels$", "", basename(rel_path)))
    if (!file.exists(sheet_path))
      stop("XLSX incompleto: falta la hoja que declara relaciones.")
    sheet_xml <- paste(readLines(sheet_path, warn = FALSE), collapse = "")
    relationships <- xml2::read_xml(rel_path)
    elements <- xml2::xml_find_all(
      relationships, "//*[local-name()='Relationship']"
    )
    for (element in elements) {
      target <- xml2::xml_attr(element, "Target")
      kind <- xml2::xml_attr(element, "Type")
      rel_id <- xml2::xml_attr(element, "Id")
      target_mode <- xml2::xml_attr(element, "TargetMode")
      if (anyNA(c(target, kind, rel_id)) || !nzchar(target))
        stop("XLSX inválido: relación incompleta.")
      if (!is.na(target_mode) && identical(target_mode, "External"))
        next
      resolved <- if (startsWith(target, "/")) {
        file.path(temp_dir, substring(target, 2L))
      } else {
        file.path(dirname(sheet_path), target)
      }
      if (file.exists(resolved)) next

      referenced <- grepl(paste0('r:id="', rel_id, '"'),
                          sheet_xml, fixed = TRUE)
      unused_drawing <- grepl("/(drawing|vmlDrawing)$", kind) && !referenced
      if (!unused_drawing)
        stop(paste("XLSX inválido: componente ausente o en uso:", target))
      if (!repair)
        stop(paste("XLSX contiene una relación de dibujo inexistente:", target))
      xml2::xml_remove(element)
      changed <- TRUE
    }
    if (repair && changed)
      xml2::write_xml(relationships, rel_path)
  }

  types_xml <- xml2::read_xml(content_types)
  overrides <- xml2::xml_find_all(
    types_xml, "//*[local-name()='Override']"
  )
  for (entry in overrides) {
    part <- xml2::xml_attr(entry, "PartName")
    if (is.na(part) || !startsWith(part, "/"))
      stop("XLSX inválido: Override sin ruta absoluta.")
    if (file.exists(file.path(temp_dir, substring(part, 2L)))) next
    unused_drawing <- grepl("^/xl/drawings/drawing[0-9]+[.]xml$", part)
    if (!unused_drawing)
      stop(paste("XLSX inválido: parte declarada e inexistente:", part))
    if (!repair)
      stop(paste("XLSX declara un dibujo inexistente:", part))
    xml2::xml_remove(entry)
    changed <- TRUE
  }
  if (repair && changed) {
    xml2::write_xml(types_xml, content_types)
    entries <- list.files(temp_dir, recursive = TRUE, all.files = TRUE,
                          include.dirs = FALSE, no.. = TRUE)
    rebuilt <- tempfile("c04_xlsx_fixed_", fileext = ".xlsx")
    on.exit(if (file.exists(rebuilt)) unlink(rebuilt), add = TRUE)
    zip::zipr(rebuilt, files = entries, root = temp_dir,
              mode = "mirror", recurse = FALSE,
              include_directories = FALSE)
    # Verify the rebuilt OOXML package before replacing the staged workbook.
    stage_c_xlsx_package_check(rebuilt, repair = FALSE)
    if (!file.copy(rebuilt, path, overwrite = TRUE))
      stop("No se pudo guardar el XLSX reparado.")
  }
  invisible(TRUE)
}
