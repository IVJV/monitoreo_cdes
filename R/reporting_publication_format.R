# ==============================================================================
# Publication formatting for validated Stage C tables (HTML / Excel)
# ==============================================================================
# Display-only transformations and workbook styling. Analytical calculations,
# independent verification and the publication gate are defined elsewhere.

build_stage_c_publication_html <- function(data, stage_04c, table_number,
                                           period_title) {
  if (!(table_number %in% c(1L, 2L)))
    stop("Número de tabla no disponible.")

  table_gt <- if (table_number == 1L) {
    render_stage_c_decile_matrix(
      data, "Population", "Percentage", value_digits = 0L,
      stage_prefix = "s02c", stage_04c = stage_04c
    )
  } else {
    render_stage_c_decile_matrix(
      data, "Income_Millions", "Participation", value_digits = 2L,
      stage_prefix = "s03c", stage_04c = stage_04c
    )
  }

  title <- if (table_number == 1L) {
    "DISTRIBUCIÓN DE LA POBLACIÓN POR DECILES DE INGRESO PER CÁPITA"
  } else {
    "CONCENTRACIÓN DEL INGRESO PER CÁPITA POR DECILES"
  }
  subtitle <- if (table_number == 1L) {
    "Población y porcentaje de la población"
  } else {
    "Ingreso per cápita (millones de USD) y participación (%)"
  }

  table_gt |>
    gt::tab_header(
      title = gt::md(paste0("**", title, " — ", period_title, "**")),
      subtitle = gt::md(paste0("*", subtitle, "*"))
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(columns = tidyselect::everything())
    )
}

# Numeric workbook view equivalent to historical Sections 2C.4 and 3C.4.
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

  title <- if (table_number == 1L) {
    "DISTRIBUCIÓN DE LA POBLACIÓN POR DECILES DE INGRESO PER CÁPITA"
  } else {
    "CONCENTRACIÓN DEL INGRESO PER CÁPITA POR DECILES"
  }
  subtitle <- if (table_number == 1L) {
    "Población y porcentaje de la población"
  } else {
    "Ingreso per cápita (millones de USD) y participación (%)"
  }

  openxlsx::writeData(wb, sheet, title, startRow = 1L, startCol = 1L)
  openxlsx::writeData(wb, sheet, subtitle, startRow = 2L, startCol = 1L)
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
