# ==============================================================================
# Common reporting helpers
# ==============================================================================
# Production reporting convention:
# "Muestra" = unweighted number of observations.
# "Población" = population estimate obtained using the expansion factor fexp.
# This convention affects presentation only; analytical methodology is unchanged.
#
# Technical names shown in reports (variables, objects, functions, identifiers)
# must preserve their exact internal spelling and be displayed using code formatting
# when referenced in narrative text. Example: ingpc, fexp, ningpc, dominio.

reporting_labels <- c(
  sample = "Muestra",
  valid_sample = "Muestra válida",
  missing_sample = "Muestra perdida",
  population = "Población",
  valid_population = "Población válida",
  missing_population = "Población perdida"
)


# ==============================================================================
# 1. Integer formatting
# ==============================================================================
format_integer_es <- function(x) {

  formatC(
    as.numeric(x),
    format = "f",
    digits = 0,
    big.mark = ".",
    decimal.mark = ","
  )
}


# ==============================================================================
# 2. Decimal formatting
# ==============================================================================
format_decimal_es <- function(x, digits = 2) {

  formatC(
    as.numeric(x),
    format = "f",
    digits = digits,
    big.mark = ".",
    decimal.mark = ","
  )
}


# ==============================================================================
# 3. Percentage formatting
# ==============================================================================
format_percentage_es <- function(x, digits = 1) {

  paste0(
    formatC(
      as.numeric(x),
      format = "f",
      digits = digits,
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )
}


# ==============================================================================
# 4. Scientific notation formatting
# ==============================================================================
format_scientific_es <- function(x, digits = 3) {

  formatted <- formatC(
    as.numeric(x),
    format = "e",
    digits = digits
  )

  gsub(
    ".",
    ",",
    formatted,
    fixed = TRUE
  )
}


# ==============================================================================
# 5. Preparation of display-only GT table views
# ==============================================================================
# The source data frame is not modified. Column IDs are explicitly mapped to
# their display-only identifiers; statistical transformations are not performed.
prepare_gt_view <- function(
  data,
  column_ids = character(),
  integer_columns = character(),
  decimal_columns = numeric()
) {
  if (!is.data.frame(data))
    stop("La vista GT requiere un data.frame.")

  if (!is.character(column_ids) ||
      (length(column_ids) > 0L &&
       (is.null(names(column_ids)) || anyNA(names(column_ids)) ||
        any(!nzchar(names(column_ids))) || anyNA(column_ids) ||
        any(!nzchar(column_ids)) || anyDuplicated(names(column_ids)))))
    stop("column_ids debe ser un vector de correspondencias con nombres válidos.")

  if (!is.character(integer_columns) || anyNA(integer_columns) ||
      anyDuplicated(integer_columns))
    stop("integer_columns debe contener nombres de columnas únicos.")

  if (!is.numeric(decimal_columns) ||
      (length(decimal_columns) > 0L &&
       (is.null(names(decimal_columns)) || anyNA(names(decimal_columns)) ||
        any(!nzchar(names(decimal_columns))) ||
        anyDuplicated(names(decimal_columns)) ||
        anyNA(decimal_columns) ||
        any(!is.finite(decimal_columns)) ||
        any(decimal_columns < 0 | decimal_columns != floor(decimal_columns)))))
    stop("decimal_columns debe asociar cada columna a decimales enteros no negativos.")

  view <- data
  if (length(column_ids) > 0L) {
    missing_source <- setdiff(names(column_ids), names(view))
    if (length(missing_source) > 0L)
      stop(paste("Columnas originales ausentes:", paste(missing_source, collapse = ", ")))

    renamed <- names(view)
    renamed[match(names(column_ids), renamed)] <- unname(column_ids)
    if (anyDuplicated(renamed))
      stop("Los identificadores GT deben ser únicos.")
    names(view) <- renamed
  }

  target_columns <- c(integer_columns, names(decimal_columns))
  if (anyDuplicated(target_columns))
    stop("Una columna no puede tener dos formatos diferentes.")
  missing_target <- setdiff(target_columns, names(view))
  if (length(missing_target) > 0L)
    stop(paste("Columnas para formato ausentes:", paste(missing_target, collapse = ", ")))

  for (column in integer_columns)
    view[[column]] <- format_integer_es(view[[column]])
  for (column in names(decimal_columns))
    view[[column]] <- format_decimal_es(
      view[[column]], digits = decimal_columns[[column]]
    )

  view
}


# ==============================================================================
# 6. Canonical INGPC-universe presentation
# ==============================================================================
# Stable display order and labels for the canonical INGPC universe.
# The analytical values originate in Stage A01. These definitions affect only
# presentation and are reused wherever that same universe is displayed.
ingpc_universe_order <- c(
  "input_total",
  "ingpc_missing",
  "ingpc_valid"
)

ingpc_universe_labels <- c(
  input_total = "Base ENEMDU original",
  ingpc_missing = "Registros con `ingpc` perdido",
  ingpc_valid = "Universo analítico (`ingpc` válido)"
)


build_ingpc_universe_report <- function(ingpc_universe_summary) {

  required_columns <- c(
    "universe_id",
    "Sample_N",
    "Expanded_N"
  )

  missing_columns <- setdiff(
    required_columns,
    names(ingpc_universe_summary)
  )

  if (length(missing_columns) > 0L)
    stop(
      paste(
        "El resumen canónico del universo de ingpc no contiene:",
        paste(missing_columns, collapse = ", ")
      )
    )

  missing_universes <- setdiff(
    ingpc_universe_order,
    ingpc_universe_summary$universe_id
  )

  if (length(missing_universes) > 0L)
    stop(
      paste(
        "Faltan universos canónicos de ingpc:",
        paste(missing_universes, collapse = ", ")
      )
    )

  row_index <- match(
    ingpc_universe_order,
    ingpc_universe_summary$universe_id
  )

  data.frame(
    Universo = unname(
      ingpc_universe_labels[
        ingpc_universe_order
      ]
    ),
    Muestra = format_integer_es(
      ingpc_universe_summary$Sample_N[row_index]
    ),
    Población = format_integer_es(
      ingpc_universe_summary$Expanded_N[row_index]
    ),
    stringsAsFactors = FALSE
  )
}


render_ingpc_universe_table <- function(ingpc_universe_summary) {

  ingpc_universe_report <-
    build_ingpc_universe_report(
      ingpc_universe_summary
    )

  ingpc_universe_report |>
    gt::gt() |>
    gt::fmt_markdown(
      columns = Universo
    ) |>
    gt::cols_align(
      align = "left",
      columns = Universo
    ) |>
    gt::cols_align(
      align = "right",
      columns = c(
        Muestra,
        Población
      )
    )
}


# ==============================================================================
# 7. Common audit-diagnostic rendering
# ==============================================================================
render_audit_diagnostics <- function(audit_diagnostics, stage_id) {

  required_columns <- c(
    "Control",
    "Verificaciones incluidas",
    "Resultado"
  )

  missing_columns <- setdiff(
    required_columns,
    names(audit_diagnostics)
  )

  if (length(missing_columns) > 0L)
    stop(
      paste(
        "El diagnóstico de auditoría no contiene:",
        paste(missing_columns, collapse = ", ")
      )
    )

  # gt emits column names as HTML header IDs. Namespace the display-only
  # columns per stage so separate audit tables never repeat IDs in the page.
  # The source audit evidence and the labels shown to readers stay unchanged.
  if (!is.character(stage_id) || length(stage_id) != 1L ||
      is.na(stage_id) || !grepl("^[A-Za-z0-9_]+$", stage_id))
    stop("El diagnóstico de auditoría requiere un stage_id válido.")

  display_columns <- paste0(
    "audit_", stage_id, "_", c("control", "checks", "result")
  )

  audit_display <- audit_diagnostics
  names(audit_display)[match(required_columns, names(audit_display))] <-
    display_columns

  audit_display |>
    gt::gt() |>
    gt::cols_label(
      .list = as.list(stats::setNames(required_columns, display_columns))
    ) |>
    gt::fmt_markdown(
      columns = tidyselect::all_of(display_columns[1])
    ) |>
    gt::text_transform(
      fn = function(x) {
        ifelse(
          x == "OK",
          paste0(
            '<span class="status-ok">',
            x,
            '</span>'
          ),
          paste0(
            '<span class="status-fail">',
            x,
            '</span>'
          )
        )
      },
      locations = gt::cells_body(
        columns = tidyselect::all_of(display_columns[3])
      )
    ) |>
    gt::cols_align(
      align = "left",
      columns = tidyselect::all_of(display_columns[1:2])
    ) |>
    gt::cols_align(
      align = "center",
      columns = tidyselect::all_of(display_columns[3])
    )
}


# ==============================================================================
# 8. Common stage-status rendering
# ==============================================================================
render_stage_status <- function(stage_passed) {

  if (
    !is.logical(stage_passed) ||
    length(stage_passed) != 1L ||
    is.na(stage_passed)
  ) {
    stop(
      "stage_passed debe contener un único valor lógico no perdido."
    )
  }

  stage_status <- if (isTRUE(stage_passed)) {
    "EJECUCIÓN CORRECTA — CONTROLES INTERNOS SUPERADOS"
  } else {
    "ETAPA NO APROBADA"
  }

  stage_status_class <- if (isTRUE(stage_passed)) {
    "stage-status-ok"
  } else {
    "stage-status-fail"
  }

  cat(
    paste0(
      '<div class="stage-status ',
      stage_status_class,
      '">',
      stage_status,
      '</div>'
    )
  )

  invisible(NULL)
}


# ==============================================================================
# 9. Common presentation for the two validated Stage C decile matrices
# ==============================================================================
# Reshape and format only a display copy. Analytical values remain untouched.
# The four Stage C04 controls must approve both tables before they are shown.
render_stage_c_decile_matrix <- function(
    data, value_column, percentage_column,
    value_digits = 0L, stage_prefix, stage_C04) {

  if (!is.list(stage_C04) ||
      !isTRUE(stage_C04$verification_approved) ||
      !isTRUE(stage_C04$stage_passed))
    stop("Las tablas de C02 y C03 requieren la aprobación de C04.")

  if (!is.character(stage_prefix) || length(stage_prefix) != 1L ||
      is.na(stage_prefix) || !grepl("^s[0-9]{2}c$", stage_prefix))
    stop("Identificador de presentación de la tabla C inválido.")

  required <- c("Variable", "Indicator", "Decile", value_column,
                percentage_column)
  if (!is.data.frame(data) || !all(required %in% names(data)))
    stop("La tabla numérica de la etapa C no tiene la estructura requerida.")

  if (nrow(data) != 176L || anyDuplicated(paste(
        as.character(data$Variable), as.character(data$Decile), sep = "|")))
    stop("La vista de la etapa C requiere 16 indicadores y 11 categorías únicas.")

  if (!is.numeric(value_digits) || length(value_digits) != 1L ||
      is.na(value_digits) || value_digits < 0 ||
      value_digits != floor(value_digits))
    stop("La precisión de presentación debe ser un entero no negativo.")

  display <- data |>
    dplyr::select(
      Variable, Indicator, Decile,
      tidyselect::all_of(c(value_column, percentage_column))
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(c(value_column, percentage_column)),
      names_to = ".measure", values_to = ".value"
    ) |>
    dplyr::mutate(
      .measure = factor(.measure,
                        levels = c(value_column, percentage_column)),
      .indicator = ifelse(as.character(.measure) == value_column,
                          Indicator, ""),
      .display = ifelse(
        as.character(.measure) == value_column,
        format_decimal_es(.value, digits = value_digits),
        format_percentage_es(.value, digits = 1L)
      )
    ) |>
    dplyr::select(Variable, .measure, .indicator, Decile, .display) |>
    tidyr::pivot_wider(names_from = Decile, values_from = .display) |>
    dplyr::arrange(Variable, .measure) |>
    dplyr::select(-Variable, -.measure)

  expected_columns <- c(".indicator", "Total", paste("Decil", 1:10))
  if (!identical(names(display), expected_columns))
    stop("La tabla de deciles no presenta todas las columnas esperadas.")

  display_names <- paste0(stage_prefix, "_", c(
    "indicator", "total", paste0("decile_", 1:10)
  ))
  names(display) <- display_names

  display |>
    gt::gt() |>
    gt::cols_label(
      .list = as.list(stats::setNames(
        c("Indicador", "Total", paste("Decil", 1:10)),
        display_names
      ))
    ) |>
    gt::cols_align(
      align = "left",
      columns = tidyselect::all_of(display_names[1])
    ) |>
    gt::cols_align(
      align = "right",
      columns = tidyselect::all_of(display_names[-1])
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = "#657181"),
      locations = gt::cells_body(rows = seq.int(2L, nrow(display), by = 2L))
    ) |>
    gt::tab_options(table.width = gt::px(1500))
}


# ==============================================================================
# 10. Shared display-only Excel download link for validated publications
# ==============================================================================
# The reporting runner creates the referenced file; this helper only renders a
# relative hyperlink, without generating files or changing analytical results.
render_stage_c_excel_download <- function(filename) {

  if (!is.character(filename) || length(filename) != 1L || is.na(filename) ||
      !grepl("^Tabla_[12]_[A-Za-z0-9_]+\\.xlsx$", filename))
    stop("La publicación requiere un nombre de archivo Excel válido.")

  # The technical HTML is written to output/reports/technical/reporting/.
  # Point to the only published workbook, under output/publications/YYYY_MM/.
  period <- sub("^.*_([0-9]{4}_[0-9]{2})\\.xlsx$", "\\1", filename)
  publication_href <- paste0("../../../publications/", period, "/", filename)

  paste0(
    '<a class="rap-excel-download" href="', publication_href,
    '" download="', filename, '">',
    '<svg xmlns="http://www.w3.org/2000/svg" width="17" height="17" ',
    'viewBox="0 0 24 24" fill="none" stroke="currentColor" ',
    'stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" ',
    'aria-hidden="true" focusable="false">',
    '<rect x="4" y="2" width="16" height="20" rx="2"/>',
    '<path d="M8 7h8M8 12h8M8 17h8M12 7v10"/>',
    '</svg> Descargar tabla en Excel (.xlsx)</a>'
  )
}
