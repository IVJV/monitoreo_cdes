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
# 5. Canonical INGPC-universe presentation
# ==============================================================================

# Stable display order and labels for the canonical INGPC universe.
# The analytical values originate in Stage 01. These definitions affect only
# presentation and are reused wherever that same universe is displayed.
ingpc_universe_order <- c(
  "input_total",
  "ingpc_missing",
  "ingpc_valid"
)

ingpc_universe_labels <- c(
  input_total = "Base ENEMDU completa",
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
# 6. Common audit-diagnostic rendering
# ==============================================================================

render_audit_diagnostics <- function(audit_diagnostics) {

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

  audit_diagnostics |>
    gt::gt() |>
    gt::fmt_markdown(
      columns = Control
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
        columns = Resultado
      )
    ) |>
    gt::cols_align(
      align = "left",
      columns = c(
        Control,
        `Verificaciones incluidas`
      )
    ) |>
    gt::cols_align(
      align = "center",
      columns = Resultado
    )
}


# ==============================================================================
# 7. Common stage-status rendering
# ==============================================================================

render_stage_status <- function(stage_passed) {

  if (
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
