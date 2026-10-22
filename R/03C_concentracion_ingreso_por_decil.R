# ==============================================================================
# Stage 03C — Income concentration by per capita income decile
# Methodological source: 3.1 ENEMDU_2008_12_Distribución_Concentración_ingpc.R,
# historical Point 3C.1 (income-concentration calculation).
# ==============================================================================


# ==============================================================================
# 1. Construct the numeric concentration table for subsequent Stage 04C control
# ==============================================================================
build_income_concentration_stage <- function(
    ENEMDU_indicators,
    population_classification_table_num,
    period_excel) {

  if (!is.data.frame(ENEMDU_indicators))
    stop("La Etapa 03C requiere la base laboral recibida en 01C.")

  if (!is.data.frame(population_classification_table_num) ||
      !all(c("Variable", "Category") %in%
           names(population_classification_table_num)))
    stop("La Etapa 03C requiere la clasificación poblacional de 02B.")

  if (!is.character(period_excel) || length(period_excel) != 1L ||
      is.na(period_excel) || !nzchar(period_excel))
    stop("La Etapa 03C requiere la etiqueta del período preparada en 01C.")

  # Reuse the 16 official indicators in the order defined by Stage 02B.
  population_vars <- as.character(population_classification_table_num$Variable)
  if (length(population_vars) != 16L || anyNA(population_vars) ||
      anyDuplicated(population_vars) ||
      !identical(population_vars[[1L]], "poblacion_total"))
    stop("La clasificación de 02B debe contener los 16 indicadores ordenados.")

  required_vars <- c("ningpc", "ingpc", "fexp", population_vars)
  missing_vars <- setdiff(required_vars, names(ENEMDU_indicators))
  if (length(missing_vars) > 0L)
    stop(paste("Faltan variables requeridas por 03C:",
               paste(missing_vars, collapse = ", ")))

  population_labels <- vapply(
    population_vars,
    function(variable) {
      variable_label <- attr(
        ENEMDU_indicators[[variable]], "label", exact = TRUE
      )
      if (is.null(variable_label)) variable else as.character(variable_label)
    },
    character(1)
  )

  # Historical Point 3C.1(a): weighted aggregate income in millions of USD;
  # within-decile participation is expressed on a 0–100 scale.
  income_concentration_decile <- ENEMDU_indicators |>
    dplyr::transmute(
      ningpc = as.integer(ningpc),
      ingpc = as.numeric(ingpc),
      fexp = as.numeric(fexp),
      dplyr::across(tidyselect::all_of(population_vars), as.numeric)
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(population_vars),
      names_to = "Variable",
      values_to = "Indicator"
    ) |>
    dplyr::group_by(ningpc, Variable) |>
    dplyr::summarise(
      Income_Millions = sum(fexp * ingpc * Indicator, na.rm = TRUE) / 1000000,
      .groups = "drop"
    ) |>
    dplyr::group_by(ningpc) |>
    dplyr::mutate(
      Participation = Income_Millions /
        Income_Millions[Variable == "poblacion_total"] * 100
    ) |>
    dplyr::ungroup()

  # Historical Point 3C.1(b): totals are sums across the ten deciles.
  income_concentration_total <- income_concentration_decile |>
    dplyr::group_by(Variable) |>
    dplyr::summarise(Income_Millions = sum(Income_Millions), .groups = "drop") |>
    dplyr::mutate(
      ningpc = 0L,
      Participation = Income_Millions /
        Income_Millions[Variable == "poblacion_total"] * 100
    )

  # Historical Point 3C.1(c): retain exact values for Stage 04C verification.
  income_concentration_table_num <- dplyr::bind_rows(
    income_concentration_total,
    income_concentration_decile
  ) |>
    dplyr::mutate(
      Periodo = period_excel,
      Variable = factor(Variable, levels = population_vars),
      Decile = dplyr::if_else(
        ningpc == 0L, "Total", paste("Decil", ningpc)
      ),
      Decile = factor(Decile, levels = c("Total", paste("Decil", 1:10))),
      Indicator = unname(population_labels[as.character(Variable)])
    ) |>
    dplyr::arrange(Variable, Decile) |>
    dplyr::select(
      Periodo, Variable, Indicator, Decile, Income_Millions, Participation
    )

  list(
    stage_id = "03c_income_concentration",
    income_concentration_table_num = income_concentration_table_num
  )
}
