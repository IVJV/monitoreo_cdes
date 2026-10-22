# ==============================================================================
# Stage 02C — Population distribution by per capita income decile
# Methodological source: 3.1 ENEMDU_2008_12_Distribución_Concentración_ingpc.R,
# historical Point 2C.1 (lines 35–85).
# ==============================================================================


# ==============================================================================
# 1. Construct the numeric distribution table for subsequent Stage 04C control
# ==============================================================================
build_population_distribution_stage <- function(
    ENEMDU_indicators,
    population_classification_table_num,
    period_excel) {

  if (!is.data.frame(ENEMDU_indicators))
    stop("La Etapa 02C requiere la base laboral recibida en 01C.")

  if (!is.data.frame(population_classification_table_num) ||
      !all(c("Variable", "Category") %in%
           names(population_classification_table_num)))
    stop("La Etapa 02C requiere la clasificación poblacional de 02B.")

  if (!is.character(period_excel) || length(period_excel) != 1L ||
      is.na(period_excel) || !nzchar(period_excel))
    stop("La Etapa 02C requiere la etiqueta del período preparada en 01C.")

  # Historical Point 2C.1(a–b): reuse the 16 official indicators and their
  # hierarchical order from Stage 02B; labels remain attached to the base.
  population_vars <- as.character(population_classification_table_num$Variable)
  if (length(population_vars) != 16L || anyNA(population_vars) ||
      anyDuplicated(population_vars) ||
      !identical(population_vars[[1L]], "poblacion_total"))
    stop("La clasificación de 02B debe contener los 16 indicadores ordenados.")

  required_vars <- c("ningpc", "fexp", population_vars)
  missing_vars <- setdiff(required_vars, names(ENEMDU_indicators))
  if (length(missing_vars) > 0L)
    stop(paste("Faltan variables requeridas por 02C:",
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

  # Historical Point 2C.1(c): exact expanded counts, within-decile shares (0–100).
  population_distribution_decile <- ENEMDU_indicators |>
    dplyr::transmute(
      ningpc = as.integer(ningpc),
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
      Population = sum(fexp * Indicator, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(ningpc) |>
    dplyr::mutate(
      Percentage = Population / Population[Variable == "poblacion_total"] * 100
    ) |>
    dplyr::ungroup()

  # Historical Point 2C.1(d): totals are sums across the ten deciles.
  population_distribution_total <- population_distribution_decile |>
    dplyr::group_by(Variable) |>
    dplyr::summarise(Population = sum(Population), .groups = "drop") |>
    dplyr::mutate(
      ningpc = 0L,
      Percentage = Population / Population[Variable == "poblacion_total"] * 100
    )

  # Historical Point 2C.1(e): retain unrounded numeric output for Stage 04C.
  population_distribution_table_num <- dplyr::bind_rows(
    population_distribution_total,
    population_distribution_decile
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
    dplyr::select(Periodo, Variable, Indicator, Decile, Population, Percentage)

  list(
    stage_id = "02c_population_distribution",
    population_distribution_table_num = population_distribution_table_num
  )
}
