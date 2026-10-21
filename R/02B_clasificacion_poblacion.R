# ==============================================================================
# Stage 02B — Population classification from the original CONDACTN codes
# Methodological source: 2.1 ENEMDU_2008_12_Indicadores_Mercado_Laboral.R,
# historical Point 2B (lines 82–315).
# ==============================================================================


# ==============================================================================
# 1. Construct the 16 labour-market indicators and preserve their metadata
# ==============================================================================
build_population_classification_base <- function(ENEMDU_decile) {

  # Preserve the decile base: add the indicators to a separate working dataset.
  ENEMDU_laboral <- ENEMDU_decile
  condactn_num <- as.numeric(ENEMDU_laboral$CONDACTN)

  make_indicator <- function(condition) {
    dplyr::if_else(
      is.na(condactn_num), NA_integer_, as.integer(condition)
    )
  }

  # Historical Point 2B.1: general population aggregates.
  ENEMDU_laboral <- ENEMDU_laboral |>
    dplyr::mutate(
      pet = make_indicator(dplyr::between(condactn_num, 1, 9)),
      pea = make_indicator(dplyr::between(condactn_num, 1, 8)),
      poblacion_ocupada = make_indicator(dplyr::between(condactn_num, 1, 6)),
      subempleo = make_indicator(condactn_num %in% c(2, 3)),
      desempleo = make_indicator(condactn_num %in% c(7, 8)),
      poblacion_total = dplyr::if_else(is.na(condactn_num), NA_integer_, 1L),
      menor_15 = make_indicator(condactn_num == 0),
      pei = make_indicator(condactn_num == 9)
    )

  # Historical Point 2B.2: CONDACTN subcategories.
  ENEMDU_laboral <- ENEMDU_laboral |>
    dplyr::mutate(
      empleo_adecuado = make_indicator(condactn_num == 1),
      subempleo_tiempo = make_indicator(condactn_num == 2),
      subempleo_ingresos = make_indicator(condactn_num == 3),
      otro_empleo_inadecuado = make_indicator(condactn_num == 4),
      empleo_no_remunerado = make_indicator(condactn_num == 5),
      empleo_no_clasificado = make_indicator(condactn_num == 6),
      desempleo_abierto = make_indicator(condactn_num == 7),
      desempleo_oculto = make_indicator(condactn_num == 8)
    )

  # Historical Point 2B.3: official variable labels and binary value labels.
  auxiliary_variable_labels <- c(
    pet = "Población en Edad de Trabajar (PET)",
    pea = "Población Económicamente Activa (PEA)",
    poblacion_ocupada = "Población Ocupada (PO)",
    subempleo = "Subempleo",
    desempleo = "Desempleo",
    poblacion_total = "Población total",
    menor_15 = "Población menor de 15 años",
    pei = "Población Económicamente Inactiva (PEI)",
    empleo_adecuado = "Empleo adecuado",
    subempleo_tiempo = "Subempleo por insuficiencia de tiempo de trabajo",
    subempleo_ingresos = "Subempleo por insuficiencia de ingresos",
    otro_empleo_inadecuado = "Otro empleo inadecuado",
    empleo_no_remunerado = "Empleo no remunerado",
    empleo_no_clasificado = "Empleo no clasificado",
    desempleo_abierto = "Desempleo abierto",
    desempleo_oculto = "Desempleo oculto"
  )

  indicator_value_labels <- c("No pertenece" = 0L, "Pertenece" = 1L)

  for (variable in names(auxiliary_variable_labels)) {
    ENEMDU_laboral[[variable]] <- haven::labelled(
      as.integer(ENEMDU_laboral[[variable]]),
      labels = indicator_value_labels,
      label = unname(auxiliary_variable_labels[[variable]])
    )
  }

  ENEMDU_laboral
}


# ==============================================================================
# 2. Preserve the exact numerical hierarchy; no gt or visual formatting here
# ==============================================================================
build_population_classification_hierarchy <- function(ENEMDU_laboral) {

  # Historical Point 2B.4: conceptual hierarchy and its parent relationships.
  population_hierarchy <- data.frame(
    Order = 1:16,
    Level = c(0, 1, 1, 2, 3, 4, 4, 5, 5, 4, 4, 4, 3, 4, 4, 2),
    Parent = c(
      NA_character_, "poblacion_total", "poblacion_total", "pet", "pea",
      "poblacion_ocupada", "poblacion_ocupada", "subempleo", "subempleo",
      "poblacion_ocupada", "poblacion_ocupada", "poblacion_ocupada",
      "pea", "desempleo", "desempleo", "pet"
    ),
    Variable = c(
      "poblacion_total", "menor_15", "pet", "pea", "poblacion_ocupada",
      "empleo_adecuado", "subempleo", "subempleo_tiempo",
      "subempleo_ingresos", "otro_empleo_inadecuado",
      "empleo_no_remunerado", "empleo_no_clasificado", "desempleo",
      "desempleo_abierto", "desempleo_oculto", "pei"
    ),
    stringsAsFactors = FALSE
  )

  population_hierarchy$Category <- vapply(
    population_hierarchy$Variable,
    function(variable) {
      as.character(attr(ENEMDU_laboral[[variable]], "label", exact = TRUE))
    },
    character(1)
  )

  population_hierarchy$Expanded_Frequency <- vapply(
    population_hierarchy$Variable,
    function(variable) {
      sum(
        ENEMDU_laboral$fexp * as.numeric(ENEMDU_laboral[[variable]]),
        na.rm = TRUE
      )
    },
    numeric(1)
  )

  total_population <- population_hierarchy$Expanded_Frequency[
    population_hierarchy$Variable == "poblacion_total"
  ]
  parent_frequency <- population_hierarchy$Expanded_Frequency[
    match(population_hierarchy$Parent, population_hierarchy$Variable)
  ]

  population_hierarchy$Population_Proportion <-
    population_hierarchy$Expanded_Frequency / total_population
  population_hierarchy$Parent_Proportion <- ifelse(
    is.na(population_hierarchy$Parent),
    1,
    population_hierarchy$Expanded_Frequency / parent_frequency
  )

  dplyr::arrange(population_hierarchy, Order)
}


# ==============================================================================
# 3. Reconcile all 16 indicators with the exact Stage 01B reference
# ==============================================================================
validate_population_classification <- function(
    population_classification_table_num,
    condactn_frequency_reference) {

  # Historical Point 2B.5: preserve all 10 original codes, including absent ones.
  population_totals <- stats::setNames(
    population_classification_table_num$Expanded_Frequency,
    population_classification_table_num$Variable
  )

  condactn_totals <- stats::setNames(rep(0, 10), as.character(0:9))
  condactn_totals[as.character(condactn_frequency_reference$CONDACTN)] <-
    condactn_frequency_reference$Frequency

  expected_from_condactn <- c(
    poblacion_total = sum(condactn_totals),
    menor_15 = unname(condactn_totals["0"]),
    pet = sum(condactn_totals[as.character(1:9)]),
    pea = sum(condactn_totals[as.character(1:8)]),
    poblacion_ocupada = sum(condactn_totals[as.character(1:6)]),
    empleo_adecuado = unname(condactn_totals["1"]),
    subempleo = sum(condactn_totals[c("2", "3")]),
    subempleo_tiempo = unname(condactn_totals["2"]),
    subempleo_ingresos = unname(condactn_totals["3"]),
    otro_empleo_inadecuado = unname(condactn_totals["4"]),
    empleo_no_remunerado = unname(condactn_totals["5"]),
    empleo_no_clasificado = unname(condactn_totals["6"]),
    desempleo = sum(condactn_totals[c("7", "8")]),
    desempleo_abierto = unname(condactn_totals["7"]),
    desempleo_oculto = unname(condactn_totals["8"]),
    pei = unname(condactn_totals["9"])
  )

  condactn_validation_difference <-
    population_totals[names(expected_from_condactn)] - expected_from_condactn

  population_validation <- data.frame(
    Indicator = names(expected_from_condactn),
    Reference = as.numeric(expected_from_condactn),
    Observed = as.numeric(population_totals[names(expected_from_condactn)]),
    Difference = as.numeric(condactn_validation_difference),
    row.names = NULL
  )

  # Preserve the historical absolute tolerance; abort on failed reconciliation.
  population_classification_consistency <-
    all(is.finite(condactn_validation_difference)) &&
    all(abs(condactn_validation_difference) <= 1e-8)

  if (!population_classification_consistency) {
    inconsistent <- names(condactn_validation_difference)[
      !is.finite(condactn_validation_difference) |
        abs(condactn_validation_difference) > 1e-8
    ]
    abort_stage_validation(
      stage_id = "02b_population_classification",
      check_id = "population_classification_consistency",
      message = paste(
        "Los indicadores poblacionales no reproducen la distribución original de CONDACTN:",
        paste(inconsistent, collapse = ", ")
      )
    )
  }

  validation_evidence <- data.frame(
    stage_id = "02b_population_classification",
    check_id = "population_classification_consistency",
    passed = population_classification_consistency,
    stringsAsFactors = FALSE
  )

  list(
    population_validation = population_validation,
    validation_evidence = validation_evidence
  )
}


# ==============================================================================
# 4. Run Stage 02B once and return its production and audit outputs
# ==============================================================================
build_population_classification_stage <- function(
    ENEMDU_decile,
    condactn_frequency_reference) {

  ENEMDU_laboral <- build_population_classification_base(ENEMDU_decile)
  population_classification_table_num <-
    build_population_classification_hierarchy(ENEMDU_laboral)
  population_check <- validate_population_classification(
    population_classification_table_num,
    condactn_frequency_reference
  )

  list(
    data = ENEMDU_laboral,
    population_classification_table_num = population_classification_table_num,
    population_validation = population_check$population_validation,
    validation_evidence = population_check$validation_evidence
  )
}
