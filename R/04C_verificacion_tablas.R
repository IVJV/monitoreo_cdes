# ==============================================================================
# Stage 04C — Independent verification of Tables 1 and 2 against CONDACTN
# Methodological source: 3.1 ENEMDU_2008_12_Distribución_Concentración_ingpc.R,
# historical Points 4C.1–4C.4 (verification before any output publication).
# ==============================================================================

build_table_output_verification_stage <- function(
    ENEMDU_indicators,
    population_distribution_table_num,
    income_concentration_table_num) {

  stage_id <- "04c_table_output_verification"
  check_id <- "table_output_consistency"
  verification_tolerance <- 1e-6

  # Preserve the independent reference: only original CONDACTN, decile, income,
  # and expansion weights are used. Constructed 02B indicators are not reused.
  if (!is.data.frame(ENEMDU_indicators) ||
      !all(c("CONDACTN", "ningpc", "ingpc", "fexp") %in%
           names(ENEMDU_indicators)))
    stop("04C requiere CONDACTN, ningpc, ingpc y fexp en la base de 01C.")

  required_table1 <- c("Variable", "Decile", "Population", "Percentage")
  required_table2 <- c("Variable", "Decile", "Income_Millions", "Participation")

  if (!is.data.frame(population_distribution_table_num) ||
      !all(required_table1 %in% names(population_distribution_table_num)))
    stop("04C requiere la tabla numérica de distribución producida por 02C.")
  if (!is.data.frame(income_concentration_table_num) ||
      !all(required_table2 %in% names(income_concentration_table_num)))
    stop("04C requiere la tabla numérica de concentración producida por 03C.")

  # Historical Point 4C.1(d): original activity-code membership is declared
  # independently of the indicator definitions in 02B.
  verification_codes <- list(
    poblacion_total = 0:9,
    menor_15 = 0,
    pet = 1:9,
    pea = 1:8,
    poblacion_ocupada = 1:6,
    empleo_adecuado = 1,
    subempleo = 2:3,
    subempleo_tiempo = 2,
    subempleo_ingresos = 3,
    otro_empleo_inadecuado = 4,
    empleo_no_remunerado = 5,
    empleo_no_clasificado = 6,
    desempleo = 7:8,
    desempleo_abierto = 7,
    desempleo_oculto = 8,
    pei = 9
  )

  # A missing or duplicated cell must not be mistaken for a passing check.
  required_deciles <- c("Total", paste("Decil", 1:10))
  expected_keys <- as.vector(outer(
    names(verification_codes), required_deciles, paste, sep = "|"
  ))
  check_table_keys <- function(table, label) {
    keys <- paste(as.character(table$Variable),
                  as.character(table$Decile), sep = "|")
    if (length(keys) != 176L || anyNA(table$Variable) ||
        anyNA(table$Decile) || anyDuplicated(keys) ||
        !setequal(keys, expected_keys))
      abort_stage_validation(
        stage_id = stage_id, check_id = check_id,
        message = paste("Cobertura incompleta o duplicada en", label,
                        "(16 indicadores por total y 10 deciles).")
      )
  }
  check_table_keys(population_distribution_table_num, "Tabla 1")
  check_table_keys(income_concentration_table_num, "Tabla 2")

  # Historical Point 4C.1(a–c): independently tabulate the original codes.
  verification_source <- ENEMDU_indicators |>
    dplyr::transmute(
      CONDACTN_code = as.integer(CONDACTN),
      Activity_condition = as.character(
        haven::as_factor(CONDACTN, levels = "labels")
      ),
      ningpc = as.integer(ningpc),
      ingpc = as.numeric(ingpc),
      fexp = as.numeric(fexp)
    )

  if (any(!is.na(verification_source$CONDACTN_code) &
          !(verification_source$CONDACTN_code %in% 0:9)) ||
      anyNA(verification_source$ningpc) ||
      any(!(verification_source$ningpc %in% 1:10)) ||
      any(!is.finite(verification_source$ingpc)) ||
      any(!is.finite(verification_source$fexp)) ||
      any(verification_source$fexp <= 0))
    abort_stage_validation(
      stage_id = stage_id, check_id = check_id,
      message = "04C recibió códigos, deciles, ingresos o factores de expansión inválidos."
    )

  verification_table_num <- verification_source |>
    dplyr::filter(!is.na(CONDACTN_code)) |>
    dplyr::group_by(CONDACTN_code, Activity_condition) |>
    dplyr::summarise(
      Population = sum(fexp),
      Income_Per_Capita_Total_USD = sum(ingpc * fexp),
      .groups = "drop"
    ) |>
    dplyr::arrange(CONDACTN_code)

  verification_table_num <- dplyr::bind_rows(
    verification_table_num,
    verification_table_num |>
      dplyr::summarise(
        CONDACTN_code = NA_integer_,
        Activity_condition = "Total",
        Population = sum(Population),
        Income_Per_Capita_Total_USD = sum(Income_Per_Capita_Total_USD)
      )
  )

  # Historical Point 4C.1(e): reference total per indicator, from source codes.
  source_total <- function(variable, codes) {
    verification_source |>
      dplyr::filter(CONDACTN_code %in% codes) |>
      dplyr::summarise(
        Variable = variable,
        Population_CONDACTN = sum(fexp),
        Income_Millions_CONDACTN = sum(ingpc * fexp) / 1000000
      )
  }
  verification_reference_totals <- dplyr::bind_rows(lapply(
    names(verification_codes),
    function(variable) source_total(variable, verification_codes[[variable]])
  ))

  # Historical Point 4C.2: compare the sixteen Total entries in both tables.
  table1_totals <- population_distribution_table_num |>
    dplyr::filter(as.character(Decile) == "Total") |>
    dplyr::transmute(Variable = as.character(Variable),
                     Population_Table1 = Population)
  table2_totals <- income_concentration_table_num |>
    dplyr::filter(as.character(Decile) == "Total") |>
    dplyr::transmute(Variable = as.character(Variable),
                     Income_Millions_Table2 = Income_Millions)

  total_verification <- verification_reference_totals |>
    dplyr::left_join(table1_totals, by = "Variable") |>
    dplyr::left_join(table2_totals, by = "Variable") |>
    dplyr::mutate(
      Population_Difference = Population_Table1 - Population_CONDACTN,
      Income_Difference = Income_Millions_Table2 - Income_Millions_CONDACTN,
      Table1_Total_OK = is.finite(Population_Table1) &
        is.finite(Population_Difference) &
        abs(Population_Difference) <= verification_tolerance,
      Table2_Total_OK = is.finite(Income_Millions_Table2) &
        is.finite(Income_Difference) &
        abs(Income_Difference) <= verification_tolerance
    )

  # Historical Point 4C.3(h): reconstruct D1–D10 independently from CONDACTN.
  source_decile <- function(variable, codes) {
    verification_source |>
      dplyr::filter(CONDACTN_code %in% codes) |>
      dplyr::group_by(ningpc) |>
      dplyr::summarise(
        Population_CONDACTN = sum(fexp),
        Income_Millions_CONDACTN = sum(ingpc * fexp) / 1000000,
        .groups = "drop"
      ) |>
      tidyr::complete(
        ningpc = 1:10,
        fill = list(Population_CONDACTN = 0,
                    Income_Millions_CONDACTN = 0)
      ) |>
      dplyr::mutate(Variable = variable, .before = 1)
  }

  verification_reference_deciles <- dplyr::bind_rows(lapply(
    names(verification_codes),
    function(variable) source_decile(variable, verification_codes[[variable]])
  )) |>
    dplyr::group_by(ningpc) |>
    dplyr::mutate(
      Percentage_CONDACTN = Population_CONDACTN /
        Population_CONDACTN[Variable == "poblacion_total"] * 100,
      Participation_CONDACTN = Income_Millions_CONDACTN /
        Income_Millions_CONDACTN[Variable == "poblacion_total"] * 100
    ) |>
    dplyr::ungroup()

  # Historical Point 4C.3(i–j): independently verify all 160 decile entries.
  table1_deciles <- population_distribution_table_num |>
    dplyr::filter(as.character(Decile) != "Total") |>
    dplyr::transmute(
      Variable = as.character(Variable),
      ningpc = match(as.character(Decile), paste("Decil", 1:10)),
      Population_Table1 = Population,
      Percentage_Table1 = Percentage
    )
  table2_deciles <- income_concentration_table_num |>
    dplyr::filter(as.character(Decile) != "Total") |>
    dplyr::transmute(
      Variable = as.character(Variable),
      ningpc = match(as.character(Decile), paste("Decil", 1:10)),
      Income_Millions_Table2 = Income_Millions,
      Participation_Table2 = Participation
    )

  decile_verification <- verification_reference_deciles |>
    dplyr::left_join(table1_deciles, by = c("Variable", "ningpc")) |>
    dplyr::left_join(table2_deciles, by = c("Variable", "ningpc")) |>
    dplyr::mutate(
      Population_Difference = Population_Table1 - Population_CONDACTN,
      Percentage_Difference = Percentage_Table1 - Percentage_CONDACTN,
      Income_Difference = Income_Millions_Table2 - Income_Millions_CONDACTN,
      Participation_Difference = Participation_Table2 - Participation_CONDACTN,
      Table1_Decile_OK = is.finite(Population_Table1) &
        is.finite(Percentage_Table1) &
        is.finite(Population_Difference) &
        is.finite(Percentage_Difference) &
        abs(Population_Difference) <= verification_tolerance &
        abs(Percentage_Difference) <= verification_tolerance,
      Table2_Decile_OK = is.finite(Income_Millions_Table2) &
        is.finite(Participation_Table2) &
        is.finite(Income_Difference) &
        is.finite(Participation_Difference) &
        abs(Income_Difference) <= verification_tolerance &
        abs(Participation_Difference) <= verification_tolerance
    )

  # Historical Point 4C.4: four mandatory checks feed a single release gate.
  verification_summary <- data.frame(
    Control = c("Tabla 1 — Totales", "Tabla 2 — Totales",
                "Tabla 1 — Deciles", "Tabla 2 — Deciles"),
    Verificaciones = c(16L, 16L, 160L, 160L),
    Discrepancias = c(
      sum(!total_verification$Table1_Total_OK),
      sum(!total_verification$Table2_Total_OK),
      sum(!decile_verification$Table1_Decile_OK),
      sum(!decile_verification$Table2_Decile_OK)
    ),
    stringsAsFactors = FALSE
  )
  verification_summary$Resultado <- ifelse(
    verification_summary$Discrepancias == 0L, "OK", "ERROR"
  )
  total_discrepancies <- sum(verification_summary$Discrepancias)
  verification_approved <- isTRUE(total_discrepancies == 0L)

  # Each mandatory check reports its own evidence, using the four results
  # already computed above. The existing aggregate publication gate is retained.
  verification_check_ids <- c(
    "table1_total_consistency",
    "table2_total_consistency",
    "table1_decile_consistency",
    "table2_decile_consistency"
  )

  validation_evidence <- data.frame(
    stage_id = rep(stage_id, length(verification_check_ids)),
    check_id = verification_check_ids,
    passed = verification_summary$Discrepancias == 0L,
    stringsAsFactors = FALSE
  )

  if (!verification_approved)
    abort_stage_validation(
      stage_id = stage_id,
      check_id = verification_check_ids[
        which(verification_summary$Discrepancias > 0L)[1L]
      ],
      message = paste(
        "Las tablas 1 y 2 no superaron la verificación independiente de CONDACTN.",
        "Discrepancias:", total_discrepancies
      )
    )

  list(
    stage_id = stage_id,
    verification_table_num = verification_table_num,
    verification_reference_totals = verification_reference_totals,
    verification_reference_deciles = verification_reference_deciles,
    total_verification = total_verification,
    decile_verification = decile_verification,
    verification_summary = verification_summary,
    verification_tolerance = verification_tolerance,
    total_discrepancies = total_discrepancies,
    verification_approved = verification_approved,
    validation_evidence = validation_evidence
  )
}
