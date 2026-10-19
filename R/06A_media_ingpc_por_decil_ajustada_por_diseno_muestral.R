# ==============================================================================
# Survey-adjusted mean of INGPC by decile
# ==============================================================================


# ==============================================================================
# 1. Estimate INGPC by decile
# ==============================================================================

estimate_ingpc_by_decile <- function(d1) {

  # Estimate mean per capita income by decile using the complex survey design
  svy_ingpc_by_decile <- d1 |>
    dplyr::group_by(ningpc) |>
    dplyr::summarise(
      Mean = srvyr::survey_mean(
        ingpc,
        vartype = c("se", "ci"),
        level = 0.95,
        na.rm = TRUE
      )
    )

  # Keep numeric estimates for validation and subsequent calculations
  svy_ingpc_by_decile_table_num <- svy_ingpc_by_decile |>
    dplyr::transmute(
      Decile = ningpc,
      Mean = Mean,
      Linearized_Std_Err = Mean_se,
      CI_95_Lower = Mean_low,
      CI_95_Upper = Mean_upp
    )

  svy_ingpc_by_decile_table_num
}


# ==============================================================================
# 2. Validate survey-adjusted INGPC means
# ==============================================================================

validate_ingpc_decile_means <- function(
  svy_ingpc_by_decile_table_num,
  point4_ingpc_mean_reference
) {

  # Validate survey means against the exact unrounded weighted means from Stage 04
  mean_tolerance <- 1e-8

  mean_validation <- svy_ingpc_by_decile_table_num |>
    dplyr::select(Decile, Mean) |>
    dplyr::left_join(
      point4_ingpc_mean_reference |>
        dplyr::rename(Mean_reference = Mean),
      by = "Decile"
    ) |>
    dplyr::mutate(
      Difference = abs(Mean - Mean_reference)
    )

  # Every survey estimate must have its corresponding Stage 04 reference mean
  if (anyNA(mean_validation$Mean_reference))
    stop(
      "La validación de medias de la encuesta falló: uno o más deciles no tienen una media de referencia de la Etapa 04."
    )

  # Reject missing and non-finite means, as required by the reference script.
  if (anyNA(mean_validation$Mean) || any(!is.finite(mean_validation$Mean)))
    stop("La validación de medias falló: existen estimaciones faltantes o no finitas.")

  # Numerical comparison uses exact unrounded values
  if (any(mean_validation$Difference > mean_tolerance))
    stop(paste(
      "La validación de medias de la encuesta falló para los deciles:",
      paste(
        mean_validation$Decile[
          mean_validation$Difference > mean_tolerance
        ],
        collapse = ", "
      )
    ))

  mean_validation
}

# ==============================================================================
# 3. Execute Stage 06A once and preserve methodological outputs
# ==============================================================================

build_ingpc_survey_mean_stage <- function(d1, point4_ingpc_mean_reference) {

  svy_ingpc_by_decile_table_num <- estimate_ingpc_by_decile(d1)
  mean_validation <- validate_ingpc_decile_means(
    svy_ingpc_by_decile_table_num,
    point4_ingpc_mean_reference
  )

  # Original 6A output: a display-only table with six decimal places.
  svy_ingpc_by_decile_table <- svy_ingpc_by_decile_table_num |>
    dplyr::mutate(
      dplyr::across(
        c(Mean, Linearized_Std_Err, CI_95_Lower, CI_95_Upper),
        ~ sprintf("%.6f", .x)
      )
    )

  list(
    svy_ingpc_by_decile_table_num = svy_ingpc_by_decile_table_num,
    svy_ingpc_by_decile_table = svy_ingpc_by_decile_table,
    mean_validation = mean_validation,
    validation_evidence = data.frame(
      stage_id = "06a_media_ingpc_por_decil",
      check_id = "survey_mean_consistency",
      passed = TRUE,
      stringsAsFactors = FALSE
    )
  )
}
