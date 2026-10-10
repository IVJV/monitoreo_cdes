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