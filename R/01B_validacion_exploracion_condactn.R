# ==============================================================================
# Stage 01B — CONDACTN validation and reference distribution
# Methodological source: 2.1 ENEMDU_2008_12_Indicadores_Mercado_Laboral.R,
# historical Point 1B (lines 14–55).
# ==============================================================================


# ==============================================================================
# 1. Validate original CONDACTN and preserve the exact weighted reference
# ==============================================================================
build_condactn_reference <- function(ENEMDU_decile) {

  # Retain the original SAV metadata; do not reconstruct category names.
  condactn_variable_label <- attr(ENEMDU_decile$CONDACTN, "label", exact = TRUE)
  condactn_value_labels <- attr(ENEMDU_decile$CONDACTN, "labels")

  condactn_metadata_present <-
    !is.null(condactn_variable_label) &&
    !is.null(condactn_value_labels) &&
    length(condactn_value_labels) > 0L

  if (!condactn_metadata_present)
    abort_stage_validation(
      stage_id = "01b_condactn_reference",
      check_id = "condactn_metadata_present",
      message = "No se encontraron las etiquetas de variable o de valores de CONDACTN."
    )

  condactn_dictionary <- data.frame(
    CONDACTN = as.numeric(unname(condactn_value_labels)),
    Category = names(condactn_value_labels),
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(CONDACTN)

  observed_condactn <- sort(unique(as.numeric(
    ENEMDU_decile$CONDACTN[!is.na(ENEMDU_decile$CONDACTN)]
  )))

  condactn_codes_valid <- !any(!(observed_condactn %in% 0:9))

  if (!condactn_codes_valid)
    abort_stage_validation(
      stage_id = "01b_condactn_reference",
      check_id = "condactn_codes_valid",
      message = paste(
        "Se encontraron códigos CONDACTN inesperados:",
        paste(observed_condactn[!(observed_condactn %in% 0:9)], collapse = ", ")
      )
    )

  # Exact source distribution for Stage 02B: no rounding and no total row.
  # As in the historical script, missing CONDACTN values are excluded.
  condactn_frequency_reference <- ENEMDU_decile |>
    dplyr::filter(!is.na(CONDACTN)) |>
    dplyr::transmute(CONDACTN = as.numeric(CONDACTN), fexp) |>
    dplyr::group_by(CONDACTN) |>
    dplyr::summarise(Frequency = sum(fexp), .groups = "drop") |>
    dplyr::left_join(condactn_dictionary, by = "CONDACTN") |>
    dplyr::arrange(CONDACTN)

  condactn_observed_codes_labelled <-
    !anyNA(condactn_frequency_reference$Category)

  if (!condactn_observed_codes_labelled)
    abort_stage_validation(
      stage_id = "01b_condactn_reference",
      check_id = "condactn_observed_codes_labelled",
      message = "Existe al menos un código CONDACTN observado sin etiqueta de valor."
    )

  validation_evidence <- data.frame(
    stage_id = rep("01b_condactn_reference", 3L),
    check_id = c(
      "condactn_metadata_present",
      "condactn_codes_valid",
      "condactn_observed_codes_labelled"
    ),
    passed = c(
      condactn_metadata_present,
      condactn_codes_valid,
      condactn_observed_codes_labelled
    ),
    stringsAsFactors = FALSE
  )

  list(
    condactn_frequency_reference = condactn_frequency_reference,
    validation_evidence = validation_evidence
  )
}


# ==============================================================================
# 2. Prepare the historical display table without modifying the reference
# ==============================================================================
build_condactn_frequency_table <- function(condactn_frequency_reference) {

  condactn_valid_expanded <- sum(condactn_frequency_reference$Frequency)

  condactn_frequency_reference |>
    dplyr::mutate(
      Valid_Percentage = Frequency / condactn_valid_expanded * 100,
      Cumulative_Percentage = cumsum(Valid_Percentage)
    ) |>
    dplyr::select(
      CONDACTN, Category, Frequency,
      Valid_Percentage, Cumulative_Percentage
    ) |>
    dplyr::bind_rows(data.frame(
      CONDACTN = NA_real_,
      Category = "Total",
      Frequency = condactn_valid_expanded,
      Valid_Percentage = 100,
      Cumulative_Percentage = NA_real_
    )) |>
    dplyr::mutate(
      CONDACTN = ifelse(is.na(CONDACTN), "", as.character(CONDACTN)),
      Frequency = round(Frequency, 0),
      dplyr::across(
        c(Valid_Percentage, Cumulative_Percentage),
        ~ round(.x, 1)
      )
    )
}


# ==============================================================================
# 3. Run Stage 01B once and collect its reusable outputs and control evidence
# ==============================================================================
build_condactn_stage <- function(ENEMDU_decile) {

  reference_result <- build_condactn_reference(ENEMDU_decile)

  list(
    condactn_frequency_reference =
      reference_result$condactn_frequency_reference,
    condactn_frequency_table =
      build_condactn_frequency_table(
        reference_result$condactn_frequency_reference
      ),
    validation_evidence = reference_result$validation_evidence
  )
}
