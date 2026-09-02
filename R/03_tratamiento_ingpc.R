# ==============================================================================
# ENEMDU INGPC treatment
# ==============================================================================


# ==============================================================================
# 1. Create INGPC analytical base
# ==============================================================================

create_ingpc_analytical_base <- function(ENEMDU_base, input_references) {

  # Preserve the analytical universe defined in the original SPSS methodology:
  # SELECT IF (NOT SYSMIS(ingpc))
  idx_ingpc_valid <- !is.na(ENEMDU_base$ingpc)

  # Create a separate analytical database; ENEMDU_base remains unchanged
  ENEMDU_decile <- ENEMDU_base[idx_ingpc_valid, , drop = FALSE]

  # Validate sample counts against Stage 01 references
  if (
    sum(!idx_ingpc_valid) != input_references$input_ingpc_missing_n ||
    nrow(ENEMDU_decile) != input_references$input_ingpc_valid_n
  )
    stop(
      "El tratamiento de INGPC no coincide con los conteos muestrales registrados en la Etapa 01."
    )

  # Validate expanded population against Stage 01 references
  if (
    !isTRUE(all.equal(
      sum(ENEMDU_base$fexp[!idx_ingpc_valid]),
      input_references$input_ingpc_missing_expanded,
      tolerance = 1e-8
    )) ||
    !isTRUE(all.equal(
      sum(ENEMDU_decile$fexp),
      input_references$input_ingpc_valid_expanded,
      tolerance = 1e-8
    ))
  )
    stop(
      "El tratamiento de INGPC no coincide con las poblaciones expandidas registradas en la Etapa 01."
    )

  ENEMDU_decile
}


# ==============================================================================
# 2. Build INGPC treatment diagnostic profile
# ==============================================================================

build_ingpc_treatment_profile <- function(ENEMDU_base, ENEMDU_decile) {

  idx_ingpc_valid <- !is.na(ENEMDU_base$ingpc)

  ingpc_treatment_summary <- data.frame(
    Step = c(
      "Original database",
      "Excluded records: missing ingpc",
      "Decile calculation database"
    ),
    Sample_N = c(
      nrow(ENEMDU_base),
      sum(!idx_ingpc_valid),
      nrow(ENEMDU_decile)
    ),
    Expanded_N = c(
      sum(ENEMDU_base$fexp),
      sum(ENEMDU_base$fexp[!idx_ingpc_valid]),
      sum(ENEMDU_decile$fexp)
    ),
    stringsAsFactors = FALSE
  )

  list(
    ingpc_treatment_summary = ingpc_treatment_summary
  )
}