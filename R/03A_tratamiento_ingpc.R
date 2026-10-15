# ==============================================================================
# ENEMDU INGPC treatment
# ==============================================================================


# ==============================================================================
# 1. Create INGPC analytical base
# ==============================================================================

create_ingpc_analytical_base <- function(
  ENEMDU_base,
  input_references,
  return_evidence = FALSE
) {

  # Preserve the analytical universe defined in the original SPSS methodology:
  # SELECT IF (NOT SYSMIS(ingpc))
  idx_ingpc_valid <- !is.na(ENEMDU_base$ingpc)

  # Create a separate analytical database; ENEMDU_base remains unchanged
  ENEMDU_decile <- ENEMDU_base[
    idx_ingpc_valid,
    ,
    drop = FALSE
  ]


  # Values actually produced by Stage 03A.
  # They are used below to validate the transformation against the canonical
  # references preserved in Stage 01A.
  observed_ingpc_missing_n <-
    sum(!idx_ingpc_valid)

  observed_ingpc_valid_n <-
    nrow(ENEMDU_decile)

  observed_ingpc_missing_expanded <-
    sum(ENEMDU_base$fexp[!idx_ingpc_valid])

  observed_ingpc_valid_expanded <-
    sum(ENEMDU_decile$fexp)


  # Validate the methodological selection rule directly
  ingpc_selection_rule_valid <-
    observed_ingpc_valid_n == sum(idx_ingpc_valid) &&
    !anyNA(ENEMDU_decile$ingpc)

  if (!ingpc_selection_rule_valid)
    abort_stage_validation(
      stage_id = "03a_ingpc_treatment",
      check_id = "ingpc_selection_rule_valid",
      message = paste(
        "La selección del universo analítico de ingpc no reproduce",
        "la regla metodológica SELECT IF (NOT SYSMIS(ingpc))."
      )
    )


  # Validate sample counts against Stage 01A references
  ingpc_sample_counts_consistent <-
    observed_ingpc_missing_n ==
      input_references$input_ingpc_missing_n &&
    observed_ingpc_valid_n ==
      input_references$input_ingpc_valid_n

  if (!ingpc_sample_counts_consistent)
    abort_stage_validation(
      stage_id = "03a_ingpc_treatment",
      check_id = "ingpc_sample_counts_consistent",
      message = "El tratamiento de INGPC no coincide con los conteos muestrales registrados en la Etapa 01A."
    )


  # Validate expanded population against Stage 01A references
  ingpc_expanded_population_consistent <-
    isTRUE(all.equal(
      observed_ingpc_missing_expanded,
      input_references$input_ingpc_missing_expanded,
      tolerance = 1e-8
    )) &&
    isTRUE(all.equal(
      observed_ingpc_valid_expanded,
      input_references$input_ingpc_valid_expanded,
      tolerance = 1e-8
    ))

  if (!ingpc_expanded_population_consistent)
    abort_stage_validation(
      stage_id = "03a_ingpc_treatment",
      check_id = "ingpc_expanded_population_consistent",
      message = "El tratamiento de INGPC no coincide con las poblaciones expandidas registradas en la Etapa 01A."
    )


  # Cross-stage evidence.
  #
  # This is not a second analytical-universe summary. The canonical universe
  # summary remains the object produced in Stage 01A. This object records only
  # the Stage 01A reference values and the values actually observed after the
  # Stage 03A transformation so that their equivalence can be audited directly.
  ingpc_universe_comparison <- data.frame(
    universe_id = c(
      "ingpc_missing",
      "ingpc_valid"
    ),

    Reference_Sample_N = c(
      input_references$input_ingpc_missing_n,
      input_references$input_ingpc_valid_n
    ),

    Observed_Sample_N = c(
      observed_ingpc_missing_n,
      observed_ingpc_valid_n
    ),

    Reference_Expanded_N = c(
      input_references$input_ingpc_missing_expanded,
      input_references$input_ingpc_valid_expanded
    ),

    Observed_Expanded_N = c(
      observed_ingpc_missing_expanded,
      observed_ingpc_valid_expanded
    ),

    stringsAsFactors = FALSE
  )


  # Structured evidence of substantive controls actually executed above.
  #
  # check_id is the stable internal identifier used to link execution evidence
  # with the control catalogue and, subsequently, the reporting layer.
  #
  # Classification, audit grouping, visible labels, and descriptions belong to
  # the control catalogue rather than to the execution evidence itself.
  validation_evidence <- data.frame(
    stage_id = rep(
      "03a_ingpc_treatment",
      3
    ),

    check_id = c(
      "ingpc_selection_rule_valid",
      "ingpc_sample_counts_consistent",
      "ingpc_expanded_population_consistent"
    ),

    passed = c(
      ingpc_selection_rule_valid,
      ingpc_sample_counts_consistent,
      ingpc_expanded_population_consistent
    ),

    stringsAsFactors = FALSE
  )


  # Preserve the existing function contract by default:
  # create_ingpc_analytical_base(...) continues returning only ENEMDU_decile.
  #
  # Structured evidence is exposed explicitly only when required by the
  # pipeline or reporting layer; ENEMDU_decile itself is never modified to
  # transport audit metadata.
  if (isTRUE(return_evidence)) {
    return(list(
      data = ENEMDU_decile,
      validation_evidence = validation_evidence,
      ingpc_universe_comparison = ingpc_universe_comparison
    ))
  }

  ENEMDU_decile
}
