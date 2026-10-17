# ==============================================================================
# Reporting context
# ==============================================================================
# Organizes and exposes reusable objects produced by each processing stage.
# Objects remain associated with their stage of origin for traceability, but any
# reporting module may consume them when required.
#
# This file does not reproduce analytical methodology, rebuild controls, or
# contain presentation logic.


# ==============================================================================
# 1. Build reporting context
# ==============================================================================

build_report_context <- function(input_file) {

  # ---------------------------------------------------------------------------
  # Stage 01A — ENEMDU ingestion
  # ---------------------------------------------------------------------------

  period_metadata <- get_period_metadata(input_file)

  ingestion_result <- read_validate_enemdu(
    input_file,
    return_evidence = TRUE
  )

  input_profile <- build_input_profile(
    ingestion_result$data,
    period_metadata
  )

  stage_01a_audit <- build_stage_audit_diagnostics(
    validation_evidence = ingestion_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "01a_ingestion"
  )


  # ---------------------------------------------------------------------------
  # Stage 02A — ENEMDU identifiers
  # ---------------------------------------------------------------------------

  identifiers_result <- create_identifiers(
    ingestion_result$data,
    return_evidence = TRUE
  )

  identifiers_profile <- build_identifiers_profile(
    identifiers_result$data
  )

  stage_02a_audit <- build_stage_audit_diagnostics(
    validation_evidence = identifiers_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "02a_identifiers"
  )


  # ---------------------------------------------------------------------------
  # Stage 03A — INGPC treatment
  # ---------------------------------------------------------------------------

  ingpc_treatment_result <- create_ingpc_analytical_base(
    identifiers_result$data,
    input_references = input_profile$references,
    return_evidence = TRUE
  )

  stage_03a_audit <- build_stage_audit_diagnostics(
    validation_evidence = ingpc_treatment_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "03a_ingpc_treatment"
  )


  # ---------------------------------------------------------------------------
  # Stage 04A — INGPC deciles
  # ---------------------------------------------------------------------------

  ingpc_deciles_result <- build_ingpc_decile_stage(
    ingpc_treatment_result$data,
    input_references = input_profile$references
  )

  stage_04a_audit <- build_stage_audit_diagnostics(
    validation_evidence = ingpc_deciles_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "04a_deciles_ingpc"
  )


  # ---------------------------------------------------------------------------
  # Reporting context
  # ---------------------------------------------------------------------------
  # The context preserves stage of origin only for traceability. It does not
  # restrict where an object may be reused in technical or audit reporting.
  #
  # Intermediate working datasets remain local to this function. The reporting
  # context exposes only reusable reporting, reference, evidence, and diagnostic
  # objects already produced by the corresponding processing stages.

  list(

    stage_01a = list(
      stage_id = "01a_ingestion",
      period_metadata = period_metadata,
      input_references = input_profile$references,
      database_overview = input_profile$database_overview,
      required_variables_overview = input_profile$required_variables_overview,
      ingpc_universe_summary = input_profile$ingpc_universe_summary,
      validation_evidence = ingestion_result$validation_evidence,
      audit_diagnostics = stage_01a_audit$diagnostics,
      stage_passed = stage_01a_audit$stage_passed
    ),

    stage_02a = list(
      stage_id = "02a_identifiers",
      identifier_summary = identifiers_profile$identifier_summary,
      domain_distribution = identifiers_profile$domain_distribution,
      validation_evidence = identifiers_result$validation_evidence,
      audit_diagnostics = stage_02a_audit$diagnostics,
      stage_passed = stage_02a_audit$stage_passed
    ),

    stage_03a = list(
      stage_id = "03a_ingpc_treatment",
      ingpc_universe_comparison =
        ingpc_treatment_result$ingpc_universe_comparison,
      validation_evidence =
        ingpc_treatment_result$validation_evidence,
      audit_diagnostics =
        stage_03a_audit$diagnostics,
      stage_passed =
        stage_03a_audit$stage_passed
    ),

    stage_04a = list(
      stage_id = "04a_deciles_ingpc",
      decile_frequency_table =
        ingpc_deciles_result$decile_frequency_table,
      decile_report_table =
        ingpc_deciles_result$decile_report_table,
      point4_ingpc_mean_reference =
        ingpc_deciles_result$point4_ingpc_mean_reference,
      validation_evidence =
        ingpc_deciles_result$validation_evidence,
      audit_diagnostics =
        stage_04a_audit$diagnostics,
      stage_passed =
        stage_04a_audit$stage_passed
    )
  )
}
