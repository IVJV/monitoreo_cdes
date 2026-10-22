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
  # Stage A01 — ENEMDU ingestion
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

  stage_A01_audit <- build_stage_audit_diagnostics(
    validation_evidence = ingestion_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "A01_ingestion"
  )


  # ---------------------------------------------------------------------------
  # Stage A02 — ENEMDU identifiers
  # ---------------------------------------------------------------------------
  identifiers_result <- create_identifiers(
    ingestion_result$data,
    return_evidence = TRUE
  )

  identifiers_profile <- build_identifiers_profile(
    identifiers_result$data
  )

  stage_A02_audit <- build_stage_audit_diagnostics(
    validation_evidence = identifiers_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "A02_identifiers"
  )


  # ---------------------------------------------------------------------------
  # Stage A03 — INGPC treatment
  # ---------------------------------------------------------------------------
  ingpc_treatment_result <- create_ingpc_analytical_base(
    identifiers_result$data,
    input_references = input_profile$references,
    return_evidence = TRUE
  )

  stage_A03_audit <- build_stage_audit_diagnostics(
    validation_evidence = ingpc_treatment_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "A03_ingpc_treatment"
  )


  # ---------------------------------------------------------------------------
  # Stage A04 — INGPC deciles
  # ---------------------------------------------------------------------------
  ingpc_deciles_result <- build_ingpc_decile_stage(
    ingpc_treatment_result$data,
    input_references = input_profile$references
  )

  stage_A04_audit <- build_stage_audit_diagnostics(
    validation_evidence = ingpc_deciles_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "A04_deciles_ingpc"
  )


  # ---------------------------------------------------------------------------
  # Stage A05 — ENEMDU complex survey design
  # ---------------------------------------------------------------------------
  d1 <- create_enemdu_survey_design(
    ingpc_deciles_result$data
  )

  # Stage A06 — Survey-adjusted mean of INGPC by decile
  ingpc_survey_mean_result <- build_ingpc_survey_mean_stage(
    d1,
    ingpc_deciles_result$point4_ingpc_mean_reference
  )

  stage_A06_audit <- build_stage_audit_diagnostics(
    validation_evidence = ingpc_survey_mean_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "A06_media_ingpc_por_decil"
  )

  # ---------------------------------------------------------------------------
  # Stage B01 — CONDACTN validation and reference distribution
  # ---------------------------------------------------------------------------
  condactn_result <- build_condactn_stage(
    ingpc_deciles_result$data
  )

  stage_B01_audit <- build_stage_audit_diagnostics(
    validation_evidence = condactn_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "B01_condactn_reference"
  )

  # ---------------------------------------------------------------------------
  # Stage B02 — Population classification and reconciliation with Stage B01
  # ---------------------------------------------------------------------------
  population_result <- build_population_classification_stage(
    ingpc_deciles_result$data,
    condactn_result$condactn_frequency_reference
  )

  stage_B02_audit <- build_stage_audit_diagnostics(
    validation_evidence = population_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "B02_population_classification"
  )

  # ---------------------------------------------------------------------------
  # Stage C01 — Reception of labor-market data and period labels
  # ---------------------------------------------------------------------------
  input_reception_result <- build_input_reception_stage(
    population_result$data,
    period_metadata
  )


  # ---------------------------------------------------------------------------
  # Stage C02 — Population distribution by per capita income decile
  # ---------------------------------------------------------------------------
  population_distribution_result <- build_population_distribution_stage(
    input_reception_result$data,
    population_result$population_classification_table_num,
    input_reception_result$period_excel
  )


  # ---------------------------------------------------------------------------
  # Stage C03 — Income concentration by per capita income decile
  # ---------------------------------------------------------------------------
  income_concentration_result <- build_income_concentration_stage(
    input_reception_result$data,
    population_result$population_classification_table_num,
    input_reception_result$period_excel
  )


  # ---------------------------------------------------------------------------
  # Stage C04 — Independent verification of Tables 1 and 2
  # ---------------------------------------------------------------------------
  table_verification_result <- build_table_output_verification_stage(
    input_reception_result$data,
    population_distribution_result$population_distribution_table_num,
    income_concentration_result$income_concentration_table_num
  )

  stage_C04_audit <- build_stage_audit_diagnostics(
    validation_evidence = table_verification_result$validation_evidence,
    control_catalog = control_catalog,
    audit_diagnostic_catalog = audit_diagnostic_catalog,
    stage_id = "C04_table_output_verification"
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

    stage_A01 = list(
      stage_id = "A01_ingestion",
      period_metadata = period_metadata,
      input_references = input_profile$references,
      database_overview = input_profile$database_overview,
      required_variables_overview = input_profile$required_variables_overview,
      ingpc_universe_summary = input_profile$ingpc_universe_summary,
      validation_evidence = ingestion_result$validation_evidence,
      audit_diagnostics = stage_A01_audit$diagnostics,
      stage_passed = stage_A01_audit$stage_passed
    ),

    stage_A02 = list(
      stage_id = "A02_identifiers",
      identifier_summary = identifiers_profile$identifier_summary,
      domain_distribution = identifiers_profile$domain_distribution,
      validation_evidence = identifiers_result$validation_evidence,
      audit_diagnostics = stage_A02_audit$diagnostics,
      stage_passed = stage_A02_audit$stage_passed
    ),

    stage_A03 = list(
      stage_id = "A03_ingpc_treatment",
      ingpc_universe_comparison =
        ingpc_treatment_result$ingpc_universe_comparison,
      validation_evidence =
        ingpc_treatment_result$validation_evidence,
      audit_diagnostics =
        stage_A03_audit$diagnostics,
      stage_passed =
        stage_A03_audit$stage_passed
    ),

    stage_A04 = list(
      stage_id = "A04_deciles_ingpc",
      decile_frequency_table =
        ingpc_deciles_result$decile_frequency_table,
      decile_report_table =
        ingpc_deciles_result$decile_report_table,
      point4_ingpc_mean_reference =
        ingpc_deciles_result$point4_ingpc_mean_reference,
      validation_evidence =
        ingpc_deciles_result$validation_evidence,
      audit_diagnostics =
        stage_A04_audit$diagnostics,
      stage_passed =
        stage_A04_audit$stage_passed
    ),

    stage_A05 = list(
      stage_id = "A05_diseno_muestral",
      d1 = d1
    ),

    stage_A06 = list(
      stage_id = "A06_media_ingpc_por_decil",
      svy_ingpc_by_decile_table_num = ingpc_survey_mean_result$svy_ingpc_by_decile_table_num,
      svy_ingpc_by_decile_table = ingpc_survey_mean_result$svy_ingpc_by_decile_table,
      mean_validation = ingpc_survey_mean_result$mean_validation,
      mean_tolerance = ingpc_survey_mean_result$mean_tolerance,
      validation_evidence = ingpc_survey_mean_result$validation_evidence,
      audit_diagnostics = stage_A06_audit$diagnostics,
      stage_passed = stage_A06_audit$stage_passed
    ),

    stage_B01 = list(
      stage_id = "B01_condactn_reference",
      condactn_frequency_reference = condactn_result$condactn_frequency_reference,
      condactn_frequency_table = condactn_result$condactn_frequency_table,
      validation_evidence = condactn_result$validation_evidence,
      audit_diagnostics = stage_B01_audit$diagnostics,
      stage_passed = stage_B01_audit$stage_passed
    ),

    stage_B02 = list(
      stage_id = "B02_population_classification",
      population_classification_table_num =
        population_result$population_classification_table_num,
      population_validation = population_result$population_validation,
      validation_evidence = population_result$validation_evidence,
      audit_diagnostics = stage_B02_audit$diagnostics,
      stage_passed = stage_B02_audit$stage_passed
    ),

    stage_C01 = list(
      stage_id = input_reception_result$stage_id,
      period_excel = input_reception_result$period_excel,
      period_title = input_reception_result$period_title,
      period_file = input_reception_result$period_file
    ),

    stage_C02 = list(
      stage_id = population_distribution_result$stage_id,
      population_distribution_table_num =
        population_distribution_result$population_distribution_table_num
    ),

    stage_C03 = list(
      stage_id = income_concentration_result$stage_id,
      income_concentration_table_num =
        income_concentration_result$income_concentration_table_num
    ),

    stage_C04 = list(
      stage_id = table_verification_result$stage_id,
      verification_table_num = table_verification_result$verification_table_num,
      total_verification = table_verification_result$total_verification,
      decile_verification = table_verification_result$decile_verification,
      verification_summary = table_verification_result$verification_summary,
      verification_tolerance = table_verification_result$verification_tolerance,
      total_discrepancies = table_verification_result$total_discrepancies,
      verification_approved = table_verification_result$verification_approved,
      validation_evidence = table_verification_result$validation_evidence,
      audit_diagnostics = stage_C04_audit$diagnostics,
      stage_passed = stage_C04_audit$stage_passed
    )
  )
}
