# ==============================================================================
# Stage 04A — INGPC deciles: control and audit definitions
# ==============================================================================

build_stage_04a_control_definitions <- function() {

  controls <- build_stage_control_catalog(
    stage_id = "04a_deciles_ingpc",
    groups = list(
      control_group(
        audit_group = "decile_classification_validity",
        check_id = c(
          "decile_no_missing",
          "decile_complete_coverage",
          "decile_codes_valid"
        ),
        control_type = "methodological"
      ),
      control_group(
        audit_group = "decile_population_consistency",
        check_id = "decile_expanded_population_consistent",
        control_type = "cross_stage"
      )
    )
  )

  diagnostics <- build_stage_audit_catalog(
    stage_id = "04a_deciles_ingpc",
    diagnostics = list(
      audit_diagnostic(
        audit_group = "decile_classification_validity",
        audit_label = "Validez de la asignación de deciles de ingreso per cápita",
        audit_description = paste(
          "Asignación de los diez deciles sin valores perdidos,",
          "con cobertura completa y códigos válidos del 1 al 10."
        )
      ),
      audit_diagnostic(
        audit_group = "decile_population_consistency",
        audit_label = "Consistencia de la población del universo analítico",
        audit_description = paste(
          "Población del universo clasificado en deciles consistente con",
          "la referencia del universo con ingreso per cápita válido de la Etapa 01A."
        )
      )
    )
  )

  list(
    controls = controls,
    diagnostics = diagnostics
  )
}
