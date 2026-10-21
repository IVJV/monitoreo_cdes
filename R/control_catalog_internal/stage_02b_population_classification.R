# ==============================================================================
# Stage 02B — Population classification: control and audit definitions
# ==============================================================================
build_stage_02b_control_definitions <- function() {

  controls <- build_stage_control_catalog(
    stage_id = "02b_population_classification",
    groups = list(
      control_group(
        audit_group = "population_classification_consistency",
        check_id = "population_classification_consistency",
        control_type = "cross_stage"
      )
    )
  )

  diagnostics <- build_stage_audit_catalog(
    stage_id = "02b_population_classification",
    diagnostics = list(
      audit_diagnostic(
        audit_group = "population_classification_consistency",
        audit_label = "Consistencia de la clasificación de la población",
        audit_description = paste(
          "Las frecuencias expandidas de los 16 indicadores de población",
          "coinciden con la referencia de condición de actividad de la Etapa 01B,",
          "con una tolerancia absoluta de 1e-8."
        )
      )
    )
  )

  list(controls = controls, diagnostics = diagnostics)
}
