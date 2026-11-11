# ==============================================================================
# Stage B02 — Population classification: control and audit definitions
# ==============================================================================
build_stage_B02_control_definitions <- function() {

  controls <- build_stage_control_catalog(
    stage_id = "B02_population_classification",
    groups = list(
      control_group(
        audit_group = "population_classification_consistency",
        check_id = "population_classification_consistency",
        control_type = "cross_stage"
      )
    )
  )

  diagnostics <- build_stage_audit_catalog(
    stage_id = "B02_population_classification",
    diagnostics = list(
      audit_diagnostic(
        audit_group = "population_classification_consistency",
        audit_label = "Consistencia de la clasificación de la población",
        audit_description =
          "Comparación de las cifras de población de los 16 indicadores con las obtenidas a partir de CONDACTN en la etapa B01."
      )
    )
  )

  list(controls = controls, diagnostics = diagnostics)
}
