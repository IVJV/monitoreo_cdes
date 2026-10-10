# ==============================================================================
# Stage 03 — INGPC treatment: control and audit definitions
# ==============================================================================

build_stage_03_control_definitions <- function() {

  controls <- build_stage_control_catalog(
    stage_id = "03_ingpc_treatment",

    groups = list(

      control_group(
        audit_group =
          "ingpc_analytical_universe_consistency",

        check_id = c(
          "ingpc_selection_rule_valid",
          "ingpc_sample_counts_consistent",
          "ingpc_expanded_population_consistent"
        ),

        control_type = "cross_stage",

        control_type_overrides = c(
          ingpc_selection_rule_valid = "methodological"
        )
      )
    )
  )


  diagnostics <- build_stage_audit_catalog(
    stage_id = "03_ingpc_treatment",

    diagnostics = list(

      audit_diagnostic(
        audit_group =
          "ingpc_analytical_universe_consistency",

        audit_label =
          "Construcción y consistencia del universo analítico de `ingpc`",

        audit_description =
          paste(
            "Regla metodológica de selección aplicada correctamente, con conteos",
            "de muestra y población consistentes con las referencias preservadas",
            "en la Etapa 01."
          )
      )
    )
  )


  list(
    controls = controls,
    diagnostics = diagnostics
  )
}
