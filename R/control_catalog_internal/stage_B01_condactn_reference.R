# ==============================================================================
# Stage B01 — CONDACTN validation: control and audit definitions
# ==============================================================================
build_stage_B01_control_definitions <- function() {

  controls <- build_stage_control_catalog(
    stage_id = "B01_condactn_reference",
    groups = list(
      control_group(
        audit_group = "condactn_reference_validity",
        check_id = c(
          "condactn_metadata_present",
          "condactn_codes_valid",
          "condactn_observed_codes_labelled"
        ),
        control_type = "integrity"
      )
    )
  )

  diagnostics <- build_stage_audit_catalog(
    stage_id = "B01_condactn_reference",
    diagnostics = list(
      audit_diagnostic(
        audit_group = "condactn_reference_validity",
        audit_label = "Validación de códigos y etiquetas de condición de actividad",
        audit_description = paste(
          "Etiquetas originales disponibles, códigos observados entre 0 y 9",
          "y correspondencia de todos los códigos observados con sus etiquetas."
        )
      )
    )
  )

  list(controls = controls, diagnostics = diagnostics)
}
