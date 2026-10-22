# ==============================================================================
# Stage A06 — Survey-adjusted INGPC mean: control and audit definitions
# ==============================================================================
build_stage_A06_control_definitions <- function() {

  controls <- build_stage_control_catalog(
    stage_id = "A06_media_ingpc_por_decil",
    groups = list(
      control_group(
        audit_group = "survey_mean_consistency",
        check_id = "survey_mean_consistency",
        control_type = "cross_stage"
      )
    )
  )

  diagnostics <- build_stage_audit_catalog(
    stage_id = "A06_media_ingpc_por_decil",
    diagnostics = list(
      audit_diagnostic(
        audit_group = "survey_mean_consistency",
        audit_label = "Consistencia del ingreso medio per cápita por decil",
        audit_description = paste(
          "Las medias estimadas con el diseño muestral se comparan con",
          "las medias ponderadas exactas conservadas en la Etapa A04."
        )
      )
    )
  )

  list(controls = controls, diagnostics = diagnostics)
}
