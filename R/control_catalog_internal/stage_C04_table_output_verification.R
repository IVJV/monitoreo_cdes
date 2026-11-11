# ==============================================================================
# Stage C04 — Independent verification of Tables 1 and 2 before publication
# ==============================================================================
build_stage_C04_control_definitions <- function() {

  controls <- build_stage_control_catalog(
    stage_id = "C04_table_output_verification",
    groups = list(
      control_group(
        audit_group = "table1_total_consistency",
        check_id = "table1_total_consistency",
        control_type = "cross_stage"
      ),
      control_group(
        audit_group = "table2_total_consistency",
        check_id = "table2_total_consistency",
        control_type = "cross_stage"
      ),
      control_group(
        audit_group = "table1_decile_consistency",
        check_id = "table1_decile_consistency",
        control_type = "cross_stage"
      ),
      control_group(
        audit_group = "table2_decile_consistency",
        check_id = "table2_decile_consistency",
        control_type = "cross_stage"
      )
    )
  )

  diagnostics <- build_stage_audit_catalog(
    stage_id = "C04_table_output_verification",
    diagnostics = list(
      audit_diagnostic(
        audit_group = "table1_total_consistency",
        audit_label = "Distribución poblacional — Totales de población",
        audit_description =
          "Comparación de los totales de población de los 16 indicadores."
      ),
      audit_diagnostic(
        audit_group = "table2_total_consistency",
        audit_label = "Concentración del ingreso per cápita — Totales",
        audit_description =
          "Comparación de los totales derivados del ingreso per cápita de los 16 indicadores, expresados en millones de USD."
      ),
      audit_diagnostic(
        audit_group = "table1_decile_consistency",
        audit_label = "Distribución poblacional — Población y porcentajes por decil",
        audit_description =
          "Comparación de la población y los porcentajes en los 160 cruces de indicador y decil."
      ),
      audit_diagnostic(
        audit_group = "table2_decile_consistency",
        audit_label = "Concentración del ingreso per cápita — Valores y participación por decil",
        audit_description =
          "Comparación del ingreso per cápita y su participación porcentual en los 160 cruces de indicador y decil."
      )
    )
  )

  list(controls = controls, diagnostics = diagnostics)
}
