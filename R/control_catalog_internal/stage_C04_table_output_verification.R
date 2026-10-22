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
        audit_label = "Tabla 1 — Totales de población",
        audit_description = paste(
          "Las poblaciones expandidas totales de los 16 indicadores",
          "coinciden con la referencia reconstruida desde CONDACTN,",
          "con una tolerancia absoluta de 1e-6."
        )
      ),
      audit_diagnostic(
        audit_group = "table2_total_consistency",
        audit_label = "Tabla 2 — Totales de ingreso",
        audit_description = paste(
          "Los ingresos totales de los 16 indicadores, expresados en",
          "millones de USD, coinciden con la referencia reconstruida",
          "desde CONDACTN, con una tolerancia absoluta de 1e-6."
        )
      ),
      audit_diagnostic(
        audit_group = "table1_decile_consistency",
        audit_label = "Tabla 1 — Población y porcentajes por decil",
        audit_description = paste(
          "Las poblaciones expandidas y sus porcentajes en los 160",
          "cruces de indicador y decil coinciden con la referencia",
          "reconstruida desde CONDACTN, con tolerancia absoluta de 1e-6."
        )
      ),
      audit_diagnostic(
        audit_group = "table2_decile_consistency",
        audit_label = "Tabla 2 — Ingreso y participación por decil",
        audit_description = paste(
          "Los ingresos en millones de USD y las participaciones en los",
          "160 cruces de indicador y decil coinciden con la referencia",
          "reconstruida desde CONDACTN, con tolerancia absoluta de 1e-6."
        )
      )
    )
  )

  list(controls = controls, diagnostics = diagnostics)
}
