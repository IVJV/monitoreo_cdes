# ==============================================================================
# Stage 01 — ENEMDU ingestion: control and audit definitions
# ==============================================================================

build_stage_01_control_definitions <- function() {

  controls <- build_stage_control_catalog(
    stage_id = "01_ingestion",

    groups = list(

      control_group(
        audit_group = "required_data_structure",

        check_id = c(
          "required_variables_present",
          "required_numeric_types_valid"
        )
      ),


      control_group(
        audit_group = "expansion_factor_validity",

        check_id = c(
          "fexp_no_missing",
          "fexp_finite",
          "fexp_positive"
        )
      )
    )
  )


  diagnostics <- build_stage_audit_catalog(
    stage_id = "01_ingestion",

    diagnostics = list(

      audit_diagnostic(
        audit_group =
          "required_data_structure",

        audit_label =
          "Estructura de datos requerida",

        audit_description =
          paste(
            "Variables requeridas presentes y variables necesarias",
            "para cálculo con tipos compatibles."
          )
      ),


      audit_diagnostic(
        audit_group =
          "expansion_factor_validity",

        audit_label =
          "Validez del factor de expansión fexp",

        audit_description =
          paste(
            "Sin valores perdidos, valores finitos y todos",
            "estrictamente positivos."
          )
      )
    )
  )


  list(
    controls = controls,
    diagnostics = diagnostics
  )
}
