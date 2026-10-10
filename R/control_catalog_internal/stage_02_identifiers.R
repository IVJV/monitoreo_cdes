# ==============================================================================
# Stage 02 — ENEMDU identifiers: control and audit definitions
# ==============================================================================

build_stage_02_control_definitions <- function() {

  controls <- build_stage_control_catalog(
    stage_id = "02_identifiers",

    groups = list(

      control_group(
        audit_group = "province_reconstruction",

        check_id = c(
          "province_no_missing",
          "province_codes_valid",
          "province_matches_original"
        ),

        control_type_overrides = c(
          province_matches_original = "methodological"
        )
      ),


      control_group(
        audit_group = "natural_region_reconstruction",

        check_id = c(
          "natural_region_no_missing",
          "natural_region_codes_valid",
          "natural_region_matches_original"
        ),

        control_type_overrides = c(
          natural_region_matches_original = "methodological"
        )
      ),


      control_group(
        audit_group = "psu_identifier_validity",

        check_id =
          "psu_structure_valid"
      ),


      control_group(
        audit_group = "study_domain_validity",

        check_id = c(
          "study_domain_assigned",
          "study_domain_codes_valid",
          "study_domain_area_consistent"
        )
      )
    )
  )


  diagnostics <- build_stage_audit_catalog(
    stage_id = "02_identifiers",

    diagnostics = list(

      audit_diagnostic(
        audit_group =
          "province_reconstruction",

        audit_label =
          "Reconstrucción de provincia",

        audit_description =
          paste(
            "Provincia reconstruida sin valores perdidos, con códigos válidos",
            "y coincidencia exacta con la variable original de provincia."
          )
      ),


      audit_diagnostic(
        audit_group =
          "natural_region_reconstruction",

        audit_label =
          "Reconstrucción de región natural",

        audit_description =
          paste(
            "Región natural reconstruida sin valores perdidos, con códigos válidos",
            "y coincidencia exacta con la variable original de región natural."
          )
      ),


      audit_diagnostic(
        audit_group =
          "psu_identifier_validity",

        audit_label =
          "Validez estructural del identificador UPM",

        audit_description =
          paste(
            "Identificador generado para todos los registros y con estructura",
            "numérica válida de 12 dígitos."
          )
      ),


      audit_diagnostic(
        audit_group =
          "study_domain_validity",

        audit_label =
          "Validez de la asignación del dominio de estudio",

        audit_description =
          paste(
            "Todos los registros asignados a uno de los códigos de dominio 01–11,",
            "con consistencia entre la clasificación urbano/rural y el área."
          )
      )
    )
  )


  list(
    controls = controls,
    diagnostics = diagnostics
  )
}
