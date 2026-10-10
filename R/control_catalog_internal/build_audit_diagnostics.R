# ==============================================================================
# Build grouped audit diagnostics from execution evidence
# ==============================================================================

build_stage_audit_diagnostics <- function(
  validation_evidence,
  control_catalog,
  audit_diagnostic_catalog,
  stage_id
) {


  # ============================================================================
  # 4.1 Validate execution evidence structure
  # ============================================================================

  required_evidence_columns <- c(
    "stage_id",
    "check_id",
    "passed"
  )


  missing_evidence_columns <- setdiff(
    required_evidence_columns,
    names(validation_evidence)
  )


  if (length(missing_evidence_columns) > 0L)
    stop(
      paste(
        "Faltan columnas requeridas en la evidencia de validación:",
        paste(
          missing_evidence_columns,
          collapse = ", "
        )
      )
    )


  # ============================================================================
  # 4.2 Validate atomic control catalogue structure
  # ============================================================================

  required_control_catalog_columns <- c(
    "stage_id",
    "check_id",
    "control_type",
    "audit_group"
  )


  missing_control_catalog_columns <- setdiff(
    required_control_catalog_columns,
    names(control_catalog)
  )


  if (length(missing_control_catalog_columns) > 0L)
    stop(
      paste(
        "Faltan columnas requeridas en el catálogo de controles:",
        paste(
          missing_control_catalog_columns,
          collapse = ", "
        )
      )
    )


  # ============================================================================
  # 4.3 Validate audit diagnostic catalogue structure
  # ============================================================================

  required_diagnostic_catalog_columns <- c(
    "stage_id",
    "audit_group",
    "audit_label",
    "audit_description",
    "display_in_audit"
  )


  missing_diagnostic_catalog_columns <- setdiff(
    required_diagnostic_catalog_columns,
    names(audit_diagnostic_catalog)
  )


  if (length(missing_diagnostic_catalog_columns) > 0L)
    stop(
      paste(
        "Faltan columnas requeridas en el catálogo de diagnósticos:",
        paste(
          missing_diagnostic_catalog_columns,
          collapse = ", "
        )
      )
    )


  # ============================================================================
  # 4.4 Select current stage
  # ============================================================================

  stage_evidence <- validation_evidence[
    validation_evidence$stage_id == stage_id,
    ,
    drop = FALSE
  ]


  stage_control_catalog <- control_catalog[
    control_catalog$stage_id == stage_id,
    ,
    drop = FALSE
  ]


  stage_diagnostic_catalog <- audit_diagnostic_catalog[
    audit_diagnostic_catalog$stage_id == stage_id,
    ,
    drop = FALSE
  ]


  if (nrow(stage_evidence) == 0L)
    stop(
      paste(
        "No existe evidencia de validación para la etapa:",
        stage_id
      )
    )


  if (nrow(stage_control_catalog) == 0L)
    stop(
      paste(
        "No existen controles catalogados para la etapa:",
        stage_id
      )
    )


  if (nrow(stage_diagnostic_catalog) == 0L)
    stop(
      paste(
        "No existen diagnósticos catalogados para la etapa:",
        stage_id
      )
    )


  # ============================================================================
  # 4.5 Validate catalogue uniqueness
  # ============================================================================

  duplicated_check_ids <- duplicated(
    stage_control_catalog$check_id
  )


  if (any(duplicated_check_ids))
    stop(
      paste(
        "Existen check_id duplicados en el catálogo de controles:",
        paste(
          unique(
            stage_control_catalog$check_id[
              duplicated_check_ids
            ]
          ),
          collapse = ", "
        )
      )
    )


  duplicated_audit_groups <- duplicated(
    stage_diagnostic_catalog$audit_group
  )


  if (any(duplicated_audit_groups))
    stop(
      paste(
        "Existen audit_group duplicados en el catálogo de diagnósticos:",
        paste(
          unique(
            stage_diagnostic_catalog$audit_group[
              duplicated_audit_groups
            ]
          ),
          collapse = ", "
        )
      )
    )


  # ============================================================================
  # 4.6 Validate execution evidence against atomic catalogue
  # ============================================================================

  duplicated_evidence_ids <- duplicated(
    stage_evidence$check_id
  )


  if (any(duplicated_evidence_ids))
    stop(
      paste(
        "Existen check_id duplicados en la evidencia de validación:",
        paste(
          unique(
            stage_evidence$check_id[
              duplicated_evidence_ids
            ]
          ),
          collapse = ", "
        )
      )
    )


  uncatalogued_checks <- setdiff(
    stage_evidence$check_id,
    stage_control_catalog$check_id
  )


  if (length(uncatalogued_checks) > 0L)
    stop(
      paste(
        "Existen controles ejecutados sin entrada en el catálogo:",
        paste(
          uncatalogued_checks,
          collapse = ", "
        )
      )
    )


  missing_control_evidence <- setdiff(
    stage_control_catalog$check_id,
    stage_evidence$check_id
  )


  if (length(missing_control_evidence) > 0L)
    stop(
      paste(
        "Falta evidencia para controles catalogados:",
        paste(
          missing_control_evidence,
          collapse = ", "
        )
      )
    )


  # ============================================================================
  # 4.7 Validate links between controls and audit diagnostics
  # ============================================================================

  controls_without_diagnostic <- setdiff(
    unique(
      stage_control_catalog$audit_group
    ),
    stage_diagnostic_catalog$audit_group
  )


  if (length(controls_without_diagnostic) > 0L)
    stop(
      paste(
        "Existen grupos de controles sin diagnóstico de auditoría definido:",
        paste(
          controls_without_diagnostic,
          collapse = ", "
        )
      )
    )


  diagnostics_without_controls <- setdiff(
    stage_diagnostic_catalog$audit_group,
    unique(
      stage_control_catalog$audit_group
    )
  )


  if (length(diagnostics_without_controls) > 0L)
    stop(
      paste(
        "Existen diagnósticos de auditoría sin controles asociados:",
        paste(
          diagnostics_without_controls,
          collapse = ", "
        )
      )
    )


  # ============================================================================
  # 4.8 Select diagnostics shown in audit report
  # ============================================================================

  visible_diagnostic_catalog <- stage_diagnostic_catalog[
    stage_diagnostic_catalog$display_in_audit,
    ,
    drop = FALSE
  ]


  visible_control_catalog <- stage_control_catalog[
    stage_control_catalog$audit_group %in%
      visible_diagnostic_catalog$audit_group,
    ,
    drop = FALSE
  ]


  # ============================================================================
  # 4.9 Attach execution results to atomic controls
  # ============================================================================

  evidence_order <- match(
    visible_control_catalog$check_id,
    stage_evidence$check_id
  )


  visible_control_catalog$passed <-
    stage_evidence$passed[
      evidence_order
    ]


  # ============================================================================
  # 4.10 Build visible grouped diagnostics
  # ============================================================================

  if (nrow(visible_diagnostic_catalog) == 0L) {

    diagnostics <- data.frame(
      Control = character(0),
      "Verificaciones incluidas" = character(0),
      Resultado = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

  } else {

    diagnostics <- do.call(
      rbind,
      lapply(
        seq_len(
          nrow(
            visible_diagnostic_catalog
          )
        ),
        function(i) {

          diagnostic_row <-
            visible_diagnostic_catalog[
              i,
              ,
              drop = FALSE
            ]


          group_rows <-
            visible_control_catalog[
              visible_control_catalog$audit_group ==
                diagnostic_row$audit_group,
              ,
              drop = FALSE
            ]


          data.frame(
            Control =
              diagnostic_row$audit_label,

            "Verificaciones incluidas" =
              diagnostic_row$audit_description,

            Resultado =
              if (all(group_rows$passed)) {
                "OK"
              } else {
                "NO SUPERADO"
              },

            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        }
      )
    )


    rownames(diagnostics) <- NULL
  }


  # ============================================================================
  # 4.11 Return grouped diagnostics and overall stage result
  # ============================================================================

  list(
    diagnostics = diagnostics,

    stage_passed = all(
      stage_evidence$passed
    )
  )
}
