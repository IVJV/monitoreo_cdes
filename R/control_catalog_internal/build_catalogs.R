# ==============================================================================
# Control catalogue and audit diagnostics
# ==============================================================================


# ==============================================================================
# 1. Catalogue construction helpers
# ==============================================================================


# ------------------------------------------------------------------------------
# 1.1 Define one atomic-control group
# ------------------------------------------------------------------------------
# A control group represents one audit diagnostic composed of one or more
# atomic checks.
#
# audit_group is declared once.
# control_type is declared once as the default for the whole group.
#
# control_type_overrides is used only when specific checks require a different
# classification from the group default.

control_group <- function(
  audit_group,
  check_id,
  control_type = "integrity",
  control_type_overrides = NULL
) {

  if (length(audit_group) != 1L)
    stop(
      "audit_group debe contener exactamente un identificador."
    )


  if (length(check_id) == 0L)
    stop(
      "Cada grupo de controles debe contener al menos un check_id."
    )


  if (anyDuplicated(check_id))
    stop(
      paste(
        "Existen check_id duplicados dentro del grupo:",
        audit_group
      )
    )


  control_types <- rep(
    control_type,
    length(check_id)
  )


  if (!is.null(control_type_overrides)) {

    if (
      is.null(names(control_type_overrides)) ||
      any(names(control_type_overrides) == "")
    )
      stop(
        paste(
          "control_type_overrides debe utilizar check_id como nombres",
          "dentro del grupo:",
          audit_group
        )
      )


    unknown_override_checks <- setdiff(
      names(control_type_overrides),
      check_id
    )


    if (length(unknown_override_checks) > 0L)
      stop(
        paste(
          "Existen overrides para check_id no definidos en el grupo",
          audit_group,
          ":",
          paste(
            unknown_override_checks,
            collapse = ", "
          )
        )
      )


    override_positions <- match(
      names(control_type_overrides),
      check_id
    )


    control_types[override_positions] <-
      unname(control_type_overrides)
  }


  data.frame(
    check_id = check_id,
    control_type = control_types,
    audit_group = audit_group,
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 1.2 Build atomic-control catalogue for one stage
# ------------------------------------------------------------------------------

build_stage_control_catalog <- function(
  stage_id,
  groups
) {

  if (length(stage_id) != 1L)
    stop(
      "stage_id debe contener exactamente un identificador."
    )


  if (length(groups) == 0L)
    stop(
      paste(
        "La etapa no contiene grupos de controles:",
        stage_id
      )
    )


  stage_catalog <- do.call(
    rbind,
    groups
  )


  stage_catalog$stage_id <- stage_id


  stage_catalog <- stage_catalog[
    ,
    c(
      "stage_id",
      "check_id",
      "control_type",
      "audit_group"
    ),
    drop = FALSE
  ]


  rownames(stage_catalog) <- NULL

  stage_catalog
}


# ------------------------------------------------------------------------------
# 1.3 Define one audit diagnostic
# ------------------------------------------------------------------------------

audit_diagnostic <- function(
  audit_group,
  audit_label,
  audit_description,
  display_in_audit = TRUE
) {

  if (length(audit_group) != 1L)
    stop(
      "audit_group debe contener exactamente un identificador."
    )


  if (length(audit_label) != 1L)
    stop(
      paste(
        "audit_label debe contener exactamente un valor para:",
        audit_group
      )
    )


  if (length(audit_description) != 1L)
    stop(
      paste(
        "audit_description debe contener exactamente un valor para:",
        audit_group
      )
    )


  if (length(display_in_audit) != 1L)
    stop(
      paste(
        "display_in_audit debe contener exactamente un valor para:",
        audit_group
      )
    )


  data.frame(
    audit_group = audit_group,
    audit_label = audit_label,
    audit_description = audit_description,
    display_in_audit = display_in_audit,
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 1.4 Build audit-diagnostic catalogue for one stage
# ------------------------------------------------------------------------------

build_stage_audit_catalog <- function(
  stage_id,
  diagnostics
) {

  if (length(stage_id) != 1L)
    stop(
      "stage_id debe contener exactamente un identificador."
    )


  if (length(diagnostics) == 0L)
    stop(
      paste(
        "La etapa no contiene diagnósticos de auditoría:",
        stage_id
      )
    )


  stage_catalog <- do.call(
    rbind,
    diagnostics
  )


  stage_catalog$stage_id <- stage_id


  stage_catalog <- stage_catalog[
    ,
    c(
      "stage_id",
      "audit_group",
      "audit_label",
      "audit_description",
      "display_in_audit"
    ),
    drop = FALSE
  ]


  rownames(stage_catalog) <- NULL

  stage_catalog
}
