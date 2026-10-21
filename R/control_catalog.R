# ==============================================================================
# Control catalogue — public canonical interface
# ==============================================================================
# Public API exposed by this file:
#   - control_catalog
#   - audit_diagnostic_catalog
#   - build_stage_audit_diagnostics()
#
# Internal constructors, stage-specific definitions, and the diagnostic engine
# are loaded into an encapsulated environment under R/control_catalog_internal/.
# Consumer modules must source only this file.

local({

  # Environment from which R/control_catalog.R was sourced.
  # Only the supported public API is explicitly exported here.
  public_env <- parent.env(environment())


  # Internal implementation environment.
  internal_env <- new.env(
    parent = public_env
  )

  internal_dir <- file.path(
    "R",
    "control_catalog_internal"
  )

  internal_files <- c(
    "build_catalogs.R",
    "build_audit_diagnostics.R",
    "stage_01a_ingestion.R",
    "stage_02a_identifiers.R",
    "stage_03a_ingpc_treatment.R",
    "stage_04a_deciles_ingpc.R",
    "stage_06a_media_ingpc_por_decil.R",
    "stage_01b_condactn_reference.R",
    "stage_02b_population_classification.R"
  )

  invisible(
    lapply(
      file.path(
        internal_dir,
        internal_files
      ),
      sys.source,
      envir = internal_env
    )
  )


  # ---------------------------------------------------------------------------
  # Assemble stage-specific canonical definitions
  # ---------------------------------------------------------------------------
  stage_definitions <- list(
    internal_env$build_stage_01a_control_definitions(),
    internal_env$build_stage_02a_control_definitions(),
    internal_env$build_stage_03a_control_definitions(),
    internal_env$build_stage_04a_control_definitions(),
    internal_env$build_stage_06a_control_definitions(),
    internal_env$build_stage_01b_control_definitions(),
    internal_env$build_stage_02b_control_definitions()
  )

  assembled_control_catalog <- do.call(
    rbind,
    lapply(
      stage_definitions,
      `[[`,
      "controls"
    )
  )

  assembled_audit_diagnostic_catalog <- do.call(
    rbind,
    lapply(
      stage_definitions,
      `[[`,
      "diagnostics"
    )
  )

  rownames(assembled_control_catalog) <- NULL
  rownames(assembled_audit_diagnostic_catalog) <- NULL


  # ---------------------------------------------------------------------------
  # Explicit public API
  # ---------------------------------------------------------------------------
  assign(
    "control_catalog",
    assembled_control_catalog,
    envir = public_env
  )

  assign(
    "audit_diagnostic_catalog",
    assembled_audit_diagnostic_catalog,
    envir = public_env
  )

  assign(
    "build_stage_audit_diagnostics",
    internal_env$build_stage_audit_diagnostics,
    envir = public_env
  )
})
