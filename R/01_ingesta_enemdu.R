# ==============================================================================
# ENEMDU ingestion
# ==============================================================================


# ==============================================================================
# 0. Canonical input schema
# ==============================================================================

# Required variables for Stage 01.
# These definitions are declared once and reused by validation and profiling.
ENEMDU_REQUIRED_VARS <- c(
  "prov", "ciudad", "zona", "sector", "rn", "area",
  "fexp", "ingpc", "pobreza", "epobreza", "CONDACTN"
)

# ==============================================================================
# 1. Extract and validate period metadata
# ==============================================================================
# Production naming convention: ENEMDU_YYYY_MM.sav.
# The "_" separator standardizes period identification in the multi-period pipeline; methodology is unchanged.

get_period_metadata <- function(input_file) {

  # Period is recovered once and kept separate from internal object names
  input_name <- basename(input_file)
  period_match <- regexec("^ENEMDU_([0-9]{4})_([0-9]{2})\\.sav$", input_name)
  period_parts <- regmatches(input_name, period_match)[[1]]

  if (length(period_parts) != 3L)
    stop("El nombre del archivo de entrada debe seguir el formato ENEMDU_YYYY_MM.sav.")

  period_year <- as.integer(period_parts[2])
  period_month <- as.integer(period_parts[3])

  if (is.na(period_year) || is.na(period_month) || period_month < 1L || period_month > 12L)
    stop(paste("Se recuperó un período inválido del archivo de entrada:", input_name))

  list(
    input_name = input_name,
    period_year = period_year,
    period_month = period_month,
    period_code = sprintf("%04d_%02d", period_year, period_month)
  )
}


# ==============================================================================
# 2. Import and validate ENEMDU source database
# ==============================================================================

read_validate_enemdu <- function(
  input_file,
  return_evidence = FALSE
) {

  # Define source file and verify that it exists before import
  if (!file.exists(input_file))
    stop(paste("No se encontró el archivo de entrada:", input_file))

  # Import complete working database
  # ENEMDU_base is preserved as the complete base; analytical subsets are
  # created separately in subsequent stages
  ENEMDU_base <- haven::read_sav(
    input_file,
    user_na = FALSE
  ) |>
    as.data.frame()

  if (nrow(ENEMDU_base) == 0L || ncol(ENEMDU_base) == 0L)
    stop("La base ENEMDU_base está vacía.")

  # Essential input validation
  missing_required_vars <- setdiff(
    ENEMDU_REQUIRED_VARS,
    names(ENEMDU_base)
  )

  required_variables_present <-
    length(missing_required_vars) == 0L

  if (!required_variables_present)
    stop(paste(
      "Faltan variables requeridas:",
      paste(missing_required_vars, collapse = ", ")
    ))

  required_numeric_vars <- c(
    "prov", "ciudad", "rn", "area", "ingpc", "fexp"
  )

  non_numeric_required_vars <- required_numeric_vars[
    !vapply(
      ENEMDU_base[required_numeric_vars],
      is.numeric,
      logical(1)
    )
  ]

  required_numeric_types_valid <-
    length(non_numeric_required_vars) == 0L

  if (!required_numeric_types_valid)
    stop(paste(
      "Las siguientes variables requeridas no son numéricas:",
      paste(non_numeric_required_vars, collapse = ", ")
    ))

  # fexp is a positive, finite survey expansion factor; these are
  # input-integrity controls
  fexp_no_missing <-
    !anyNA(ENEMDU_base$fexp)

  if (!fexp_no_missing)
    stop("El factor de expansión fexp contiene valores perdidos.")

  fexp_finite <-
    !any(!is.finite(ENEMDU_base$fexp))

  if (!fexp_finite)
    stop("El factor de expansión fexp contiene valores no finitos.")

  fexp_positive <-
    !any(ENEMDU_base$fexp <= 0)

  if (!fexp_positive)
    stop("El factor de expansión fexp contiene valores no positivos.")

  # Structured evidence of substantive controls actually executed above.
  #
  # check_id is the stable internal identifier used to link execution evidence
  # with the control catalogue and, subsequently, the reporting layer.
  #
  # Operational prerequisites remain enforced by stop() but are not duplicated
  # here because the current audit policy keeps them implicit.
  #
  # Classification, audit grouping, visible labels, and descriptions belong to
  # the control catalogue rather than to the execution evidence itself.
  validation_evidence <- data.frame(
    stage_id = rep(
      "01_ingestion",
      5
    ),
    check_id = c(
      "required_variables_present",
      "required_numeric_types_valid",
      "fexp_no_missing",
      "fexp_finite",
      "fexp_positive"
    ),
    passed = c(
      required_variables_present,
      required_numeric_types_valid,
      fexp_no_missing,
      fexp_finite,
      fexp_positive
    ),
    stringsAsFactors = FALSE
  )

  # Preserve the existing function contract by default:
  # read_validate_enemdu(input_file) continues returning only ENEMDU_base.
  #
  # Structured evidence is exposed explicitly only when required by the
  # pipeline or reporting layer; ENEMDU_base itself is never modified to
  # transport audit metadata.
  if (isTRUE(return_evidence)) {
    return(list(
      data = ENEMDU_base,
      validation_evidence = validation_evidence
    ))
  }

  ENEMDU_base
}


# ==============================================================================
# 3. Build input references and diagnostic profile
# ==============================================================================

build_input_profile <- function(
  ENEMDU_base,
  period_metadata
) {

  # Count valid and missing observations for each required variable
  valid_n <- vapply(
    ENEMDU_base[ENEMDU_REQUIRED_VARS],
    function(x) sum(!is.na(x)),
    integer(1)
  )

  missing_n <- vapply(
    ENEMDU_base[ENEMDU_REQUIRED_VARS],
    function(x) sum(is.na(x)),
    integer(1)
  )

  # Expanded population associated with valid and missing observations
  valid_expanded <- vapply(
    ENEMDU_REQUIRED_VARS,
    function(variable)
      sum(
        ENEMDU_base$fexp[
          !is.na(ENEMDU_base[[variable]])
        ]
      ),
    numeric(1)
  )

  missing_expanded <- vapply(
    ENEMDU_REQUIRED_VARS,
    function(variable)
      sum(
        ENEMDU_base$fexp[
          is.na(ENEMDU_base[[variable]])
        ]
      ),
    numeric(1)
  )

  # Preserve input references required downstream
  # INGPC references are used later to verify the analytical universe employed
  # for decile construction and subsequent survey estimates
  references <- list(
    input_sample_n = nrow(ENEMDU_base),
    input_expanded_n = sum(ENEMDU_base$fexp),
    input_ingpc_valid_n = unname(valid_n["ingpc"]),
    input_ingpc_missing_n = unname(missing_n["ingpc"]),
    input_ingpc_valid_expanded = unname(valid_expanded["ingpc"]),
    input_ingpc_missing_expanded = unname(missing_expanded["ingpc"])
  )

  # Canonical INGPC universe summary.
  # Exact values are preserved here; labels and visible numeric formatting
  # belong to the reporting layer.
  ingpc_universe_summary <- data.frame(
    universe_id = c(
      "ingpc_valid",
      "ingpc_missing",
      "input_total"
    ),
    Sample_N = c(
      references$input_ingpc_valid_n,
      references$input_ingpc_missing_n,
      references$input_sample_n
    ),
    Expanded_N = c(
      references$input_ingpc_valid_expanded,
      references$input_ingpc_missing_expanded,
      references$input_expanded_n
    ),
    stringsAsFactors = FALSE
  )

  # General database overview for technical traceability
  database_overview <- data.frame(
    Indicador = c(
      "Archivo",
      "Período",
      "Observaciones",
      "Variables",
      "Tamaño (MB)",
      "Variables con etiqueta",
      "Variables con etiquetas de valor"
    ),
    Valor = c(
      period_metadata$input_name,
      period_metadata$period_code,
      references$input_sample_n,
      ncol(ENEMDU_base),
      round(
        as.numeric(object.size(ENEMDU_base)) / 1024^2,
        2
      ),
      sum(vapply(
        ENEMDU_base,
        function(x)
          !is.null(attr(x, "label")),
        logical(1)
      )),
      sum(vapply(
        ENEMDU_base,
        function(x) {
          labels <- attr(x, "labels")
          !is.null(labels) && length(labels) > 0
        },
        logical(1)
      ))
    ),
    stringsAsFactors = FALSE
  )

  # Required-variable diagnostic overview
  required_variables_overview <- data.frame(
    Variable = ENEMDU_REQUIRED_VARS,
    Class = vapply(
      ENEMDU_base[ENEMDU_REQUIRED_VARS],
      function(x) class(x)[1],
      character(1)
    ),
    Valid_N = valid_n,
    Missing_N = missing_n,
    Valid_Expanded = valid_expanded,
    Missing_Expanded = missing_expanded,
    Label = vapply(
      ENEMDU_base[ENEMDU_REQUIRED_VARS],
      function(x) {
        variable_label <- attr(x, "label")
        if (is.null(variable_label)) "" else as.character(variable_label)
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  list(
    references = references,
    database_overview = database_overview,
    required_variables_overview = required_variables_overview,
    ingpc_universe_summary = ingpc_universe_summary
  )
}
