# ==============================================================================
# ENEMDU identifiers
# ==============================================================================


# ==============================================================================
# 1. Create province
# ==============================================================================
create_province <- function(
  ENEMDU_base,
  return_evidence = FALSE
) {

  # Reconstruct province from ciudad following the original SPSS methodology
  ENEMDU_base$provin <- as.integer(
    substr(
      sprintf("%06d", as.integer(ENEMDU_base$ciudad)),
      1,
      2
    )
  )

  # Preserve metadata from the original source variable prov
  attr(ENEMDU_base$provin, "label") <- attr(ENEMDU_base$prov, "label")
  attr(ENEMDU_base$provin, "labels") <- attr(ENEMDU_base$prov, "labels")

  # Validate reconstruction against the original variable prov
  prov_original <- as.integer(ENEMDU_base$prov)
  provin_reconstructed <- as.integer(ENEMDU_base$provin)

  province_difference <-
    (is.na(prov_original) != is.na(provin_reconstructed)) |
    (
      !is.na(prov_original) &
        !is.na(provin_reconstructed) &
        prov_original != provin_reconstructed
    )

  invalid_province_code <-
    !is.na(provin_reconstructed) &
    !(provin_reconstructed %in% c(1:22, 90L))

  province_no_missing <-
    !anyNA(provin_reconstructed)

  province_codes_valid <-
    !any(invalid_province_code)

  province_matches_original <-
    !any(province_difference)

  if (!province_no_missing)
    abort_stage_validation(
      stage_id = "A02_identifiers",
      check_id = "province_no_missing",
      message = "La reconstrucción de provincia a partir de ciudad produjo valores perdidos."
    )

  if (!province_codes_valid)
    abort_stage_validation(
      stage_id = "A02_identifiers",
      check_id = "province_codes_valid",
      message = paste(
        sum(invalid_province_code),
        "registros presentan códigos de provincia reconstruidos no válidos."
      )
    )

  if (!province_matches_original)
    abort_stage_validation(
      stage_id = "A02_identifiers",
      check_id = "province_matches_original",
      message = paste(
        sum(province_difference),
        "registros difieren entre la variable original prov y la variable reconstruida provin."
      )
    )

  # Preserve evidence from the controls actually executed above.
  validation_evidence <- data.frame(
    stage_id = rep("A02_identifiers", 3),
    check_id = c(
      "province_no_missing",
      "province_codes_valid",
      "province_matches_original"
    ),
    passed = c(
      province_no_missing,
      province_codes_valid,
      province_matches_original
    ),
    stringsAsFactors = FALSE
  )

  if (isTRUE(return_evidence)) {
    return(list(
      data = ENEMDU_base,
      validation_evidence = validation_evidence
    ))
  }

  ENEMDU_base
}


# ==============================================================================
# 2. Create natural region
# ==============================================================================
create_natural_region <- function(
  ENEMDU_base,
  return_evidence = FALSE
) {

  provin_num <- as.integer(ENEMDU_base$provin)
  # Reconstruct natural region using the province grouping defined in SPSS
  ENEMDU_base$rnatura <- dplyr::case_when(
    provin_num %in% c(1:6, 10, 11, 17, 18) ~ 1L,
    provin_num %in% c(7:9, 12, 13, 90) ~ 2L,
    provin_num %in% c(14:16, 19, 21, 22) ~ 3L,
    provin_num == 20 ~ 4L,
    TRUE ~ NA_integer_
  )

  # Add variable and value labels
  attr(ENEMDU_base$rnatura, "label") <- "Región natural"
  attr(ENEMDU_base$rnatura, "labels") <- c(
    "Sierra" = 1L,
    "Costa" = 2L,
    "Amazonía" = 3L,
    "Insular" = 4L
  )

  # Validate reconstruction against the original variable rn
  rn_original <- as.integer(ENEMDU_base$rn)
  rnatura_reconstructed <- as.integer(ENEMDU_base$rnatura)

  region_difference <-
    (is.na(rn_original) != is.na(rnatura_reconstructed)) |
    (
      !is.na(rn_original) &
        !is.na(rnatura_reconstructed) &
        rn_original != rnatura_reconstructed
    )

  invalid_region_code <-
    !is.na(rnatura_reconstructed) &
    !(rnatura_reconstructed %in% 1:4)

  natural_region_no_missing <-
    !anyNA(rnatura_reconstructed)

  natural_region_codes_valid <-
    !any(invalid_region_code)

  natural_region_matches_original <-
    !any(region_difference)

  if (!natural_region_no_missing)
    abort_stage_validation(
      stage_id = "A02_identifiers",
      check_id = "natural_region_no_missing",
      message = "La reconstrucción de región natural a partir de provin produjo valores perdidos."
    )

  if (!natural_region_codes_valid)
    abort_stage_validation(
      stage_id = "A02_identifiers",
      check_id = "natural_region_codes_valid",
      message = paste(
        sum(invalid_region_code),
        "registros presentan códigos de región natural reconstruidos no válidos."
      )
    )

  if (!natural_region_matches_original)
    abort_stage_validation(
      stage_id = "A02_identifiers",
      check_id = "natural_region_matches_original",
      message = paste(
        sum(region_difference),
        "registros difieren entre la variable original rn y la variable reconstruida rnatura."
      )
    )

  # Preserve evidence from the controls actually executed above.
  validation_evidence <- data.frame(
    stage_id = rep("A02_identifiers", 3),
    check_id = c(
      "natural_region_no_missing",
      "natural_region_codes_valid",
      "natural_region_matches_original"
    ),
    passed = c(
      natural_region_no_missing,
      natural_region_codes_valid,
      natural_region_matches_original
    ),
    stringsAsFactors = FALSE
  )

  if (isTRUE(return_evidence)) {
    return(list(
      data = ENEMDU_base,
      validation_evidence = validation_evidence
    ))
  }

  ENEMDU_base
}


# ==============================================================================
# 3. Create PSU identifier
# ==============================================================================
create_psu <- function(
  ENEMDU_base,
  return_evidence = FALSE
) {

  # Reproduce the PSU/UPM identifier defined in the original methodology
  ENEMDU_base$identif <- paste0(
    sprintf("%02d", as.integer(ENEMDU_base$provin)),
    substr(
      sprintf("%06d", as.integer(ENEMDU_base$ciudad)),
      3,
      6
    ),
    trimws(as.character(ENEMDU_base$zona)),
    trimws(as.character(ENEMDU_base$sector))
  )

  # Preserve the original methodological variable label
  attr(ENEMDU_base$identif, "label") <- "Identificación del sector"

  # Validate expected 12-digit PSU structure
  invalid_identif <-
    is.na(ENEMDU_base$identif) |
    !grepl("^[0-9]{12}$", ENEMDU_base$identif)

  psu_structure_valid <-
    !any(invalid_identif)

  if (!psu_structure_valid)
    abort_stage_validation(
      stage_id = "A02_identifiers",
      check_id = "psu_structure_valid",
      message = paste(
        sum(invalid_identif),
        "registros presentan un identificador PSU/UPM no válido."
      )
    )

  # Preserve evidence from the control actually executed above.
  validation_evidence <- data.frame(
    stage_id = "A02_identifiers",
    check_id = "psu_structure_valid",
    passed = psu_structure_valid,
    stringsAsFactors = FALSE
  )

  if (isTRUE(return_evidence)) {
    return(list(
      data = ENEMDU_base,
      validation_evidence = validation_evidence
    ))
  }

  ENEMDU_base
}


# ==============================================================================
# 4. Create study domain
# ==============================================================================
create_study_domain <- function(
  ENEMDU_base,
  return_evidence = FALSE
) {

  rnatura_num <- as.integer(ENEMDU_base$rnatura)
  area_num <- as.integer(ENEMDU_base$area)
  ciudad_num <- as.integer(ENEMDU_base$ciudad)

  # Reproduce the study-domain classification defined in the original SPSS
  ENEMDU_base$dominio <- dplyr::case_when(

    # Self-represented cities
    ciudad_num == 170150 & area_num == 1 ~ "01",
    ciudad_num ==  90150 & area_num == 1 ~ "02",
    ciudad_num ==  10150 & area_num == 1 ~ "03",
    ciudad_num ==  70150 & area_num == 1 ~ "04",
    ciudad_num == 180150 & area_num == 1 ~ "05",

    # Urban domains by natural region
    rnatura_num == 1 &
      area_num == 1 &
      !(ciudad_num %in% c(10150, 170150, 180150)) ~ "06",

    rnatura_num == 2 &
      area_num == 1 &
      !(ciudad_num %in% c(70150, 90150)) ~ "07",

    rnatura_num == 3 &
      area_num == 1 ~ "08",

    # Rural domains by natural region
    rnatura_num == 1 & area_num == 2 ~ "09",
    rnatura_num == 2 & area_num == 2 ~ "10",
    rnatura_num == 3 & area_num == 2 ~ "11",

    TRUE ~ NA_character_
  )

  # Validate domain assignment and consistency with area
  valid_domain_codes <- sprintf("%02d", 1:11)

  unassigned_domain <- is.na(ENEMDU_base$dominio)

  invalid_domain_code <-
    !unassigned_domain &
    !(ENEMDU_base$dominio %in% valid_domain_codes)

  domain_area_mismatch <-
    !unassigned_domain & (
      (
        ENEMDU_base$dominio %in% sprintf("%02d", 1:8) &
          area_num != 1
      ) |
      (
        ENEMDU_base$dominio %in% sprintf("%02d", 9:11) &
          area_num != 2
      )
    )

  study_domain_assigned <-
    !any(unassigned_domain)

  study_domain_codes_valid <-
    !any(invalid_domain_code)

  study_domain_area_consistent <-
    !any(domain_area_mismatch)

  if (!study_domain_assigned)
    abort_stage_validation(
      stage_id = "A02_identifiers",
      check_id = "study_domain_assigned",
      message = paste(
        sum(unassigned_domain),
        "registros no fueron asignados a un dominio de estudio."
      )
    )

  if (!study_domain_codes_valid)
    abort_stage_validation(
      stage_id = "A02_identifiers",
      check_id = "study_domain_codes_valid",
      message = paste(
        sum(invalid_domain_code),
        "registros presentan códigos de dominio de estudio no válidos."
      )
    )

  if (!study_domain_area_consistent)
    abort_stage_validation(
      stage_id = "A02_identifiers",
      check_id = "study_domain_area_consistent",
      message = paste(
        sum(domain_area_mismatch),
        "registros presentan inconsistencias entre el dominio de estudio y el área."
      )
    )

  # Preserve evidence from the controls actually executed above.
  validation_evidence <- data.frame(
    stage_id = rep("A02_identifiers", 3),
    check_id = c(
      "study_domain_assigned",
      "study_domain_codes_valid",
      "study_domain_area_consistent"
    ),
    passed = c(
      study_domain_assigned,
      study_domain_codes_valid,
      study_domain_area_consistent
    ),
    stringsAsFactors = FALSE
  )

  if (isTRUE(return_evidence)) {
    return(list(
      data = ENEMDU_base,
      validation_evidence = validation_evidence
    ))
  }

  ENEMDU_base
}


# ==============================================================================
# 5. Create all identifiers and preserve structured validation evidence
# ==============================================================================
create_identifiers <- function(
  ENEMDU_base,
  return_evidence = FALSE
) {

  # Execute each transformation once and collect the evidence produced by the
  # same controls that govern fail-fast execution.
  province_result <- create_province(
    ENEMDU_base,
    return_evidence = TRUE
  )

  natural_region_result <- create_natural_region(
    province_result$data,
    return_evidence = TRUE
  )

  psu_result <- create_psu(
    natural_region_result$data,
    return_evidence = TRUE
  )

  study_domain_result <- create_study_domain(
    psu_result$data,
    return_evidence = TRUE
  )

  ENEMDU_base <- study_domain_result$data

  validation_evidence <- rbind(
    province_result$validation_evidence,
    natural_region_result$validation_evidence,
    psu_result$validation_evidence,
    study_domain_result$validation_evidence
  )

  rownames(validation_evidence) <- NULL

  if (isTRUE(return_evidence)) {
    return(list(
      data = ENEMDU_base,
      validation_evidence = validation_evidence
    ))
  }

  ENEMDU_base
}


# ==============================================================================
# 6. Build identifier diagnostic profile
# ==============================================================================
build_identifiers_profile <- function(ENEMDU_base) {

  # Technical summary of the identifiers constructed in Stage A02.
  # The reporting layer consumes these computed results and is responsible only
  # for presentation formatting; it does not recompute analytical quantities.
  identifier_variables <- c(
    "provin",
    "rnatura",
    "identif",
    "dominio"
  )

  identifier_descriptions <- c(
    "Provincia reconstruida",
    "Región natural reconstruida",
    "Identificador UPM",
    "Dominio de estudio"
  )

  identifier_summary <- data.frame(
    Variable = identifier_variables,
    Description = identifier_descriptions,

    Sample_Valid = vapply(
      identifier_variables,
      function(variable)
        sum(!is.na(ENEMDU_base[[variable]])),
      integer(1)
    ),

    Population_Valid = vapply(
      identifier_variables,
      function(variable)
        sum(
          ENEMDU_base$fexp[
            !is.na(ENEMDU_base[[variable]])
          ]
        ),
      numeric(1)
    ),

    Distinct_Values = vapply(
      identifier_variables,
      function(variable)
        dplyr::n_distinct(
          ENEMDU_base[[variable]],
          na.rm = TRUE
        ),
      integer(1)
    ),

    stringsAsFactors = FALSE
  )

  # Diagnostic distribution by study domain.
  # Exact expanded values are preserved here; visible rounding belongs to the
  # reporting layer.
  domain_distribution <- ENEMDU_base |>
    dplyr::group_by(dominio, area) |>
    dplyr::summarise(
      Sample_N = dplyr::n(),
      Expanded_N = sum(fexp),
      Unique_PSUs = dplyr::n_distinct(identif),
      .groups = "drop"
    ) |>
    dplyr::arrange(dominio, area)

  list(
    identifier_summary = identifier_summary,
    domain_distribution = domain_distribution
  )
}
