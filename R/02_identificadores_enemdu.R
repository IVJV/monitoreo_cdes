# ==============================================================================
# ENEMDU identifiers
# ==============================================================================


# ==============================================================================
# 1. Create province
# ==============================================================================

create_province <- function(ENEMDU_base) {

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

  if (anyNA(provin_reconstructed))
    stop("La reconstrucción de provincia a partir de ciudad produjo valores perdidos.")

  if (any(invalid_province_code))
    stop(paste(
      sum(invalid_province_code),
      "registros presentan códigos de provincia reconstruidos no válidos."
    ))

  if (any(province_difference))
    stop(paste(
      sum(province_difference),
      "registros difieren entre la variable original prov y la variable reconstruida provin."
    ))

  ENEMDU_base
}


# ==============================================================================
# 2. Create natural region
# ==============================================================================

create_natural_region <- function(ENEMDU_base) {

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

  if (anyNA(rnatura_reconstructed))
    stop("La reconstrucción de región natural a partir de provin produjo valores perdidos.")

  if (any(invalid_region_code))
    stop(paste(
      sum(invalid_region_code),
      "registros presentan códigos de región natural reconstruidos no válidos."
    ))

  if (any(region_difference))
    stop(paste(
      sum(region_difference),
      "registros difieren entre la variable original rn y la variable reconstruida rnatura."
    ))

  ENEMDU_base
}


# ==============================================================================
# 3. Create PSU identifier
# ==============================================================================

create_psu <- function(ENEMDU_base) {

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

  if (any(invalid_identif))
    stop(paste(
      sum(invalid_identif),
      "registros presentan un identificador PSU/UPM no válido."
    ))

  ENEMDU_base
}


# ==============================================================================
# 4. Create study domain
# ==============================================================================

create_study_domain <- function(ENEMDU_base) {

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

  if (any(unassigned_domain))
    stop(paste(
      sum(unassigned_domain),
      "registros no fueron asignados a un dominio de estudio."
    ))

  if (any(invalid_domain_code))
    stop(paste(
      sum(invalid_domain_code),
      "registros presentan códigos de dominio de estudio no válidos."
    ))

  if (any(domain_area_mismatch))
    stop(paste(
      sum(domain_area_mismatch),
      "registros presentan inconsistencias entre el dominio de estudio y el área."
    ))

  ENEMDU_base
}


# ==============================================================================
# 5. Build identifier diagnostic profile
# ==============================================================================

build_identifiers_profile <- function(ENEMDU_base) {

  # Keep only controls that verify substantive identifier construction.
  # These controls have already passed inside the corresponding construction
  # functions; this summary preserves them for technical traceability.
  identifier_validation_summary <- data.frame(
    Control = c(
      "Reconstrucción de provincia",
      "Reconstrucción de región natural",
      "Identificador PSU/UPM",
      "Dominio de estudio"
    ),
    Resultado = rep("OK", 4),
    stringsAsFactors = FALSE
  )

  # Diagnostic distribution by study domain
  domain_distribution <- ENEMDU_base |>
    dplyr::group_by(dominio, area) |>
    dplyr::summarise(
      Sample_N = dplyr::n(),
      Expanded_N = round(sum(fexp), 0),
      Unique_PSUs = dplyr::n_distinct(identif),
      .groups = "drop"
    ) |>
    dplyr::arrange(dominio, area)

  list(
    identifier_validation_summary = identifier_validation_summary,
    domain_distribution = domain_distribution
  )
}