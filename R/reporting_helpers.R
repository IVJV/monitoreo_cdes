# ==============================================================================
# Common reporting helpers
# ==============================================================================

# Production reporting convention:
# "Muestra" = unweighted number of observations.
# "Población" = population estimate obtained using the expansion factor fexp.
# This convention affects presentation only; analytical methodology is unchanged.

reporting_labels <- c(
  sample = "Muestra",
  valid_sample = "Muestra válida",
  missing_sample = "Muestra perdida",
  population = "Población",
  valid_population = "Población válida",
  missing_population = "Población perdida"
)

reporting_sample_population_note <- paste(
  "Convención de presentación:",
  "Muestra corresponde al número de observaciones sin ponderar;",
  "Población corresponde a la estimación obtenida mediante el factor de expansión fexp."
)


# ==============================================================================
# 1. Integer formatting
# ==============================================================================

format_integer_es <- function(x) {

  formatC(
    as.numeric(x),
    format = "f",
    digits = 0,
    big.mark = ".",
    decimal.mark = ","
  )
}


# ==============================================================================
# 2. Decimal formatting
# ==============================================================================

format_decimal_es <- function(x, digits = 2) {

  formatC(
    as.numeric(x),
    format = "f",
    digits = digits,
    big.mark = ".",
    decimal.mark = ","
  )
}


# ==============================================================================
# 3. Percentage formatting
# ==============================================================================

format_percentage_es <- function(x, digits = 1) {

  paste0(
    formatC(
      as.numeric(x),
      format = "f",
      digits = digits,
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )
}


# ==============================================================================
# 4. Scientific notation formatting
# ==============================================================================

format_scientific_es <- function(x, digits = 3) {

  formatted <- formatC(
    as.numeric(x),
    format = "e",
    digits = digits
  )

  gsub(
    ".",
    ",",
    formatted,
    fixed = TRUE
  )
}