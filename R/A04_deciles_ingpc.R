# ==============================================================================
# ENEMDU per capita income deciles
# ==============================================================================


# ==============================================================================
# 1. SPSS-equivalent weighted ntile
# ==============================================================================
weighted_ntile_spss <- function(x, w, n = 10) {

  # Reproduce SPSS RANK ... /NTILES(n) /TIES=MEAN using survey weights
  rank_base <- aggregate(
    w ~ x,
    data = data.frame(
      x = as.numeric(x),
      w = as.numeric(w)
    ),
    FUN = sum
  )

  rank_base <- rank_base[
    order(rank_base$x),
    ,
    drop = FALSE
  ]

  total_w <- sum(rank_base$w)

  cum_w_before <- c(
    0,
    head(cumsum(rank_base$w), -1)
  )

  rank_base$mean_rank <-
    cum_w_before + (rank_base$w + 1) / 2

  rank_base$ntile <-
    floor(
      1 + (rank_base$mean_rank * n) / (total_w + 1)
    )

  as.integer(
    rank_base$ntile[
      match(as.numeric(x), rank_base$x)
    ]
  )
}


# ==============================================================================
# 2. Create INGPC deciles
# ==============================================================================
create_ingpc_deciles <- function(ENEMDU_decile, return_evidence = FALSE) {

  # Calculate per capita income deciles using the validated methodology
  ENEMDU_decile$ningpc <- weighted_ntile_spss(
    ENEMDU_decile$ingpc,
    ENEMDU_decile$fexp,
    n = 10
  )

  # Preserve variable metadata
  attr(ENEMDU_decile$ningpc, "label") <-
    "Per capita income decile"

  # Validate generated deciles
  observed_deciles <-
    sort(unique(ENEMDU_decile$ningpc))

  missing_deciles <-
    setdiff(1:10, observed_deciles)

  invalid_deciles <-
    setdiff(observed_deciles, 1:10)

  decile_no_missing <- !anyNA(ENEMDU_decile$ningpc)
  if (!decile_no_missing)
    abort_stage_validation(
      stage_id = "A04_deciles_ingpc",
      check_id = "decile_no_missing",
      message = "La variable de decil de ingreso per cápita contiene valores perdidos."
    )

  decile_complete_coverage <- length(missing_deciles) == 0L
  if (!decile_complete_coverage)
    abort_stage_validation(
      stage_id = "A04_deciles_ingpc",
      check_id = "decile_complete_coverage",
      message = paste(
        "Faltan deciles esperados:",
        paste(missing_deciles, collapse = ", ")
      )
    )

  decile_codes_valid <- length(invalid_deciles) == 0L
  if (!decile_codes_valid)
    abort_stage_validation(
      stage_id = "A04_deciles_ingpc",
      check_id = "decile_codes_valid",
      message = paste(
        "Se generaron valores de decil no válidos:",
        paste(invalid_deciles, collapse = ", ")
      )
    )

  # Keep the original return contract: a data frame unless evidence is requested.
  validation_evidence <- data.frame(
    stage_id = rep("A04_deciles_ingpc", 3L),
    check_id = c(
      "decile_no_missing",
      "decile_complete_coverage",
      "decile_codes_valid"
    ),
    passed = c(
      decile_no_missing,
      decile_complete_coverage,
      decile_codes_valid
    ),
    stringsAsFactors = FALSE
  )

  if (isTRUE(return_evidence))
    return(list(
      data = ENEMDU_decile,
      validation_evidence = validation_evidence
    ))

  ENEMDU_decile
}


# ==============================================================================
# 3. Build decile frequency table
# ==============================================================================
build_decile_frequency_table <- function(
  ENEMDU_decile,
  input_references,
  return_evidence = FALSE
) {

  # Expanded population in the valid INGPC analytical universe
  total_expanded_valid <- sum(ENEMDU_decile$fexp)

  # Validate against the Stage A01 reference.
  decile_expanded_population_consistent <- isTRUE(all.equal(
    total_expanded_valid,
    input_references$input_ingpc_valid_expanded,
    tolerance = 1e-8
  ))

  if (!decile_expanded_population_consistent)
    abort_stage_validation(
      stage_id = "A04_deciles_ingpc",
      check_id = "decile_expanded_population_consistent",
      message = paste(
        "La población expandida de la tabla de frecuencias por decil",
        "difiere de la población con INGPC válido registrada en la Etapa A01."
      )
    )

  # Build weighted frequency table
  decile_frequency_table <- aggregate(
    fexp ~ ningpc,
    data = ENEMDU_decile,
    FUN = sum
  )

  names(decile_frequency_table) <- c(
    "Decile",
    "Frequency"
  )

  decile_frequency_table$Percentage <-
    (
      decile_frequency_table$Frequency /
        total_expanded_valid
    ) * 100

  # Add total row
  decile_frequency_table <- rbind(
    decile_frequency_table,
    data.frame(
      Decile = "Total",
      Frequency = total_expanded_valid,
      Percentage = 100
    )
  )

  # This comparison is a cross-stage control; the table contains only
  # statistical results and never carries audit metadata.
  if (isTRUE(return_evidence))
    return(list(
      decile_frequency_table = decile_frequency_table,
      validation_evidence = data.frame(
        stage_id = "A04_deciles_ingpc",
        check_id = "decile_expanded_population_consistent",
        passed = decile_expanded_population_consistent,
        stringsAsFactors = FALSE
      )
    ))

  decile_frequency_table
}


# ==============================================================================
# 4. Build decile report table
# ==============================================================================
build_decile_report_table <- function(ENEMDU_decile) {

  # Weighted report by decile
  decile_report_table <- do.call(
    rbind,
    lapply(
      split(
        ENEMDU_decile,
        ENEMDU_decile$ningpc
      ),
      function(df) {

        expanded_n <- sum(df$fexp)

        weighted_sum <- sum(
          df$ingpc * df$fexp
        )

        data.frame(
          Decile = unique(df$ningpc),
          N = expanded_n,
          Sum = weighted_sum,
          Mean = weighted_sum / expanded_n,
          Min = min(df$ingpc),
          Max = max(df$ingpc)
        )
      }
    )
  )

  rownames(decile_report_table) <- NULL

  # Preserve exact unrounded means for Stage A06 validation
  point4_ingpc_mean_reference <-
    decile_report_table[
      ,
      c("Decile", "Mean")
    ]

  rownames(point4_ingpc_mean_reference) <- NULL

  # Add total row
  total_report_row <- data.frame(
    Decile = "Total",
    N = sum(ENEMDU_decile$fexp),
    Sum = sum(
      ENEMDU_decile$ingpc *
        ENEMDU_decile$fexp
    ),
    Mean =
      sum(
        ENEMDU_decile$ingpc *
          ENEMDU_decile$fexp
      ) /
      sum(ENEMDU_decile$fexp),
    Min = min(ENEMDU_decile$ingpc),
    Max = max(ENEMDU_decile$ingpc)
  )

  decile_report_table <- rbind(
    decile_report_table,
    total_report_row
  )

  list(
    decile_report_table = decile_report_table,
    point4_ingpc_mean_reference =
      point4_ingpc_mean_reference
  )
}


# ==============================================================================
# 5. Execute Stage A04 once and collect reusable results and audit evidence
# ==============================================================================
build_ingpc_decile_stage <- function(ENEMDU_decile, input_references) {

  # Decile construction is run once; all downstream tables share that same data.
  decile_result <- create_ingpc_deciles(
    ENEMDU_decile,
    return_evidence = TRUE
  )

  frequency_result <- build_decile_frequency_table(
    decile_result$data,
    input_references,
    return_evidence = TRUE
  )

  report_result <- build_decile_report_table(decile_result$data)

  validation_evidence <- rbind(
    decile_result$validation_evidence,
    frequency_result$validation_evidence
  )
  rownames(validation_evidence) <- NULL

  list(
    data = decile_result$data,
    decile_frequency_table = frequency_result$decile_frequency_table,
    decile_report_table = report_result$decile_report_table,
    point4_ingpc_mean_reference = report_result$point4_ingpc_mean_reference,
    validation_evidence = validation_evidence
  )
}
