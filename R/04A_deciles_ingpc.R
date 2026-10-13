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

create_ingpc_deciles <- function(ENEMDU_decile) {

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

  if (anyNA(ENEMDU_decile$ningpc))
    stop(
      "La variable de decil de ingreso per cápita contiene valores perdidos."
    )

  if (length(missing_deciles) > 0L)
    stop(paste(
      "Faltan deciles esperados:",
      paste(missing_deciles, collapse = ", ")
    ))

  if (length(invalid_deciles) > 0L)
    stop(paste(
      "Se generaron valores de decil no válidos:",
      paste(invalid_deciles, collapse = ", ")
    ))

  ENEMDU_decile
}


# ==============================================================================
# 3. Build decile frequency table
# ==============================================================================

build_decile_frequency_table <- function(
  ENEMDU_decile,
  input_references
) {

  # Expanded population in the valid INGPC analytical universe
  total_expanded_valid <- sum(ENEMDU_decile$fexp)

  # Validate against the Stage 01A reference
  if (
    !isTRUE(all.equal(
      total_expanded_valid,
      input_references$input_ingpc_valid_expanded,
      tolerance = 1e-8
    ))
  )
    stop(
      paste(
        "La población expandida de la tabla de frecuencias por decil",
        "difiere de la población con INGPC válido registrada en la Etapa 01A."
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

  # Preserve exact unrounded means for Stage 06A validation
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