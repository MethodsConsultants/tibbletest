#' Calculates inverse probability of treatment weights using a random forest.
#'
#' @param df <`tbl_df`> Data frame on which to perform weighting.
#' @param treatment <`tidy-select`> Treatment column to use in propensity score model.
#' @param ivs <`tidy-select`> Columns of independent variables to use in propensity score model.
#' @param impute_missing <`lgl`> If `TRUE` will internally impute missing values in IVs for propensity score fit.
#'
#' @return <`tbl_df`> Data frame with new column for propensity weights.
#'
#' @import purrr
#' @import dplyr
#' @importFrom rlang := !! sym
#'
#' @export
add_propensity_weights <- function(df, treatment, ivs, impute_missing = FALSE) {

  assert_that(is.data.frame(df))

  treatment <- df %>%
    select({{ treatment }}) %>%
    colnames()

  assert_that(is_categorical_variable(df, treatment))
  assert_that(noNA(df[[treatment]]))

  ivs <- df %>%
    select({{ ivs }}) %>%
    colnames()

  assert_that(all(ivs %in% colnames(df)))

  fit_df <- df %>%
  	mutate_at(
  	  treatment,
  	  factor
  	)

  missing_ivs <- fit_df %>%
    select(ivs) %>%
    is.na() %>%
    sum()

  if (missing_ivs > 0 & !impute_missing) {

    stop(
      "\nOne or more IVs have missing data, you should impute or drop missing cases prior to using this function.\n",
      "If you want to leave the missing data but still calculate propensity scores for every observation, then set `impute_missing = TRUE`.",
      call. = FALSE
    )

  }

  if (missing_ivs > 0 & length(ivs) == 1) {

    if (is_categorical_variable(fit_df, ivs)) {

      iv_quo <- sym(ivs)

      most_common_category <- fit_df %>%
        select(!!iv_quo) %>%
        drop_na() %>%
        count(!!iv_quo) %>%
        arrange(desc(n)) %>%
        slice(1) %>%
        pull(!!iv_quo)

      fit_df <- fit_df %>%
        mutate(!!ivs := replace_na(!!ivs, most_common_category))

    } else {

      median_value <- median(fit_df[[ivs]], na.rm = TRUE)

      fit_df <- fit_df %>%
        mutate(!!ivs := replace_na(!!ivs, median_value))

    }
  }

  if (missing_ivs > 0 & length(ivs) > 1) {

      fit_df <- fit_df %>%
        select(ivs) %>%
        mutate_if(is.character, as.factor) %>%
        mice::mice(m = 1, print = FALSE) %>%
        mice::complete() %>%
        mutate(!!treatment := fit_df[[treatment]])

  }

  ranger_prediction <- paste0(treatment, " ~ ", paste(ivs, collapse = " + ")) %>%
    ranger::ranger(data = fit_df, probability = TRUE, respect.unordered.factors = TRUE) %>%
    pluck("predictions") %>%
    tibble::as_tibble()

  propensity_score <- ranger_prediction %>%
    transpose() %>%
    map2_dbl(df[[treatment]], ~ pluck(.x, .y))

  df %>%
    mutate(
      propensity_weight = 1 / propensity_score
    ) %>%
    group_by(!!sym(treatment)) %>%
    mutate(
      propensity_weight = propensity_weight / mean(propensity_weight)
    ) %>%
    ungroup()

}
