#' Calculates inverse probability of treatment weights (IPTW) using a random
#' forest
#'
#' \code{add_propensity_weights()} adds a column of propensity weights to a data
#' frame. It calculates the propensity score using a random forest model to
#' predict the probability of treatment selection conditional on baseline
#' covariates. Stabilized inverse propensity weights are then calculated from
#' the propensity scores and these weights are returned as a new column in the
#' data frame.
#'
#' @section Random Forest Model: A propensity score is the probability of a unit
#'   being assigned to a particular treatment group given a set of baseline
#'   characteristics. To calculate the propensity score for each unit, we fit a
#'   model with treatment group as the outcome and baseline characteristics as
#'   independent variables. The \code{ranger} package is used to fit a random
#'   forest model to calculate the propensity scores. A probability forest is
#'   fit by specifying \code{probability = TRUE}, and the
#'   \code{respect.unordered.factors} argument is set to \code{TRUE}. All other
#'   arguments are left at their defaults. See \code{\link[ranger]{ranger}} for
#'   more details.
#'
#' @section Missing Data Imputation: \code{ranger} does not support missing
#'   values, so they have to be dealt with prior to running the model. By
#'   setting \code{impute_missing = TRUE} the function will automatically impute
#'   the independent variables prior to fitting the random forest model. It uses
#'   k-nearest neighbors with \code{k = 5} for imputation.
#'
#' @section Stabilized Inverse Probability of Treatment Weights: The IPTW for
#'   each subject is calculated by taking the inverse of the probability that
#'   they received the treatment that they actually received. That is, if a
#'   subject is in the "control" group, then their weight is calculated as
#'   \eqn{1 / P(Treatment = Control)}. An issue with traditional IPTWs is that
#'   the sample size of the weighted data (sum of the weights) is higher than
#'   the original sample size. An improvement to the traditional weights are
#'   stabilized weights, which insure that the mean weight in each treatment
#'   group is 1. The weights that the function outputs are stabilized weights.
#'
#' @param df <`tbl_df`> Data frame on which to perform weighting.
#' @param treatment <`tidy-select`> Treatment column to use in propensity score
#'   model.
#' @param ivs <`tidy-select`> Columns of independent variables to use in
#'   propensity score model.
#' @param impute_missing <`lgl`> If `TRUE` will internally impute missing values
#'   in IVs for propensity score fit. Uses kNN with k = 5 for imputation.
#'
#' @return <`tbl_df`> Data frame with new column for propensity weights.
#'
#' @import purrr
#' @import dplyr
#' @importFrom rlang :=
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
    select(all_of(ivs)) %>%
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

      most_common_category <- fit_df %>%
        select(ivs) %>%
        drop_na() %>%
        count(.data[[ivs]]) %>%
        arrange(desc(n)) %>%
        slice(1) %>%
        pull(ivs)

      fit_df <- fit_df %>%
        mutate("{ivs}" := replace_na(.data[[ivs]], most_common_category))

    } else {

      median_value <- median(fit_df[[ivs]], na.rm = TRUE)

      fit_df <- fit_df %>%
        mutate("{ivs}" := replace_na(.data[[ivs]], median_value))

    }
  }

  if (missing_ivs > 0 & length(ivs) > 1) {

    fit_df <- fit_df %>%
      select(all_of(ivs)) %>%
      mutate_if(is.character, as.factor) %>%
      VIM::kNN(imp_var = FALSE) %>%
      mutate("{treatment}" := fit_df[[treatment]])

  }

  withr::with_seed(
    seed = 1,
    ranger_prediction <- paste0(treatment, " ~ ", paste(ivs, collapse = " + ")) %>%
      ranger::ranger(data = fit_df, probability = TRUE, respect.unordered.factors = TRUE) %>%
      pluck("predictions") %>%
      tibble::as_tibble()
  )

  propensity_score <- ranger_prediction %>%
    transpose() %>%
    map2_dbl(
      as.character(df[[treatment]]),
      ~ pluck(.x, .y)
    )

  df %>%
    mutate(
      propensity_weight = 1 / propensity_score
    ) %>%
    group_by(.data[[treatment]]) %>%
    mutate(
      propensity_weight = propensity_weight / mean(propensity_weight)
    ) %>%
    ungroup()

}
