#' Calculates inverse probability of treatment weights using a random forest
#'
#' @param df <`tbl`> Data frame on which to perform weighting
#' @param treatment <`character(1)`> String with name of treatment column
#' @param ivs <`character`> Character vector with names of independent variabes to use in propensity score model
#'
#' @return <`tbl`> Data frame with new column for propensity weights
#'
#' @import purrr
#'
#' @export
propensity_weighting <- function(df, treatment, ivs) {

  ranger_prediction <- paste0(treatment, " ~ ", paste(ivs, collapse = " + ")) %>%
    ranger::ranger(data = df, probability = TRUE, respect.unordered.factors = TRUE) %>%
    pluck("predictions") %>%
    tibble::as_tibble()

  propensity_score <- ranger_prediction %>%
    transpose() %>%
    map2_dbl(df[[treatment]], ~pluck(.x, .y))

  df %>%
    dplyr::mutate(
      propensity_weight = 1 / propensity_score,
      propensity_weight = propensity_weight / mean(propensity_weight)
    )

}

