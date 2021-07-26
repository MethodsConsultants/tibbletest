#' Helper function which calculates the mean of a vector with an optional vector of weights
#'
#' @param vec <`numeric`> Vector to calculate mean
#' @param weight_vec <`numeric`> Optional vector of weights, must be same length as `vec`
#'
#' @return <`character`> Mean of vector
#'
#' @keywords internal
#' @noRd
weighted_mean <- function(vec, weight_vec = rep(1, length(vec))) {

  assertthat::assert_that(is.numeric(vec) | is.logical(vec))
  assertthat::assert_that(is.numeric(weight_vec))
  assertthat::assert_that(all(weight_vec > 0))
  assertthat::assert_that(length(vec) == length(weight_vec))

  stats::weighted.mean(vec, weight_vec, na.rm = TRUE)

}

#' Helper function which calculates the SD of a vector with an optional vector of weights
#'
#' @param vec <`numeric`> Vector to calculate SD
#' @param weight_vec <`numeric`> Optional vector of weights, must be same length as `vec`
#'
#' @return <`character`> SD of vector
#'
#' @keywords internal
#' @noRd
weighted_sd <- function(vec, weight_vec = rep(1, length(vec))) {

  assertthat::assert_that(is.numeric(vec))
  assertthat::assert_that(is.numeric(weight_vec))
  assertthat::assert_that(all(weight_vec > 0))
  assertthat::assert_that(length(vec) == length(weight_vec))

  tbl <- tibble(vec, weight_vec) %>%
    drop_na()

  m <- weighted_mean(tbl$vec, tbl$weight_vec)
  v1 <- sum(tbl$weight_vec)
  v2 <- sum(tbl$weight_vec ^ 2)
  variance <- sum(tbl$weight_vec * (tbl$vec - m) ^ 2) / (v1 - v2/v1)

  variance %>%
    sqrt()

}

#' Helper function which calculates a quantile of a vector with observation weights. Taken from `sjstats:::wtd_md_helper`
#'
#' @param x <`numeric`> Continuous vector of which we want to calculate a quantile
#' @param weights <`numeric`> Nonnegative vector of observation weights
#' @param p <`numeric(1)`> Quantile to compute (0.5 is median)
#'
#' @return <`numeric(1)`>
#'
#' @keywords internal
#' @noRd
weighted_quantile <- function(x, weights, p) {

  x[is.na(weights)] <- NA
  weights[is.na(x)] <- NA

  weights <- na.omit(weights)
  x <- na.omit(x)

  order <- order(x)
  x <- x[order]
  weights <- weights[order]

  rw <- cumsum(weights) / sum(weights)
  md.values <- min(which(rw >= p))

  if (rw[md.values] == p) {
    q <- mean(x[md.values:(md.values + 1)])
  } else {
    q <- x[md.values]
  }

  q

}
