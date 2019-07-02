#' Helper function which calculates p-value via chi-square or fisher
#'
#' @param df <`tbl_df`> Dataframe that has variable and treatment columns of interest
#' @param var <`character(1)`> Name of variable column
#' @param treatment <`character(1)`> Name of treatment column
#' @param weight_var <`character(1)`> Name of variable with observation weights
#'
#' @return <`numeric(1)`> p-value
#'
#' @noRd
p_chi_fisher <- function(df, var, treatment, weight_var) {

  df <- df %>%
    drop_na(!!var, !!treatment) %>%
    mutate_at(vars(weight_var), ~ .x / mean(.x))

  paste0(var, "~", treatment, "+", weight_var) %>%
    as.formula() %>%
    sjstats:::wtd_chisqtest.formula(data = df) %>%
    purrr::pluck("p.value")

}

#' Helper function which calculates p-value via anova
#'
#' @inheritParams p_chi_fisher
#'
#' @return <`numeric(1)`> p-value
#'
#' @import dplyr
#'
#' @noRd
p_anova <- function(df, var, treatment, weight_var) {

  weight_var <- sym(weight_var)

  df <- df %>%
    rename(wt = !!weight_var)

  paste0(var, " ~ ", treatment) %>%
    lm(
      weights = wt,
      data = df
    ) %>%
    anova() %>%
    pull(`Pr(>F)`) %>%
    purrr::pluck(1)

}

#' Helper function which calculates p-value via Kruskal-Wallis
#'
#' @inheritParams p_chi_fisher
#'
#' @return <`numeric(1)`> p-value
#'
#' @import survey
#'
#' @noRd
p_kruskal <- function(df, var, treatment, weight_var) {

  if (all(df[[weight_var]] == 1)) {

    p <- kruskal.test(df[[var]], df[[treatment]]) %>%
      purrr::pluck("p.value")
    return(p)

  }

  survey_obj <- svydesign(~1, data = df, weights = df[[weight_var]])

  var %>%
    paste0(" ~ ", treatment) %>%
    stats::as.formula() %>%
    svyranktest(design = survey_obj) %>%
    purrr::pluck("p.value") %>%
    as.numeric()

}

#' Helper function which returns whether or not the column is numeric
#'
#' @inheritParams summary_cat
#' @param var <`character(1)`> Name of variable in dataframe.
#'
#' @return <`logical(1)`>
#'
#' @importFrom stats na.omit
#'
#' @keywords internal
#' @noRd
is_numeric_variable <- function(df, var) {

  var <- df %>%
    pull(var)

  vals <- var %>%
    na.omit() %>%
    unique()

  logical <- vals %>%
    `%in%`(c(0, 1)) %>%
    all()

  if (length(vals) == 0) {
    logical <- FALSE
  }

  if (logical) {
    return(FALSE)
  }

  purrr::is_bare_numeric(var)

}

#' Helper function which returns whether or not the column is categorical
#'
#' @inheritParams is_numeric_variable
#'
#' @return <`logical(1)`>
#'
#' @importFrom stats na.omit
#'
#' @keywords internal
#' @noRd
is_categorical_variable <- function(df, var) {

  var <- df %>%
    pull(var)

  vals <- var %>%
    na.omit() %>%
    unique()

  logical <- vals %>%
    `%in%`(c(0, 1)) %>%
    all()

  if (length(vals) == 0) {
    logical <- FALSE
  }

  if (logical) {
    return(TRUE)
  }

  (purrr::is_bare_character(var) | purrr::is_bare_logical(var) | is.factor(var))

}

#' Helper function which calculates proportions and outputs in "N (%)" format
#'
#' @param count_vec <`numeric`> Vector of counts.
#' @param weighted <`logical(1)`> Are these weighted counts?
#'
#' @return <`character`> Count and proportion "N (%)"
#'
#' @keywords internal
#' @noRd
proportions <- function(count_vec, weighted) {

  if (weighted) {

    total <- sum(count_vec, na.rm = TRUE)
    probs <- round(count_vec / total * 100, 2)

    probs <- case_when(
      is.na(probs) ~ 0,
      TRUE ~ probs
    )

    percentage <- paste0(as.character(probs), "%")
    return(percentage)

  }

  total <- sum(count_vec, na.rm = TRUE)
  probs <- round(count_vec / total * 100, 2)

  probs <- case_when(
    is.na(probs) ~ 0,
    TRUE ~ probs
  )

  int_vec <- case_when(
    is.na(count_vec) ~ 0,
    TRUE ~ count_vec
  )

  paste0(as.character(count_vec), " (", as.character(probs), "%)")

}

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

  assertthat::assert_that(is.numeric(vec))
  assertthat::assert_that(is.numeric(weight_vec))
  assertthat::assert_that(all(weight_vec > 0))
  assertthat::assert_that(length(vec) == length(weight_vec))

  weight_vec <- weight_vec / mean(weight_vec)
  weighted.mean(vec, weight_vec, na.rm = TRUE)

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


