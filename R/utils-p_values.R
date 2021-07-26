#' Helper function which calculates p-value via chi-square or fisher
#' Uses `survey::svychisq` for weighted tests, otherwise uses `stats::chisq.test` or `stats::fisher.test` depending on cell counts
#'
#' @param df <`tbl_df`> Dataframe that has variable and treatment columns of interest
#' @param var <`character(1)`> Name of variable column
#' @param treatment <`character(1)`> Name of treatment column
#' @param weight_var <`character(1)`> Name of variable with observation weights
#'
#' @return <`numeric(1)`> p-value
#'
#' @importFrom survey svydesign svychisq
#' @import purrr
#'
#' @noRd
p_chi_fisher <- function(df, var, treatment, weight_var) {

  if (any(df[[weight_var]] != 1)) {

    df <- df %>%
      tidyr::drop_na(.data[[var]], .data[[treatment]])

    survey_obj <- svydesign(~1, data = df, weights = df[[weight_var]])

    p_val <- var %>%
      paste0("~", ., " + ", treatment) %>%
      stats::as.formula() %>%
      svychisq(design = survey_obj) %>%
      pluck("p.value") %>%
      as.numeric()

    return(p_val)

  }

  chisq_wrapper <- function(var, df, treatment) {

    stats::chisq.test(
      x = as.factor(df[[var]]),
      y = as.factor(df[[treatment]])
    ) %>%
      pluck("p.value") %>%
      as.numeric()

  }

  fisher_wrapper <- function(var, df, treatment) {

    p_val <- stats::fisher.test(
      x = as.factor(df[[var]]),
      y = as.factor(df[[treatment]])
    ) %>%
      pluck("p.value")

  }

  chisq_wrapper <- purrr::quietly(chisq_wrapper)
  chisq <- chisq_wrapper(var, df, treatment)

  if (length(chisq$warnings) == 0) {
    return(chisq$result)
  } else {
    return(fisher_wrapper(var, df, treatment))
  }

}

#' Helper function which calculates p-value via anova
#' Uses `survey::svyglm` and `survey::regTermTest` for weighted tests and `stats::lm` and `stats::anova` otherwise
#'
#' @inheritParams p_chi_fisher
#'
#' @return <`numeric(1)`> p-value
#'
#' @import dplyr
#' @importFrom survey svydesign svyglm regTermTest
#'
#' @noRd
p_anova <- function(df, var, treatment, weight_var) {

  if (any(df[[weight_var]] != 1)) {

    df <- df %>%
      tidyr::drop_na(.data[[var]], .data[[treatment]])

    survey_obj <- svydesign(~1, data = df, weights = df[[weight_var]])

    p_val <- var %>%
      paste0(" ~ ", treatment) %>%
      stats::as.formula() %>%
      svyglm(design = survey_obj) %>%
      regTermTest(
        test.terms = treatment,
        method = "Wald"
      ) %>%
      purrr::pluck("p") %>%
      as.numeric()

    return(p_val)

  }

  paste0(var, " ~ ", treatment) %>%
    stats::lm(data = df) %>%
    stats::anova() %>%
    pull(.data$`Pr(>F)`) %>%
    purrr::pluck(1)

}

#' Helper function which calculates p-value via Kruskal-Wallis
#' Uses `survey::svyranktest` for weighted tests and `stats::kruskal.test` otherwise
#'
#' @inheritParams p_chi_fisher
#'
#' @return <`numeric(1)`> p-value
#'
#' @importFrom survey svydesign svyranktest
#'
#' @noRd
p_kruskal <- function(df, var, treatment, weight_var) {

  if (any(df[[weight_var]] != 1)) {

    df <- df %>%
      tidyr::drop_na(.data[[var]], .data[[treatment]])

    survey_obj <- svydesign(~1, data = df, weights = df[[weight_var]])

    p_val <- var %>%
      paste0(" ~ ", treatment) %>%
      stats::as.formula() %>%
      svyranktest(design = survey_obj) %>%
      purrr::pluck("p.value") %>%
      as.numeric()

    return(p_val)

  }

  paste0(var, " ~ ", treatment) %>%
    stats::as.formula() %>%
    stats::kruskal.test(data = df) %>%
    purrr::pluck("p.value")

}
