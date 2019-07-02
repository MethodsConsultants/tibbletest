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
