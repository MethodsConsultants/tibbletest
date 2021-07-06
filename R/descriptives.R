#' Creates a table of summary statistics
#'
#' Generates a table with summary statistics for each variable, optionally split by a grouping variable.
#'
#' @param df <`tbl_df`> Data frame with treatment and variables of interest as columns.
#' @param variables <`tidy-select`> Columns to summarize in table. If left blank, will be inferred from data.
#' @param treatment <`tidy-select`> Treatment column. If left blank, will produce univariate summary statistics.
#' @param weights <`tidy-select`> Column with observation weights. If left blank, will not use observation weights.
#' @param nonparametric <`tidy-select`> Columns of continuous variables to calculate median/IQR/non-parametric test.
#'
#' @return <`tbl_df`> Tibble with summary statistics split by treatment (optional).
#'
#' @import dplyr
#' @import purrr
#' @import assertthat
#'
#' @export
descriptives <- function(df, treatment = NULL, variables = NULL, weights = NULL, nonparametric = NULL) {

  assert_that(is.data.frame(df))

  if (!missing(treatment)) {
    treatment <- df %>%
      select({{ treatment }}) %>%
      colnames()
    assert_that(is_categorical_variable(df, treatment))
  }

  if (!missing(variables)) {
    variables <- df %>%
      select({{ variables }}) %>%
      colnames()
  }

  if (!missing(weights)) {
    weights <- df %>%
      select({{ weights }}) %>%
      colnames()
    assert_that(is.numeric(df[[weights]]))
    assert_that(noNA(df[[weights]]))
    assert_that(all(df[[weights]] > 0))
  }

  if (!missing(nonparametric)) {
    nonparametric <- df %>%
      select({{ nonparametric }}) %>%
      colnames()
    assert_that(all(map_lgl(nonparametric, is_numeric_variable, df = df)))
  }


  if (is.null(variables)) {
    variables <- colnames(df)
    variables <- variables[!(variables %in% c(treatment, weights))]
  }

  cont_lgl <- variables %>%
    map_lgl(is_numeric_variable, df = df)
  cont_vars <- variables[cont_lgl]

  cat_lgl <- variables %>%
    map_lgl(is_categorical_variable, df = df)
  cat_vars <- variables[cat_lgl]

  if (any(!variables %in% c(cont_vars, cat_vars))) {
    message(
      "Variables (",
      paste(variables[!(variables %in% c(cont_vars, cat_vars))], collapse = ", "),
      ") were skipped, type must be integer, double, character, logical, or factor"
    )
  }

  if (length(cat_vars) == 0 & length(c(cont_vars, nonparametric)) == 0) {
    stop("No valid variable types")
  }

  if (!is.null(treatment)) {

    count_attr <- df %>%
      tidyr::drop_na(.data[[treatment]]) %>%
      count(.data[[treatment]]) %>%
      rename(label = treatment)

  } else {
    count_attr <- tibble(label = "Statistics", n = nrow(df))
  }

  if (!is.null(weights) & !is.null(treatment)) {

    weight_mean <- df %>%
      drop_na(.data[[treatment]]) %>%
      pull(.data[[weights]]) %>%
      mean()

    if (weight_mean != 1) {
      message("Weights were normalized to a mean of 1 to preserve sample size in significance tests")
    }

  }

  if (length(cat_vars) == 0) {

    cont_tbl <- cont_descriptives(df, cont_vars, treatment, weights, nonparametric)

    cont_tbl <- cont_tbl %>%
      mutate(
        Variable = factor(
          Variable,
          levels = unique(c(variables, nonparametric))
        )
      ) %>%
      arrange(Variable) %>%
      mutate(
        Variable = as.character(Variable)
      )

    attr(cont_tbl, "counts") <- count_attr
    class(cont_tbl) <- c("tbl_test", class(cont_tbl))
    return(cont_tbl)

  }

  if (length(c(cont_vars, nonparametric)) == 0) {

    cat_tbl <- cat_descriptives(df, cat_vars, treatment, weights)

    cat_tbl <- cat_tbl %>%
      mutate(
        Variable = factor(
          Variable,
          levels = variables
        )
      ) %>%
      arrange(Variable) %>%
      mutate(
        Variable = as.character(Variable)
      )

    attr(cat_tbl, "counts") <- count_attr
    class(cat_tbl) <- c("tbl_test", class(cat_tbl))
    return(cat_tbl)

  }

  cont_tbl <- cont_descriptives(df, cont_vars, treatment, weights, nonparametric)
  cat_tbl <- cat_descriptives(df, cat_vars, treatment, weights)

  tbl <- bind_rows(cat_tbl, cont_tbl) %>%
    tidyr::replace_na(list(Label = ""))

  tbl <- tbl %>%
    mutate(
      Variable = factor(
        Variable,
        levels = unique(c(variables, nonparametric))
      )
    ) %>%
    arrange(Variable) %>%
    mutate(
      Variable = as.character(Variable)
    )

  attr(tbl, "counts") <- count_attr
  class(tbl) <- c("tbl_test", class(tbl))
  tbl

}

#' Creates a table of summary statistics for categorical variables
#'
#' Generates a table with count and percent for each categorical variable, optionally split by a grouping variable.
#'
#' @inheritParams descriptives
#' @param cat_vars <`character`> Character vector of categorical column names.
#'
#' @return <`tbl_df`> Tibble with summary statistics split by treatment (optional).
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#'
#' @noRd
cat_descriptives <- function(df, cat_vars, treatment, weights) {

  df <- df %>%
    mutate_if(is.factor, as.character)

  if (is.null(weights)) {

    df <- df %>%
      mutate(obs_weights = 1)

    weighted <- FALSE

  } else {

    df <- df %>%
      rename(obs_weights = weights)

    weighted <- TRUE

  }

  if (is.null(treatment)) {

    df %>%
      select(any_of(cat_vars), obs_weights) %>%
      pivot_longer(
        cols = any_of(cat_vars),
        names_to = "Variable",
        values_to = "Label"
      ) %>%
      group_by(Variable, Label) %>%
      summarise(weighted_count = sum(obs_weights)) %>%
      ungroup() %>%
      drop_na() %>%
      group_by(Variable) %>%
      mutate(Statistics = proportions(weighted_count, weighted = weighted)) %>%
      ungroup() %>%
      select(-weighted_count)

  } else {

    df <- df %>%
      drop_na(.data[[treatment]])

    p_chi_fisher <- possibly(p_chi_fisher, NA_real_)

    df %>%
      select(.data[[treatment]], any_of(cat_vars), obs_weights) %>%
      pivot_longer(
        cols = any_of(cat_vars),
        names_to = "Variable",
        values_to = "Label"
      ) %>%
      group_by(.data[[treatment]], Variable, Label) %>%
      summarise(weighted_count = sum(obs_weights)) %>%
      ungroup() %>%
      drop_na() %>%
      tidyr::complete(.data[[treatment]], tidyr::nesting(Variable, Label)) %>%
      group_by(.data[[treatment]], Variable) %>%
      mutate(Statistics = proportions(weighted_count, weighted = weighted)) %>%
      ungroup() %>%
      select(-weighted_count) %>%
      pivot_wider(
        names_from = .data[[treatment]],
        values_from = "Statistics"
      ) %>%
      mutate(`P Value` = map_dbl(Variable, p_chi_fisher, df = df, treatment = treatment, weight_var = "obs_weights"))

  }
}

#' Creates a table of summary statistics for continuous variables
#'
#' Generates a table with mean and sd for each continuous variable, optionally split by a grouping variable.
#'
#' @inheritParams descriptives
#' @param cont_vars <`character`> Character vector of continuous column names.
#'
#' @return <`tbl_df`> Tibble with summary statistics split by treatment (optional).
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#'
#' @noRd
cont_descriptives <- function(df, cont_vars, treatment, weights, nonparametric) {

  if (!is.null(treatment)) {

    df <- df %>%
      drop_na(.data[[treatment]])

  }

  if (is.null(weights)) {

    df <- df %>%
      mutate(obs_weights = 1)

  } else {

    df <- df %>%
      rename(obs_weights = weights)

  }

  if (is.null(nonparametric)) {

    parametric_tbl <- mean_sd_descriptives(df, cont_vars, treatment)
    return(parametric_tbl)

  }

  cont_vars <- cont_vars[!(cont_vars %in% nonparametric)]

  if (length(cont_vars) == 0) {

    nonparametric_tbl <- median_IQR_descriptives(df, nonparametric, treatment)
    return(nonparametric_tbl)

  }

  parametric_tbl <- mean_sd_descriptives(df, cont_vars, treatment)
  nonparametric_tbl <- median_IQR_descriptives(df, nonparametric, treatment)

  bind_rows(parametric_tbl, nonparametric_tbl)

}

#' Creates summary table for continuous variables with mean/sd/anova.
#'
#' @inheritParams cont_descriptives
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#'
#' @noRd
mean_sd_descriptives <- function(df, cont_vars, treatment) {

  ## Set names to force consistent syntax in summarise_at
  var_named <- cont_vars %>%
    set_names()

  if (is.null(treatment)) {

    df %>%
      summarise_at(
        .vars = var_named,
        .funs = list(
          "Mean" = ~weighted_mean(.x, obs_weights),
          "SD" = ~weighted_sd(.x, obs_weights)
        )
      ) %>%
      gather(
        key = "Variable",
        value = "val"
      ) %>%
      separate(
        col = Variable,
        into = c("Variable", "mean_sd"),
        sep = "_(?!.*_)"
      ) %>%
      spread(
        key = mean_sd,
        value = val
      ) %>%
      mutate(Statistics = paste0(round(Mean, 2), " (", round(SD, 2), ")")) %>%
      select(-Mean, -SD)

  } else {

    p_anova <- possibly(p_anova, NA_real_)

    df %>%
      group_by(.data[[treatment]]) %>%
      summarise_at(
        .vars = var_named,
        .funs = list(
          "Mean" = ~weighted_mean(.x, obs_weights),
          "SD" = ~weighted_sd(.x, obs_weights)
        )
      ) %>%
      gather("Variable", "val", -.data[[treatment]]) %>%
      separate(Variable, c("Variable", "mean_sd"), "_(?!.*_)") %>%
      spread(key = mean_sd, value = val) %>%
      mutate(mean_sd = paste0(round(Mean, 2), " (", round(SD, 2), ")")) %>%
      select(-Mean, -SD) %>%
      spread(.data[[treatment]], mean_sd) %>%
      mutate(`P Value` = map_dbl(Variable, p_anova, df = df, treatment = treatment, weight_var = "obs_weights"))

  }
}

#' Creates summary table for continuous variables with median/IQR/Kruskal-Wallis.
#'
#' @inheritParams cont_descriptives
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#'
#' @noRd
median_IQR_descriptives <- function(df, cont_vars, treatment) {

  ## Set names to force consistent syntax in summarise_at
  var_named <- cont_vars %>%
    set_names()

  if (is.null(treatment)) {

    df %>%
      summarise_at(
        .vars = var_named,
        .funs = list(
          "Median" = ~weighted_quantile(.x, obs_weights, 0.5),
          "Q1" = ~weighted_quantile(.x, obs_weights, 0.25),
          "Q3" = ~weighted_quantile(.x, obs_weights, 0.75)
        )
      ) %>%
      gather(
        key = "Variable",
        value = "val"
      ) %>%
      separate(
        col = Variable,
        into = c("Variable", "median_iqr"),
        sep = "_(?!.*_)"
      ) %>%
      spread(
        key = median_iqr,
        value = val
      ) %>%
      mutate(Statistics = paste0(round(Median, 2), " [", round(Q1, 2), ", ", round(Q3, 2), "]")) %>%
      select(-Median, -Q1, -Q3)

  } else {

    p_kruskal <- possibly(p_kruskal, NA_real_)

    df %>%
      group_by(.data[[treatment]]) %>%
      summarise_at(
        .vars = var_named,
        .funs = list(
          "Median" = ~weighted_quantile(.x, obs_weights, 0.5),
          "Q1" = ~weighted_quantile(.x, obs_weights, 0.25),
          "Q3" = ~weighted_quantile(.x, obs_weights, 0.75)
        )
      ) %>%
      gather("Variable", "val", -.data[[treatment]]) %>%
      separate(Variable, c("Variable", "median_iqr"), "_(?!.*_)") %>%
      spread(key = median_iqr, value = val) %>%
      mutate(median_iqr = paste0(round(Median, 2), " [", round(Q1, 2), ", ", round(Q3, 2), "]")) %>%
      select(-Median, -Q1, -Q3) %>%
      spread(.data[[treatment]], median_iqr) %>%
      mutate(`P Value` = map_dbl(Variable, p_kruskal, df = df, treatment = treatment, weight_var = "obs_weights"))

  }
}
