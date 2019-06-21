#' Creates a table of summary statistics
#'
#' Generates a table with summary statistics for each variable, optionally split by a grouping variable.
#'
#' @param df <`tbl_df`> Data frame with treatment and variables of interest as columns.
#' @param variables <`character`> Character vector of column names. If left blank, will be inferred from data.
#' @param treatment <`character(1)`> String with name of treatment column. If left blank, will produce univariate summary statistics.
#' @param weights <`character(1)`> String with name of column with observation weights. If left blank, will not use observation weights.
#'
#' @return <`tbl_df`> Tibble with summary statistics split by treatment (optional).
#'
#' @import dplyr
#' @import purrr
#' @import assertthat
#'
#' @export
descriptives <- function(df, treatment = NULL, variables = NULL, weights = NULL) {

  assert_that(is.data.frame(df))

  if (!is.null(treatment)) {
    assert_that(is.string(treatment))
    assert_that(is_categorical_variable(df, treatment))
  }

  if (!is.null(variables)) {
    assert_that(is.character(variables))
    assert_that(all(variables %in% colnames(df)))
  }

  if (!is.null(weights)) {
    assert_that(is.string(weights))
    assert_that(is.numeric(df[[weights]]))
    assert_that(all(!is.na(df[[weights]])))
    assert_that(all(df[[weights]] > 0))
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

  if (length(cat_vars) == 0 & length(cont_vars) == 0) {
    stop("No valid variable types")
  }

  if (length(cat_vars) == 0) {
    cont_tbl <- mean_sd_func(df, cont_vars, treatment, weights)
    return(cont_tbl)
  }

  if (length(cont_vars) == 0) {
    cat_tbl <- cat_func(df, cat_vars, treatment, weights)
    return(cat_tbl)
  }

  cont_tbl <- mean_sd_func(df, cont_vars, treatment, weights)
  cat_tbl <- cat_func(df, cat_vars, treatment, weights)

  bind_rows(cat_tbl, cont_tbl) %>%
    tidyr::replace_na(list(Label = ""))

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
cat_func <- function(df, cat_vars, treatment, weights) {

  cat_vars <- syms(cat_vars)

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
      select(!!!cat_vars, obs_weights) %>%
      gather(
        key = Variable,
        value = Label,
        !!!cat_vars
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

    treatment <- sym(treatment)

    df <- df %>%
      drop_na(!!treatment)

    p_chi_fisher <- possibly(p_chi_fisher, NA_real_)

    df %>%
      select(!!treatment, !!!cat_vars, obs_weights) %>%
      gather(
        key = Variable,
        value = Label,
        !!!cat_vars
      ) %>%
      group_by(!!treatment, Variable, Label) %>%
      summarise(weighted_count = sum(obs_weights)) %>%
      ungroup() %>%
      drop_na() %>%
      group_by(!!treatment, Variable) %>%
      mutate(Statistics = proportions(weighted_count, weighted = weighted)) %>%
      ungroup() %>%
      select(-weighted_count) %>%
      spread(
        key = !!treatment,
        value = Statistics
      ) %>%
      mutate(`P Value` = map_dbl(Variable, p_chi_fisher, df = df, treatment = quo_name(treatment), weight_var = "obs_weights"))

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
mean_sd_func <- function(df, cont_vars, treatment, weights) {

  if (!is.null(treatment)) {

    treatment <- sym(treatment)

    df <- df %>%
      drop_na(!!treatment)

  }

  if (is.null(weights)) {

    df <- df %>%
      mutate(obs_weights = 1)

  } else {

    df <- df %>%
      rename(obs_weights = weights)

  }

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
      mutate(mean_sd = paste0(round(Mean, 2), " (", round(SD, 2), ")")) %>%
      select(-Mean, -SD) %>%
      rename(Statistics = mean_sd)

  } else {

    p_anova <- possibly(p_anova, NA_real_)

    df %>%
      group_by(!!treatment) %>%
      summarise_at(
        .vars = var_named,
        .funs = list(
          "Mean" = ~weighted_mean(.x, obs_weights),
          "SD" = ~weighted_sd(.x, obs_weights)
        )
      ) %>%
      gather("Variable", "val", -!!treatment) %>%
      separate(Variable, c("Variable", "mean_sd"), "_(?!.*_)") %>%
      spread(key = mean_sd, value = val) %>%
      mutate(mean_sd = paste0(round(Mean, 2), " (", round(SD, 2), ")")) %>%
      select(-Mean, -SD) %>%
      spread(!!treatment, mean_sd) %>%
      mutate(`P Value` = map_dbl(Variable, p_anova, df = df, treatment = quo_name(treatment), weight_var = "obs_weights"))

  }
}
