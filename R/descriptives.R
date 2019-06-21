#' Creates a table of summary statistics
#'
#' Generates a table with summary statistics for each variable, optionally split by a grouping variable.
#'
#' @param df <`tbl_df`> Data frame with treatment and variables of interest as columns.
#' @param variables <`character`> Character vector of column names. If left blank, will be inferred from data.
#' @param treatment <`character(1)`> String with name of treatment column. If left blank, will produce univariate summary statistics.
#'
#' @return <`tbl_df`> Tibble with summary statistics split by treatment (optional).
#'
#' @import dplyr
#' @import purrr
#'
#' @export
descriptives <- function(df, treatment = NULL, variables = NULL) {

  if (is.null(variables)) {
    variables <- colnames(df)
    variables <- variables[!(variables %in% treatment)]
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
    cont_tbl <- mean_sd_func(df, cont_vars, treatment)
    return(cont_tbl)
  }

  if (length(cont_vars) == 0) {
    cat_tbl <- cat_func(df, cat_vars, treatment)
    return(cat_tbl)
  }

  cont_tbl <- mean_sd_func(df, cont_vars, treatment)
  cat_tbl <- cat_func(df, cat_vars, treatment)

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
cat_func <- function(df, cat_vars, treatment) {

  cat_vars <- syms(cat_vars)

  df <- df %>%
    mutate_if(is.factor, as.character)

  if (is.null(treatment)) {

    df %>%
      count(!!!cat_vars) %>%
      gather(
        key = Variable,
        value = Label,
        !!!cat_vars
      ) %>%
      group_by(Variable, Label) %>%
      summarise_all(sum) %>%
      ungroup() %>%
      drop_na() %>%
      group_by(Variable) %>%
      mutate(Statistics = proportions(n)) %>%
      ungroup() %>%
      select(-n)

  } else {

    treatment <- sym(treatment)

    df <- df %>%
      drop_na(!!treatment)

    p_chi_fisher <- possibly(p_chi_fisher, NA_real_)

    df %>%
      count(!!treatment, !!!cat_vars) %>%
      gather(
        key = Variable,
        value = Label,
        !!!cat_vars
      ) %>%
      group_by(!!treatment, Variable, Label) %>%
      summarise_all(sum) %>%
      ungroup() %>%
      drop_na() %>%
      group_by(!!treatment, Variable) %>%
      mutate(Statistics = proportions(n)) %>%
      ungroup() %>%
      select(-n) %>%
      spread(
        key = !!treatment,
        value = Statistics
      ) %>%
      mutate(`P Value` = map_dbl(Variable, p_chi_fisher, df = df, treatment = quo_name(treatment)))

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
mean_sd_func <- function(df, cont_vars, treatment) {

  Mean <- partial(mean, na.rm = TRUE)
  SD <- partial(sd, na.rm = TRUE)

  ## Set names to force consistent syntax in summarise_at
  var_named <- cont_vars %>%
    set_names()

  if (is.null(treatment)) {

    df %>%
      summarise_at(var_named, list("Mean" = Mean, "SD" = SD)) %>%
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

    treatment <- sym(treatment)

    df <- df %>%
      drop_na(!!treatment)

    p_anova <- possibly(p_anova, NA_real_)

    df %>%
      group_by(!!treatment) %>%
      summarise_at(var_named, list("Mean" = Mean, "SD" = SD)) %>%
      gather("Variable", "val", -!!treatment) %>%
      separate(Variable, c("Variable", "mean_sd"), "_(?!.*_)") %>%
      spread(key = mean_sd, value = val) %>%
      mutate(mean_sd = paste0(round(Mean, 2), " (", round(SD, 2), ")")) %>%
      select(-Mean, -SD) %>%
      spread(!!treatment, mean_sd) %>%
      mutate(`P Value` = map_dbl(Variable, p_anova, df = df, treatment = quo_name(treatment)))

  }
}
