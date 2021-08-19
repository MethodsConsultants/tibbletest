#' Creates a data frame which presents a descriptive comparison of variables across a grouping variable.
#'
#' \code{descriptives()} creates a data frame which presents summary statistics stratified by a grouping variable.
#' It presents mean and SD (or median and IQR) for continuous variables and frequency and percentage for categorical variables.
#' P-values are also presented which test for significant differences in the variables across the groups. The output of this
#' function is intended to be passed to the \code{\link{format_tbl}} function to clean up the table to be used in a report.
#'
#' @section Summary Statistics: By default the mean and SD for continuous variables and frequency and percentage
#' for categorical variables are presented. By specifying a continuous variable in the \code{nonparametric}
#' argument, median and interquartile range (IQR) will be presented instead for that variable.
#'
#' @section Significance Testing: If a treatment group column is specified in the \code{treatment} argument, significance testing
#' will be performed. For continuous variables, p-values from a one-way ANOVA are presented. Note that if there are only two treatment
#' groups this is equivalent to a t-test. If a continuous variable is specified in the \code{nonparametric} argument, then a Kruskal-Wallis
#' test is performed instead. Note that if there are only two treatment groups this is equivalent to the Mann-Whitney test.
#' For categorical variables, either a chi-squared or Fisher's exact test is performed depending on the distribution of the variable.
#'
#' @section Weights: By specifying a column in the \code{weights} argument, weighted summary statistics and p-values will be presented.
#' Weighted p-values are all calculated using the \code{survey} package. \code{\link[survey]{svychisq}}, \code{\link[survey]{regTermTest}}, and
#' \code{\link[survey]{svyranktest}} are used for weighted versions of the chi-squared, ANOVA, and Kruskal-Wallis respectively.
#'
#' @param df <`tbl_df`> Data frame with treatment and variables of interest as columns.
#' @param variables <`tidy-select`> Columns to summarize in table. If left blank, will be inferred from data.
#' @param treatment <`tidy-select`> Treatment column. If left blank, will produce univariate summary statistics.
#' @param weights <`tidy-select`> Column with observation weights. If left blank, will not use observation weights.
#' @param nonparametric <`tidy-select`> Columns of continuous variables to calculate median/IQR/non-parametric test.
#'
#' @name descriptives
NULL
#> NULL

#' @rdname descriptives
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

  if (length(cat_vars) == 0) {

    cont_tbl <- cont_descriptives(df, cont_vars, treatment, weights, nonparametric, test_type = "p")

    cont_tbl <- cont_tbl %>%
      mutate(
        Variable = factor(
          .data$Variable,
          levels = unique(c(variables, nonparametric))
        )
      ) %>%
      arrange(.data$Variable) %>%
      mutate(
        Variable = as.character(.data$Variable)
      )

    attr(cont_tbl, "counts") <- count_attr
    class(cont_tbl) <- c("descriptives", class(cont_tbl))
    return(cont_tbl)

  }

  if (length(c(cont_vars, nonparametric)) == 0) {

    cat_tbl <- cat_descriptives(df, cat_vars, treatment, weights, test_type = "p")

    cat_tbl <- cat_tbl %>%
      mutate(
        Variable = factor(
          .data$Variable,
          levels = variables
        )
      ) %>%
      arrange(.data$Variable) %>%
      mutate(
        Variable = as.character(.data$Variable)
      )

    attr(cat_tbl, "counts") <- count_attr
    class(cat_tbl) <- c("descriptives", class(cat_tbl))
    return(cat_tbl)

  }

  cont_tbl <- cont_descriptives(df, cont_vars, treatment, weights, nonparametric, test_type = "p")
  cat_tbl <- cat_descriptives(df, cat_vars, treatment, weights, test_type = "p")

  tbl <- bind_rows(cat_tbl, cont_tbl) %>%
    tidyr::replace_na(list(Label = ""))

  tbl <- tbl %>%
    mutate(
      Variable = factor(
        .data$Variable,
        levels = unique(c(variables, nonparametric))
      )
    ) %>%
    arrange(.data$Variable) %>%
    mutate(
      Variable = as.character(.data$Variable)
    )

  attr(tbl, "counts") <- count_attr
  class(tbl) <- c("descriptives", class(tbl))
  tbl

}

#' @rdname descriptives
#'
#' @import dplyr
#' @import purrr
#' @import assertthat
#'
#' @export
covariate_balance <- function(df, treatment = NULL, variables = NULL, weights = NULL, nonparametric = NULL) {

  assert_that(is.data.frame(df))
  assert_that(!missing(treatment))

  treatment <- df %>%
    select({{ treatment }}) %>%
    colnames()
  assert_that(is_categorical_variable(df, treatment))

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

  count_attr <- df %>%
    tidyr::drop_na(.data[[treatment]]) %>%
    count(.data[[treatment]]) %>%
    rename(label = treatment)

  if (length(cat_vars) == 0) {

    cont_tbl <- cont_descriptives(df, cont_vars, treatment, weights, nonparametric, test_type = "std_diff")

    cont_tbl <- cont_tbl %>%
      mutate(
        Variable = factor(
          .data$Variable,
          levels = unique(c(variables, nonparametric))
        )
      ) %>%
      arrange(.data$Variable) %>%
      mutate(
        Variable = as.character(.data$Variable)
      )

    attr(cont_tbl, "counts") <- count_attr
    class(cont_tbl) <- c("covariate_balance", class(cont_tbl))
    return(cont_tbl)

  }

  if (length(c(cont_vars, nonparametric)) == 0) {

    cat_tbl <- cat_descriptives(df, cat_vars, treatment, weights, test_type = "std_diff")

    cat_tbl <- cat_tbl %>%
      mutate(
        Variable = factor(
          .data$Variable,
          levels = variables
        )
      ) %>%
      arrange(.data$Variable) %>%
      mutate(
        Variable = as.character(.data$Variable)
      )

    attr(cat_tbl, "counts") <- count_attr
    class(cat_tbl) <- c("covariate_balance", class(cat_tbl))
    return(cat_tbl)

  }

  cont_tbl <- cont_descriptives(df, cont_vars, treatment, weights, nonparametric, test_type = "std_diff")
  cat_tbl <- cat_descriptives(df, cat_vars, treatment, weights, test_type = "std_diff")

  tbl <- bind_rows(cat_tbl, cont_tbl) %>%
    tidyr::replace_na(list(Label = ""))

  tbl <- tbl %>%
    mutate(
      Variable = factor(
        .data$Variable,
        levels = unique(c(variables, nonparametric))
      )
    ) %>%
    arrange(.data$Variable) %>%
    mutate(
      Variable = as.character(.data$Variable)
    )

  attr(tbl, "counts") <- count_attr
  class(tbl) <- c("covariate_balance", class(tbl))
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
cat_descriptives <- function(df, cat_vars, treatment, weights, test_type) {

  df <- df %>%
    mutate_at(
      vars(
        any_of(cat_vars), treatment
      ),
      as.character
    )

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
      select(any_of(cat_vars), .data$obs_weights) %>%
      pivot_longer(
        cols = any_of(cat_vars),
        names_to = "Variable",
        values_to = "Label"
      ) %>%
      group_by(.data$Variable, .data$Label) %>%
      summarise(weighted_count = sum(.data$obs_weights)) %>%
      ungroup() %>%
      drop_na() %>%
      group_by(.data$Variable) %>%
      mutate(Statistics = proportions(.data$weighted_count, weighted = weighted)) %>%
      ungroup() %>%
      select(-.data$weighted_count)

  } else {

    df <- df %>%
      drop_na(.data[[treatment]])

    tbl <- df %>%
      select(.data[[treatment]], any_of(cat_vars), .data$obs_weights) %>%
      pivot_longer(
        cols = any_of(cat_vars),
        names_to = "Variable",
        values_to = "Label"
      ) %>%
      group_by(.data[[treatment]], .data$Variable, .data$Label) %>%
      summarise(weighted_count = sum(.data$obs_weights)) %>%
      ungroup() %>%
      drop_na() %>%
      tidyr::complete(
        .data[[treatment]],
        tidyr::nesting(Variable, Label)
      ) %>%
      group_by(.data[[treatment]], .data$Variable) %>%
      mutate(Statistics = proportions(.data$weighted_count, weighted = weighted)) %>%
      ungroup() %>%
      select(-.data$weighted_count) %>%
      pivot_wider(
        names_from = .data[[treatment]],
        values_from = "Statistics"
      )

    if (test_type == "p") {

      p_chi_fisher <- possibly(p_chi_fisher, NA_real_)

      tbl %>%
        mutate(
          `P Value` = map_dbl(
            .data$Variable,
            p_chi_fisher,
            df = df,
            treatment = treatment,
            weight_var = "obs_weights"
          )
        )
    } else if (test_type == "std_diff") {

      std_diff_categorical <- possibly(std_diff_categorical, NA_real_)

      tbl %>%
        mutate(
          "Absolute Standardized Difference (%)" = map_dbl(
            .data$Variable,
            std_diff_categorical,
            df = df,
            treatment = treatment,
            weight_var = "obs_weights"
          )
        )
    }
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
cont_descriptives <- function(df, cont_vars, treatment, weights, nonparametric, test_type) {

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

    parametric_tbl <- mean_sd_descriptives(df, cont_vars, treatment, test_type)
    return(parametric_tbl)

  }

  cont_vars <- cont_vars[!(cont_vars %in% nonparametric)]

  if (length(cont_vars) == 0) {

    nonparametric_tbl <- median_IQR_descriptives(df, nonparametric, treatment, test_type)
    return(nonparametric_tbl)

  }

  parametric_tbl <- mean_sd_descriptives(df, cont_vars, treatment, test_type)
  nonparametric_tbl <- median_IQR_descriptives(df, nonparametric, treatment, test_type)

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
mean_sd_descriptives <- function(df, cont_vars, treatment, test_type) {

  ## Set names to force consistent syntax in summarise_at
  var_named <- cont_vars %>%
    set_names()

  if (is.null(treatment)) {

    df %>%
      summarise_at(
        .vars = var_named,
        .funs = list(
          "Mean" = ~weighted_mean(.x, .data$obs_weights),
          "SD" = ~weighted_sd(.x, .data$obs_weights)
        )
      ) %>%
      pivot_longer(
        cols = everything(),
        names_to = c("Variable", "mean_sd"),
        names_sep = "_(?!.*_)",
        values_to = "val"
      ) %>%
      pivot_wider(
        names_from = .data$mean_sd,
        values_from = .data$val
      ) %>%
      mutate(Statistics = paste0(round(.data$Mean, 2), " (", round(.data$SD, 2), ")")) %>%
      select(-.data$Mean, -.data$SD)

  } else {

    tbl <- df %>%
      group_by(.data[[treatment]]) %>%
      summarise_at(
        .vars = var_named,
        .funs = list(
          "Mean" = ~weighted_mean(.x, .data$obs_weights),
          "SD" = ~weighted_sd(.x, .data$obs_weights)
        )
      ) %>%
      pivot_longer(
        cols = -.data[[treatment]],
        names_to = c("Variable", "mean_sd"),
        names_sep = "_(?!.*_)",
        values_to = "val"
      ) %>%
      pivot_wider(
        names_from = .data$mean_sd,
        values_from = .data$val
      ) %>%
      mutate(mean_sd = paste0(round(.data$Mean, 2), " (", round(.data$SD, 2), ")")) %>%
      select(-.data$Mean, -.data$SD) %>%
      pivot_wider(
        names_from = .data[[treatment]],
        values_from = .data$mean_sd
      )

    if (test_type == "p") {

      p_anova <- possibly(p_anova, NA_real_)

      tbl %>%
        mutate(
          `P Value` = map_dbl(
            .data$Variable,
            p_anova,
            df = df,
            treatment = treatment,
            weight_var = "obs_weights"
          )
        )
    } else if (test_type == "std_diff") {

      std_diff_continuous <- possibly(std_diff_continuous, NA_real_)

      tbl %>%
        mutate(
          "Absolute Standardized Difference (%)" = map_dbl(
            .data$Variable,
            std_diff_continuous,
            df = df,
            treatment = treatment,
            weight_var = "obs_weights"
          )
        )
    }
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
median_IQR_descriptives <- function(df, cont_vars, treatment, test_type) {

  ## Set names to force consistent syntax in summarise_at
  var_named <- cont_vars %>%
    set_names()

  if (is.null(treatment)) {

    df %>%
      summarise_at(
        .vars = var_named,
        .funs = list(
          "Median" = ~weighted_quantile(.x, .data$obs_weights, 0.5),
          "Q1" = ~weighted_quantile(.x, .data$obs_weights, 0.25),
          "Q3" = ~weighted_quantile(.x, .data$obs_weights, 0.75)
        )
      ) %>%
      pivot_longer(
        cols = everything(),
        names_to = c("Variable", "median_iqr"),
        names_sep = "_(?!.*_)",
        values_to = "val"
      ) %>%
      pivot_wider(
        names_from = .data$median_iqr,
        values_from = .data$val
      ) %>%
      mutate(Statistics = paste0(round(.data$Median, 2), " [", round(.data$Q1, 2), ", ", round(.data$Q3, 2), "]")) %>%
      select(-.data$Median, -.data$Q1, -.data$Q3)

  } else {

    tbl <- df %>%
      group_by(.data[[treatment]]) %>%
      summarise_at(
        .vars = var_named,
        .funs = list(
          "Median" = ~weighted_quantile(.x, .data$obs_weights, 0.5),
          "Q1" = ~weighted_quantile(.x, .data$obs_weights, 0.25),
          "Q3" = ~weighted_quantile(.x, .data$obs_weights, 0.75)
        )
      ) %>%
      pivot_longer(
        cols = -.data[[treatment]],
        names_to = c("Variable", "median_iqr"),
        names_sep = "_(?!.*_)",
        values_to = "val"
      ) %>%
      pivot_wider(
        names_from = .data$median_iqr,
        values_from = .data$val
      ) %>%
      mutate(median_iqr = paste0(round(.data$Median, 2), " [", round(.data$Q1, 2), ", ", round(.data$Q3, 2), "]")) %>%
      select(-.data$Median, -.data$Q1, -.data$Q3) %>%
      pivot_wider(
        names_from = .data[[treatment]],
        values_from = .data$median_iqr
      )

    if (test_type == "p") {

      p_kruskal <- possibly(p_kruskal, NA_real_)

      tbl %>%
        mutate(
          `P Value` = map_dbl(
            .data$Variable,
            p_kruskal,
            df = df,
            treatment = treatment,
            weight_var = "obs_weights"
          )
        )
    } else if (test_type == "std_diff") {

      std_diff_continuous <- possibly(std_diff_continuous, NA_real_)

      tbl %>%
        mutate(
          "Absolute Standardized Difference (%)" = map_dbl(
            .data$Variable,
            std_diff_continuous,
            df = df,
            treatment = treatment,
            weight_var = "obs_weights"
          )
        )
    }
  }
}
