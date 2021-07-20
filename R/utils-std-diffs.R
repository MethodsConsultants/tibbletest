#' Helper function which calculates the absolute standardize difference of a continuous variable between two groups
#'
#' @param df <`tbl_df`> Dataframe that has variable and treatment columns of interest
#' @param var <`character(1)`> Name of variable column
#' @param treatment <`character(1)`> Name of treatment column
#' @param weight_var <`character(1)`> Name of variable with observation weights
#'
#' @return <`numeric(1)`> Standardized Difference
#'
#' @import dplyr
#'
#' @keywords internal
#' @noRd
std_diff_continuous <- function(df, var, treatment, weight_var) {

  df <- df %>%
    drop_na(.data[[var]], .data[[treatment]])

  summary_tbl <- df %>%
    group_by(.data[[treatment]]) %>%
    summarise_at(
      var,
      c(
        mean = ~ weighted_mean(.x, weight_vec = .data[[weight_var]]),
        sd = ~ weighted_sd(.x, weight_vec = .data[[weight_var]])
      )
    )

  calculate_std_diff <- function(x) {

    mean_diff <- x$mean[1] - x$mean[2]

    pooled_sd <- sqrt(
      (x$sd[1] ^ 2 + x$sd[2] ^ 2) / 2
    )

    abs(100 * mean_diff / pooled_sd)

  }

  combn(1:nrow(summary_tbl), 2, simplify = FALSE) %>%
    map(
      ~ summary_tbl %>%
        slice(.x)
    ) %>%
    map_dbl(
      calculate_std_diff
    ) %>%
    max()

}

#' Helper function which calculates the absolute standardize difference of a categorical variable between two groups
#'
#' @inheritParams std_diff_continuous
#'
#' @return <`numeric(1)`> Standardized Difference
#'
#' @import dplyr
#'
#' @noRd
std_diff_categorical <- function(df, var, treatment, weight_var) {

  weighted_props <- df %>%
    drop_na(.data[[treatment]], .data[[var]]) %>%
    group_by(.data[[treatment]], .data[[var]]) %>%
    summarise_at(
      weight_var,
      sum
    ) %>%
    mutate(p = .data[[weight_var]] / sum(.data[[weight_var]])) %>%
    ungroup()

  weighted_props_list <- split(
    weighted_props$p,
    weighted_props[[treatment]]
  )

  calculate_std_diff <- function(x) {

    prop_diff <- x[[1]] - x[[2]]

    var_1 <- x[[1]] * (1 - x[[1]])
    var_2 <- x[[2]] * (1 - x[[2]])

    pooled_sd <- sqrt(
      (var_1 + var_2) / 2
    )

    abs(100 * prop_diff / pooled_sd) %>%
      max()

  }

  combn(1:length(weighted_props_list), 2, simplify = FALSE) %>%
    map(
      ~ `[`(weighted_props_list, .x)
    ) %>%
    map_dbl(
      calculate_std_diff
    ) %>%
    max()

}
