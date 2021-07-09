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

  mean_diff <- summary_tbl$mean[1] - summary_tbl$mean[2]

  pooled_sd <- sqrt(
    (summary_tbl$sd[1] ^ 2 + summary_tbl$sd[2] ^ 2) / 2
  )

  abs(100 * mean_diff / pooled_sd)

}

#' Helper function which calculates the absolute standardize difference of a binary variable between two groups
#'
#' @inheritParams std_diff_continuous
#'
#' @return <`numeric(1)`> Standardized Difference
#'
#' @import dplyr
#'
#' @noRd
std_diff_binary <- function(df, var, treatment, weight_var) {

  df <- df %>%
    drop_na(.data[[var]], .data[[treatment]])

  summary_tbl <- df %>%
    group_by(.data[[treatment]]) %>%
    summarise_at(
      var,
      c(p = ~ weighted_mean(.x, weight_vec = .data[[weight_var]]))
    )

  prop_diff <- summary_tbl$p[1] - summary_tbl$p[2]

  var_1 <- summary_tbl$p[1] * (1 - summary_tbl$p[1])
  var_2 <- summary_tbl$p[2] * (1 - summary_tbl$p[2])

  pooled_sd <- sqrt(
    (var_1 + var_2) / 2
  )

  abs(100 * prop_diff / pooled_sd)

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
    slice(-1) %>%
    ungroup()

  weighted_props_list <- split(
    weighted_props$p,
    weighted_props[[treatment]]
  )

  covariance_matrices <- weighted_props_list %>%
    map(
      function(p) {
        vars <- p * (1 - p)
        covs <- -outer(p, p)
        diag(covs) <- vars
        drop(covs)
      }
    )

  covariance_mean <- reduce(covariance_matrices, `+`) / length(covariance_matrices)

  prop_diff <- weighted_props_list %>%
    reduce(`-`)

  as.numeric(100 * sqrt(t(prop_diff) %*% MASS::ginv(covariance_mean) %*% prop_diff))

}
