#' Formats the descriptives table outputted by `descriptives` or `covariate_balance`.
#'
#' Allows for easy formatting of summary statistics table. Treatment group counts are added to the column names,
#' duplicated categorical variable names removed, and TRUE/FALSE variables are collapsed to a single row. P-values
#' and standardized differences are rounded for `descriptives` and `covariate_balance` objects respectively.
#'
#' @param tbl <`tbl_df`> Tibble outputted by `descriptives` or `covariate_balance`.
#' @param \dots Additional arguments passed to/from other methods.
#'
#' @return <`tbl_df`> Formatted tibble
#'
#' @import assertthat
#' @import dplyr
#'
#' @export
format_tbl <- function(tbl, ...) {
  UseMethod("format_tbl")
}

#' @describeIn format_tbl Formats the descriptives table outputted by `descriptives`.
#'
#' @param tbl <`tbl_df`> Tibble outputted by `descriptives` or `covariate_balance`.
#' @param add_Ns <`logical(1)`> Should treatment counts be added to column names?
#' @param remove_duplicates <`logical(1)`> Should repeated variable names and p-values for categorical variables be removed?
#' If TRUE, variable names and p-values will be presented only once on their own row.
#' @param remove_false <`logical(1)`> Should binary variables only show the N and percent of TRUE?
#' @param p_val_digits <`integer(1)`> Number of digits to round p-values.
#' @param \dots Additional arguments passed to/from other methods.
#'
#' @export
format_tbl.descriptives <- function(tbl, add_Ns = TRUE, remove_duplicates = TRUE, remove_false = TRUE, p_val_digits = 3L, ...) {

  assert_that(!is.null(attr(tbl, "counts")))

  tbl_attr <- attr(tbl, "counts")

  if ("P Value" %in% colnames(tbl)) {

    tbl <- tbl %>%
      mutate(
        `P Value` = case_when(
          .data$`P Value` < 1 / (10 ^ p_val_digits) ~ paste0("< ", 1 / (10 ^ p_val_digits)),
          .data$`P Value` > 1 - (1 / (10 ^ p_val_digits)) ~ paste0("> ", 1 - (1 / (10 ^ p_val_digits))),
          TRUE ~ as.character(round(.data$`P Value`, p_val_digits))
        )
      )

  }

  split_by_var <- tbl %>%
    mutate(Variable = factor(.data$Variable, levels = unique(.data$Variable))) %>%
    group_by(.data$Variable) %>%
    group_split()

  format_one_variable <- function(tbl) {

    tbl <- tbl %>%
      mutate(Variable = as.character(.data$Variable))

    if ("Label" %in% colnames(tbl)) {
      if (all(tbl$Label %in% c("TRUE", "FALSE")) & remove_false) {

        tbl <- tbl %>%
          filter(.data$Label == "TRUE") %>%
          mutate(Label = "")

        return(tbl)

      }
    }

    if (nrow(tbl) == 1) {
      return(tbl)
    }

    if (remove_duplicates) {

      blank_row <- rep("", ncol(tbl)) %>%
        set_names(colnames(tbl))

      if ("P Value" %in% colnames(tbl)) {

        out_tbl <- tbl %>%
          add_row(!!!blank_row, .before = 1) %>%
          mutate(
            Variable = c(tbl$Variable[1], rep("", nrow(tbl))),
            `P Value` = c(tbl$`P Value`[1], rep("", nrow(tbl)))
          )

        return(out_tbl)

      } else {

        out_tbl <- tbl %>%
          add_row(!!!blank_row, .before = 1) %>%
          mutate(
            Variable = c(tbl$Variable[1], rep("", nrow(tbl)))
          )

        return(out_tbl)

      }
    }

    return(tbl)

  }

  output <- split_by_var %>%
    map_dfr(format_one_variable)

  if ("Label" %in% colnames(output)) {
    if (all(output$Label == "")) {

      output <- output %>%
        select(-.data$Label)

    }
  }

  if (!is.null(tbl_attr) & add_Ns) {

    name_df <- tbl_attr %>%
      mutate(
        new = paste0(.data$label, " (N=", n, ")")
      )

    named_vector <- set_names(
      x = name_df$new,
      nm = name_df$label
    )

    output <- output %>%
      rename_at(vars(names(named_vector)), ~named_vector[.x])

  }

  output

}

#' @describeIn format_tbl Formats the descriptives table outputted by `covariate_balance`.
#'
#' @param tbl <`tbl_df`> Tibble outputted by `descriptives` or `covariate_balance`.
#' @param add_Ns <`logical(1)`> Should treatment counts be added to column names?
#' @param remove_duplicates <`logical(1)`> Should repeated variable names and p-values for categorical variables be removed?
#' If TRUE, variable names and p-values will be presented only once on their own row.
#' @param remove_false <`logical(1)`> Should binary variables only show the N and percent of TRUE?
#' @param std_diff_digits <`integer(1)`> Number of digits to round standardized differences.
#' @param \dots Additional arguments passed to/from other methods.
#'
#' @export
format_tbl.covariate_balance <- function(tbl, add_Ns = TRUE, remove_duplicates = TRUE, remove_false = TRUE, std_diff_digits = 1L, ...) {

  assert_that(!is.null(attr(tbl, "counts")))

  tbl_attr <- attr(tbl, "counts")

    tbl <- tbl %>%
      mutate(
        "Absolute Standardized Difference (%)" = as.character(round(.data$`Absolute Standardized Difference (%)`, std_diff_digits))
      )

  split_by_var <- tbl %>%
    mutate(Variable = factor(.data$Variable, levels = unique(.data$Variable))) %>%
    group_by(.data$Variable) %>%
    group_split()

  format_one_variable <- function(tbl) {

    tbl <- tbl %>%
      mutate(Variable = as.character(.data$Variable))

    if ("Label" %in% colnames(tbl)) {
      if (all(tbl$Label %in% c("TRUE", "FALSE")) & remove_false) {

        tbl <- tbl %>%
          filter(.data$Label == "TRUE") %>%
          mutate(Label = "")

        return(tbl)

      }
    }

    if (nrow(tbl) == 1) {
      return(tbl)
    }

    if (remove_duplicates) {

      blank_row <- rep("", ncol(tbl)) %>%
        set_names(colnames(tbl))

      out_tbl <- tbl %>%
        add_row(!!!blank_row, .before = 1) %>%
        mutate(
          Variable = c(tbl$Variable[1], rep("", nrow(tbl))),
          `Absolute Standardized Difference (%)` = c(tbl$`Absolute Standardized Difference (%)`[1], rep("", nrow(tbl)))
        )

      return(out_tbl)
    }

    return(tbl)

  }

  output <- split_by_var %>%
    map_dfr(format_one_variable)

  if ("Label" %in% colnames(output)) {
    if (all(output$Label == "")) {

      output <- output %>%
        select(-.data$Label)

    }
  }

  if (!is.null(tbl_attr) & add_Ns) {

    name_df <- tbl_attr %>%
      mutate(
        new = paste0(.data$label, " (N=", n, ")")
      )

    named_vector <- set_names(
      x = name_df$new,
      nm = name_df$label
    )

    output <- output %>%
      rename_at(vars(names(named_vector)), ~named_vector[.x])

  }

  output

}


#' Creates a Love plot comparing standardized differences before and after weighting
#'
#' @param unadj <`tbl_df`> Tibble outputted by `covariate_balance()` without weighting.
#' @param adj <`tbl_df`> Tibble outputted by `covariate_balance()` after propensity score weighting.
#'
#' @return <`ggplot`> ggplot2 Love plot
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
love_plot <- function(unadj, adj) {

  unadj <- unadj %>%
    select(
      .data$Variable,
      .data$`Absolute Standardized Difference (%)`
    ) %>%
    mutate(`Absolute Standardized Difference (%)` = as.numeric(.data$`Absolute Standardized Difference (%)`)) %>%
    tidyr::drop_na() %>%
    distinct() %>%
    rename(
      "Unweighted" = .data$`Absolute Standardized Difference (%)`
    )

  adj <- adj %>%
    select(
      .data$Variable,
      .data$`Absolute Standardized Difference (%)`
    ) %>%
    mutate(`Absolute Standardized Difference (%)` = as.numeric(.data$`Absolute Standardized Difference (%)`)) %>%
    tidyr::drop_na() %>%
    distinct() %>%
    rename(
      "Weighted" = .data$`Absolute Standardized Difference (%)`
    )

  plot_dat <- inner_join(
    unadj,
    adj,
    by = "Variable"
  ) %>%
    arrange(desc(.data$Unweighted)) %>%
    mutate(Variable = forcats::fct_rev(forcats::as_factor(.data$Variable))) %>%
    pivot_longer(
      cols = -.data$Variable,
      names_to = "type",
      values_to = "std_diff"
    )

  plot_dat %>%
    ggplot(
      aes(x = .data$Variable, y = .data$std_diff, color = .data$type)
    ) +
    geom_point(size = 2) +
    theme_bw() +
    coord_flip() +
    labs(
      x = NULL,
      y = "Absolute Standardized Difference (%)",
      color = NULL
    ) +
    theme(text = element_text(size = 14)) +
    geom_hline(yintercept=10, lty=2)

}
