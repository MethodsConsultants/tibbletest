#' Formats the descriptives table outputted by `descriptives`.
#'
#' Allows for easy formatting of summary statistics table. Treatment group counts are added to the column names,
#' duplicated categorical variable names and p-values are removed, and p-values are rounded.
#'
#' @param obj <`tbl_df`> Tibble outputted by `descriptives`.
#' @param add_Ns <`logical(1)`> Should treatment counts be added to column names?
#' @param remove_duplicates <`logical(1)`> Should repeated variable names and p-values for categorical variables be removed?
#' If TRUE, variable names and p-values will be presented only once on their own row.
#' @param remove_false <`logical(1)`> Should binary variables only show the N and percent of TRUE?
#'
#' @return <`tbl_df`> Formatted tibble
#'
#' @import assertthat
#' @import dplyr
#'
#' @export
format_tbl <- function(obj, ...) {
  UseMethod("format_tbl")
}

#' Formats the descriptives table outputted by `descriptives`.
#'
#' @inheritParams format_tbl
#' @param p_val_digits <`integer(1)`> Number of digits to round p-values.
#'
#' @exportS3Method format_tbl descriptives
format_tbl.descriptives <- function(obj, add_Ns = TRUE, remove_duplicates = TRUE, remove_false = TRUE, p_val_digits = 3L) {

  assert_that(!is.null(attr(obj, "counts")))

  obj_attr <- attr(obj, "counts")

  if ("P Value" %in% colnames(obj)) {

    obj <- obj %>%
      mutate(
        `P Value` = case_when(
          `P Value` < 1 / (10 ^ p_val_digits) ~ paste0("< ", 1 / (10 ^ p_val_digits)),
          TRUE ~ as.character(round(`P Value`, p_val_digits))
        )
      )

  }

  split_by_var <- obj %>%
    mutate(Variable = factor(Variable, levels = unique(Variable))) %>%
    group_by(Variable) %>%
    group_split()

  format_one_variable <- function(tbl) {

    tbl <- tbl %>%
      mutate(Variable = as.character(Variable))

    if ("Label" %in% colnames(tbl)) {
      if (all(tbl$Label %in% c("TRUE", "FALSE")) & remove_false) {

        tbl <- tbl %>%
          filter(Label == "TRUE") %>%
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
        select(-Label)

    }
  }

  if (!is.null(obj_attr) & add_Ns) {

    name_df <- obj_attr %>%
      mutate(
        new = paste0(label, " (N=", n, ")")
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

#' Formats the descriptives table outputted by `covariate_balance`.
#'
#' @inheritParams format_tbl
#' @param std_diff_digits <`integer(1)`> Number of digits to round standardized differences.
#'
#' @exportS3Method format_tbl covariate_balance
format_tbl.covariate_balance <- function(obj, add_Ns = TRUE, remove_duplicates = TRUE, remove_false = TRUE, std_diff_digits = 1L) {

  assert_that(!is.null(attr(obj, "counts")))

  obj_attr <- attr(obj, "counts")

    obj <- obj %>%
      mutate(
        "Absolute Standardized Difference (%)" = as.character(round(`Absolute Standardized Difference (%)`, std_diff_digits))
      )

  split_by_var <- obj %>%
    mutate(Variable = factor(Variable, levels = unique(Variable))) %>%
    group_by(Variable) %>%
    group_split()

  format_one_variable <- function(tbl) {

    tbl <- tbl %>%
      mutate(Variable = as.character(Variable))

    if ("Label" %in% colnames(tbl)) {
      if (all(tbl$Label %in% c("TRUE", "FALSE")) & remove_false) {

        tbl <- tbl %>%
          filter(Label == "TRUE") %>%
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
        select(-Label)

    }
  }

  if (!is.null(obj_attr) & add_Ns) {

    name_df <- obj_attr %>%
      mutate(
        new = paste0(label, " (N=", n, ")")
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
      Variable,
      `Absolute Standardized Difference (%)`
    ) %>%
    mutate(`Absolute Standardized Difference (%)` = as.numeric(`Absolute Standardized Difference (%)`)) %>%
    tidyr::drop_na() %>%
    distinct() %>%
    rename(
      "Unweighted" = `Absolute Standardized Difference (%)`
    )

  adj <- adj %>%
    select(
      Variable,
      `Absolute Standardized Difference (%)`
    ) %>%
    mutate(`Absolute Standardized Difference (%)` = as.numeric(`Absolute Standardized Difference (%)`)) %>%
    tidyr::drop_na() %>%
    distinct() %>%
    rename(
      "Weighted" = `Absolute Standardized Difference (%)`
    )

  plot_dat <- inner_join(
    unadj,
    adj,
    by = "Variable"
  ) %>%
    arrange(desc(Unweighted)) %>%
    mutate(Variable = fct_rev(as_factor(Variable))) %>%
    pivot_longer(
      cols = -Variable,
      names_to = "type",
      values_to = "std_diff"
    )

  plot_dat %>%
    ggplot(
      aes(x = Variable, y = std_diff, color = type)
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
