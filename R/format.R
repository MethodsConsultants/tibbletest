#' Formats the descriptives table outputted by `descriptives`
#'
#' For categorical variables with multiple labels, variable name and p-value are moved to their own row and are not repeated. Additionally, p-values are rounded.
#'
#' @param obj <`tbl_df`> Tibble outputted by `descriptives`
#' @param p_val_digits <`integer(1)`> Number of digits to round p-values
#'
#' @return <`tbl_df`> Formatted tibble
#'
#' @import assertthat
#' @import dplyr
#'
#' @export
format_tbl <- function(obj, p_val_digits = 3L) {

  assert_that(p_val_digits >= 2)

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
    group_by(Variable) %>%
    group_split()

  format_one_variable <- function(tbl) {

    if (nrow(tbl) == 1) {
      return(tbl)
    }

    blank_row <- rep("", ncol(tbl)) %>%
      rlang::set_names(colnames(tbl))

    if ("P Value" %in% colnames(tbl)) {

      out_tbl <- tbl %>%
        add_row(!!!blank_row, .before = 1) %>%
        mutate(
          Variable = c(tbl$Variable[1], rep("", nrow(tbl))),
          `P Value` = c(tbl$`P Value`[1], rep("", nrow(tbl)))
        )

      return(out_tbl)

    }

    tbl %>%
      add_row(!!!blank_row, .before = 1) %>%
      mutate(
        Variable = c(tbl$Variable[1], rep("", nrow(tbl)))
      )

  }

  split_by_var %>%
    purrr::map_dfr(format_one_variable)

}
