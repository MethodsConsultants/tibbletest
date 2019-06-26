#' Helper function which calculates proportions and outputs in "N (%)" format
#'
#' @param count_vec <`numeric`> Vector of counts.
#' @param weighted <`logical(1)`> Are these weighted counts?
#'
#' @return <`character`> Count and proportion "N (%)"
#'
#' @keywords internal
#' @noRd
proportions <- function(count_vec, weighted) {

  if (weighted) {

    total <- sum(count_vec, na.rm = TRUE)
    probs <- round(count_vec / total * 100, 2)

    probs <- case_when(
      is.na(probs) ~ 0,
      TRUE ~ probs
    )

    percentage <- paste0(as.character(probs), "%")
    return(percentage)

  }

  total <- sum(count_vec, na.rm = TRUE)
  probs <- round(count_vec / total * 100, 2)

  probs <- case_when(
    is.na(probs) ~ 0,
    TRUE ~ probs
  )

  count_vec <- case_when(
    is.na(count_vec) ~ 0,
    TRUE ~ count_vec
  )

  paste0(as.character(count_vec), " (", as.character(probs), "%)")

}


#' Converts yes/no or 0/1 vector to logical
#'
#' @inheritParams is_numeric_variable
#'
#' @import dplyr
#' @import stringr
#'
#' @keywords internal
#' @noRd
convert_logical <- function(x) {

  # If "yes"/"no" or "true"/"false" variable, convert to logical.
  if (x %>% is.character()) {

    vals <- x %>%
      stats::na.omit() %>%
      unique()

    if (length(vals) > 0) {

      is_yes_no <- vals %>%
        str_trim() %>%
        str_to_lower() %>%
        `%in%`(c("no", "yes")) %>%
        all()

      if (is_yes_no) {

        x <- x %>%
          tibble::enframe(name = NULL) %>%
          mutate(value = str_to_lower(str_trim(value)),
                 value = case_when(
                   value == "yes" ~ TRUE,
                   value == "no" ~ FALSE,
                   TRUE ~ NA
                 )) %>%
          pull(value)
      }

      is_true_false_str <- vals %>%
        str_trim() %>%
        str_to_lower() %>%
        `%in%`(c("true", "false")) %>%
        all()

      if (is_true_false_str) {

        x <- x %>%
          enframe(name = NULL) %>%
          mutate(value = str_to_lower(str_trim(value)),
                 value = case_when(
                   value == "true" ~ TRUE,
                   value == "false" ~ FALSE,
                   TRUE ~ NA
                 )) %>%
          pull(value)
      }
    }
  }

  # If 0/1 variable, convert to logical.
  if (x %>% is.numeric()) {

    vals <- x %>%
      as.character() %>%
      stats::na.omit() %>%
      unique()

    if (length(vals) > 0) {

      is_0_1 <- vals %>%
        `%in%`(c("0", "1")) %>%
        all()

      if (is_0_1) {

        x <- x %>%
          tibble::enframe(name = NULL) %>%
          mutate(value = case_when(
            value == 1 ~ TRUE,
            value == 0 ~ FALSE,
            TRUE ~ NA
          )) %>%
          pull(value)
      }
    }
  }

  x

}
