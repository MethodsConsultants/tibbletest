#' Helper function which calculates p-value via chi-square or fisher
#'
#' @param df <`tbl_df`> Dataframe that has variable and treatment columns of interest
#' @param var <`character(1)`> Name of variable column
#' @param treatment <`character(1)`> Name of treatment column
#'
#' @examples
#'
#' p_chi_fisher(mtcars, var = "am", treatment = "vs")
#' @return <`numeric(1)`> p-value
#'
#' @importFrom stats chisq.test fisher.test
#'
#' @noRd
p_chi_fisher <- function(df, var, treatment) {

  chisq_wrapper <- function(var, df, treatment) {

    table <- chisq.test(
      x = as.factor(df[[var]]),
      y = as.factor(df[[treatment]])
    )

    return(table$p.value)

  }

  fisher_wrapper <- function(var, df, treatment) {

    table <- fisher.test(
      x = as.factor(df[[var]]),
      y = as.factor(df[[treatment]]),
      simulate.p.value = TRUE
    )

    return(table$p.value)

  }

  chisq_wrapper <- purrr::quietly(chisq_wrapper)
  chisq <- chisq_wrapper(var, df, treatment)

  if (length(chisq$warnings) == 0) {

    return(chisq$result)

  } else {

    return(fisher_wrapper(var, df, treatment))

  }
}

#' Helper function which calculates p-value via anova
#'
#' @inheritParams p_chi_fisher
#'
#' @return <`numeric(1)`> p-value
#'
#' @import dplyr
#' @importFrom stats as.formula lm anova
#'
#' @noRd
p_anova <- function(df, var, treatment) {

  form <- as.formula(paste0(var, " ~ ", treatment))

  lm(form, df) %>%
    anova() %>%
    pull(`Pr(>F)`) %>%
    purrr::pluck(1)

}

#' Helper function which returns whether or not the column is numeric
#'
#' @inheritParams summary_cat
#' @param var <`character(1)`> Name of variable in dataframe.
#'
#' @return <`logical(1)`>
#'
#' @importFrom stats na.omit
#'
#' @keywords internal
#' @noRd
is_numeric_variable <- function(df, var) {

  var <- df %>%
    pull(var)

  vals <- var %>%
    na.omit() %>%
    unique()

  logical <- vals %>%
    `%in%`(c(0, 1)) %>%
    all()

  if (length(vals) == 0) {
    logical <- FALSE
  }

  if (logical) {
    return(FALSE)
  }

  purrr::is_bare_numeric(var)

}

#' Helper function which returns whether or not the column is categorical
#'
#' @inheritParams is_numeric_variable
#'
#' @return <`logical(1)`>
#'
#' @importFrom stats na.omit
#'
#' @keywords internal
#' @noRd
is_categorical_variable <- function(df, var) {

  var <- df %>%
    pull(var)

  vals <- var %>%
    na.omit() %>%
    unique()

  logical <- vals %>%
    `%in%`(c(0, 1)) %>%
    all()

  if (length(vals) == 0) {
    logical <- FALSE
  }

  if (logical) {
    return(TRUE)
  }

  (purrr::is_bare_character(var) | purrr::is_bare_logical(var) | is.factor(var))

}

#' Helper function which calculates proportions and outputs in "N (%)" format
#'
#' @param int_vec <`integer`> Vector of counts.
#'
#' @return <`character`> Count and proportion "N (%)"
#'
#' @keywords internal
#' @noRd
proportions <- function(int_vec) {

  total <- sum(int_vec, na.rm = TRUE)
  probs <- round(int_vec / total * 100, 2)

  probs <- case_when(
    is.na(probs) ~ 0,
    TRUE ~ probs
  )

  int_vec <- case_when(
    is.na(int_vec) ~ 0L,
    TRUE ~ int_vec
  )

  paste0(as.character(int_vec), " (", as.character(probs), "%)")

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
