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
