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

  int_vec <- case_when(
    is.na(count_vec) ~ 0,
    TRUE ~ count_vec
  )

  paste0(as.character(count_vec), " (", as.character(probs), "%)")

}
