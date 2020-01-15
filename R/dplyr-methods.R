#' @export
mutate.tbl_test <- function(.data, ...) {

  out <- NextMethod()
  attr(out, "counts") <- attr(.data, "counts")
  out

}
