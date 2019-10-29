
#' Combine bounds for presentation
#'
#' @param df a dataframe or tibble
#' @param label name of the new column
#'
#' @return
#' @export
#'
#' @examples
combine_ci <- function(df, label) {
  out <- unite(df, !! label, contains("low"), contains("high"), sep = ", ")
  out <- mutate(out, !! label := paste0("(", .data[[label]], ")"))
  return(out)
}

#' Format digits to be the same length
#'
#' @param x vector to which apply
#' @param n the number of digits to follow the decimal
#'
#' @return
#' @export
#'
#' @examples
format_digits <- function(x, n) {
  format(round(as.double(x), digits = n), nsmall = n)
}
