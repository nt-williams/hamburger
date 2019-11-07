
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
  format(round(as.double(x), digits = n), nsmall = n, trim = TRUE)
}

#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a skimr table
#'
#' @param x an object of class skim_df
#' @param missing return the missing column, logical
#' @param complete return the complete column, logical
#' @param n return the n column, logical
#' @param mean return the mean column, logical
#' @param sd return the sd column, logical
#' @param p0 return the p0 column, logical
#' @param p25 return the p25 column, logical
#' @param p50 return the p50 column, logical
#' @param p75 return the p75 column, logical
#' @param p100 return the p100 column, logical
#' @param hist return the hist column, logical
#'
#' @return
#' @export
#'
#' @examples
tidy.skim_df <- function(x, missing = FALSE, complete = TRUE, n = TRUE, mean = TRUE, sd = TRUE,
                         p0 = FALSE, p25 = FALSE, p50 = FALSE, p75 = FALSE, p100 = FALSE, hist = FALSE) {

  df <- as.data.frame(x)
  nn <- names(df)
  to_keep <- nn[!(nn %in% c("type", "level", "value"))]
  stats <- c("missing", "complete", "n", "mean", "sd", "p0", "p25", "p50", "p75", "p100", "hist")
  status <- c(missing, complete, n, mean, sd, p0, p25, p50, p75, p100, hist)
  want <- stats[status]

  out <- purrr::map(want, ~ {x[x$stat == .x, to_keep]})

  out <- purrr::map(out, ~ pivot_wider(.x,
                                       names_from = stat,
                                       values_from = formatted))

  suppressMessages(purrr::reduce(out, left_join))
}
