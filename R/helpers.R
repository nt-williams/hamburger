
#' Combine bounds for presentation.
#'
#' @param df a dataframe or tibble
#' @param label name of the new column
#'
#' @return
#' @export
combine_ci <- function(df, label) {
  out <- unite(df, !! label, contains("low"), contains("high"), sep = ", ")
  out <- mutate(out, !! label := paste0("(", .data[[label]], ")"))
  return(out)
}

#' Format digits to be the same length.
#'
#' @param x a numeric object
#' @param n the number of digits to follow the decimal
#'
#' @return a character representation of the original input with the formatted digits
#' @export
#'
#' @examples
#' format_digits(1.34523435353, 2)
format_digits <- function(x, n) {
  format(round(as.double(x), digits = n), nsmall = n, trim = TRUE)
}

#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a numeric skimr table.
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
tidy.skim_df <- function(x, missing = FALSE, complete = TRUE, n = TRUE, mean = TRUE, sd = TRUE,
                         p0 = FALSE, p25 = FALSE, p50 = FALSE, p75 = FALSE, p100 = FALSE, hist = FALSE) {

  df <- as.data.frame(x)
  nn <- names(df)
  to_keep <- nn[!(nn %in% c("type", "level", "formatted"))]
  stats <- c("missing", "complete", "n", "mean", "sd", "p0", "p25", "p50", "p75", "p100", "hist")
  status <- c(missing, complete, n, mean, sd, p0, p25, p50, p75, p100, hist)
  want <- stats[status]

  out <- lapply(want, function(x) df[df$stat == x, to_keep])
  out <- lapply(out, function(x) tidyr::pivot_wider(x, names_from = stat, values_from = value))

  tibble::as_tibble(Reduce(merge, out))
}

#' Wrap text in paranthesis.
#'
#' @param x a character or numeric value
#'
#' @return a character representation of the original input surrounded in parenthesis
#' @export
#'
#' @examples
#' wrap_parenthesis(55)
wrap_parenthesis <- function(x) {
  paste0("(", x, ")")
}

#' Find table indexes from an html document matching patterns.
#'
#' @param x An object of class \code{xml_document}
#' @param pattern Pattern to search for within html nodes of CSS class "table"
#'
#' @return a vector of indexes that indicates nodes matching the pattern.
#' @export
search_tables <- function(x, pattern) {
  nodes <- as.character(html_nodes(x, css = "table"))
  i <- grep(pattern, nodes)
  return(i)
}

#' Parse an html table into a tibble.
#'
#' @param x An object of class \code{xml_document}
#' @param i html node index to parse as a tibble
#'
#' @return The html node as a tibble.
#' @export
get_table <- function(x, i) {
  node <- rvest::html_table(rvest::html_nodes(x, css = "table")[[i]], fill = T)
  node_tbl <- tibble::as_tibble(node, .name_repair = "unique")
  return(node_tbl)
}
