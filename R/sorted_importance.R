# sorted_importance ------------------------------------------------------------

#' Importance of Strings
#'
#' Decreasingly sorted frequencies of strings, by default weighted by their
#' length. This function can be used to find the most "important"
#' folder paths in terms of frequency and length.
#'
#' @param x vector of character strings
#' @param weighted if \code{TRUE} (default) the frequencies of strings are
#'  multiplied by the corresponding string lengths
#'
#' @return named integer vector (of class table) containing the decreasingly
#'   sorted importance values of the elements in \code{x}. The importance of a
#'   string is either its frequency in \code{x} (if weighted is FALSE) or the
#'   product of this frequency and the string length (if weighted is TRUE)
#'
#' @examples
#' strings <- c("a", "a", "a", "bc", "bc", "cdefg")
#'
#' (importance <- kwb.pathdict:::sorted_importance(strings))
#'
#' # Check that each input element is mentioned in the output
#' all(unique(strings) %in% names(importance))
#'
#' # weighted = FALSE just returns the frequencies of strings in x
#' (importance <- kwb.pathdict:::sorted_importance(strings, weighted = FALSE))
#'
#' # Check if the sum of frequencies is the number of elements in x
#' sum(importance) == length(strings)
#'
#' # You may use the function to assess the "importance" of directory paths
#' kwb.pathdict:::sorted_importance(dirname(kwb.pathdict:::example_paths()))
#'
sorted_importance <- function(x, weighted = TRUE)
{
  freq <- table(x)

  sort(if (weighted) nchar(names(freq)) * freq else freq, decreasing = TRUE)
}
