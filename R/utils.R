# order_decreasingly_by --------------------------------------------------------

#' Order Data Frame Decreasingly by one Column
#'
#' @param df data frame
#' @param column name of column by which to order decreasingly.
#' @importFrom kwb.utils selectColumns
#' @examples
#' (df <- data.frame(a = 1:3, b = 11:13))
#' kwb.pathdict:::order_decreasingly_by(df, "a")
order_decreasingly_by <- function(df, column)
{
  # Order decreasingly by this "effective" score
  kwb.utils::resetRowNames(
    df[order(kwb.utils::selectColumns(df, column), decreasing = TRUE), ]
  )
}

# stop_ ------------------------------------------------------------------------
stop_ <- function(...)
{
  stop(..., call. = FALSE)
}
