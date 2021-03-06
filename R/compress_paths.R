# compress_paths ---------------------------------------------------------------
#' @importFrom kwb.utils catIf getAttribute
compress_paths <- function(
  x, depth = 1, maxdepth = 1, dicts = list(), dbg = FALSE
)
{
  # kwb.utils::assignArgumentDefaults(compress_paths)

  kwb.utils::catIf(dbg, sprintf(
    "\n### In compress_paths(depth = %d)...\n", depth
  ))

  dirs <- dirname(x)

  ok <- (dirs != "." & dirs != "/")

  if (! any(ok)) {
    return(structure(x, dict = list()))
  }

  y <- compress(
    x = dirs[ok],
    dict = if (depth > 1) dicts[[depth - 1]] else NULL,
    prefix = letters[depth]
  )

  log_result_if(dbg, dirs[ok], y)

  dict_new <- kwb.utils::getAttribute(y, "dict")

  dicts[[depth]] <- dict_new

  if (depth < maxdepth) {

    y2 <- compress_paths(
      x = as.character(dict_new), depth = depth + 1, maxdepth = maxdepth,
      dicts = dicts
    )

    dict_new[seq_along(dict_new)] <- as.character(y2)

    dict_new <- c(dict_new, kwb.utils::getAttribute(y2, "dict"))
  }

  x[ok] <- file.path(as.character(y), basename(x[ok]))

  structure(x, dict = dict_new)
}

# compress ---------------------------------------------------------------------
compress <- function(x, dict = NULL, prefix = "a", extend.dict = FALSE)
{
  # If there is a dictionary replace element that are in there
  if (length(dict)) {

    x <- use_dictionary(x, dict)
  }

  # Create a new dictionary if there are any duplicates in x but do not
  # consider elements that are already placeholders
  dict_new <- to_dictionary(x = x[! is_placeholder(x)], prefix)

  x <- use_dictionary(x, dict_new)

  stopifnot(length(intersect(names(dict_new), names(dict))) == 0)

  structure(x, dict = if (extend.dict) c(dict, dict_new) else dict_new)
}

# to_dictionary ----------------------------------------------------------------

#' Create Dictionary from Unique Strings
#'
#' @param x vector of strings
#' @param prefix prefix to be given to the keys in the dictionary.
#'   Default: "a"
#' @param leading_zeros whether to make all keys in the dictionary have same
#'   length by adding leading zeros to the keys. Default: \code{FALSE}
#' @value list with unique values of \code{x} as values and element names
#'   "<prefix>_<i>" with <i> being a number from 1 to the number of unique
#'   elements in \code{x}
#' @importFrom stats setNames
#' @examples
#' # Define input strings
#' x <- c("elephant", "mouse", "cat", "cat", "cat", "mouse", "cat", "cat")
#'
#' # Create a dictionary for the unique values in x
#' kwb.pathdict:::to_dictionary(x)
#'
#' # Note that "cat" is the first entry because it has the highest "importance"
#' kwb.pathdict:::sorted_importance(x)
#'
to_dictionary <- function(x, prefix = "a", leading_zeros = FALSE)
{
  if (length(x) == 0) {
    return(list())
  }

  dict <- as.list(names(sorted_importance(x)))

  keys <- to_dictionary_key(seq_along(dict), prefix, leading_zeros)

  stats::setNames(dict, keys)
}

# log_result_if ----------------------------------------------------------------
#' @importFrom kwb.utils catLines getAttribute
log_result_if <- function(dbg, x, y)
{
  if (dbg) {

    kwb.utils::catLines(c("\n### x:", x))

    kwb.utils::catLines(c("\n### y:", y))

    cat("\n### str(dict):\n")

    utils::str(kwb.utils::getAttribute(y, "dict"))
  }
}
