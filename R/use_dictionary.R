# use_dictionary ---------------------------------------------------------------

#' Substitute Values that are in a Dictionary with their Keys
#'
#' @param x vector of character
#' @param dict list of key = value pairs. Values of this list that are found in
#'   \code{x} are replaced by the placeholder "<key>" in \code{x}.
#' @param method method to be applied, must be one of "full" or "part".
#'   If "full", the full values must match, otherwise the values in \code{dict}
#'   are interpreted as patterns that are matched against the values in \code{x}
#'   and the matching parts are replaced with the corresponding "<key>"
#'   placeholder
#' @return \code{x} in which values or parts of the values are replaced with
#'   their short forms as they are defined in the dictionary \code{dict}
#' @export
#' @examples
#' # Define a vector of long values
#' x <- c("What a nice day", "Have a nice day", "Good morning")
#'
#' # Define short forms for full or partial values
#' dict_full <- list(wand = "What a nice day", gm = "Good morning")
#' dict_part <- list(w = "What", nd = "nice day", g = "Good")
#'
#' # Replace long form values with their short forms
#' kwb.pathdict:::use_dictionary(x, dict_full, method = "full")
#' kwb.pathdict:::use_dictionary(x, dict_part, method = "part")
use_dictionary <- function(x, dict, method = "full")
{
  if (method == "full") {

    indices <- match(x, as.character(dict))

    found <- ! is.na(indices)

    x[found] <- to_placeholder(names(dict)[indices[found]])

  } else if (method == "part") {

    keys <- names(dict)

    n <- length(keys)

    for (i in seq_len(n)) {

      key <- keys[i]

      pattern <- dict[[key]]

      replacement <- to_placeholder(key)

      if (i %% 20 == 0) {

        cat(sprintf(
          "%4.1f %% Substituting '%s' with '%s'...\n",
          100 * i/n, pattern, replacement
        ))
      }

      x <- gsub(pattern, replacement, x, fixed = TRUE)
    }

  } else {

    stop_("use_dictionary(): method must be one of 'full', 'part'")
  }

  x
}
