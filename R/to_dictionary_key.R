# to_dictionary_key ------------------------------------------------------------
to_dictionary_key <- function(
  i, prefix = "p", leading_zeros = FALSE, max_i = NULL
)
{
  stopifnot(is.numeric(i))

  i <- as.integer(i)

  stopifnot(all(i > 0))

  fmt <- if (leading_zeros) {

    max_i <- kwb.utils::defaultIfNULL(max_i, max(i))

    raw_key <- to_dictionary_key(max_i, prefix = "", leading_zeros = FALSE)

    paste0("%0", nchar(raw_key), "X")

  } else {

    "%X"
  }

  paste0(prefix, sprintf(fmt, i))
}
