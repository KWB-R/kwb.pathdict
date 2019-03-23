# to_dictionary_key ------------------------------------------------------------
to_dictionary_key <- function(i, prefix = "p", leading.zeros = FALSE)
{
  fmt <- if (leading.zeros) {

    digits <- nchar(to_dictionary_key(length(i), ""))

    paste0("%s%0", digits, "X")

  } else {

    "%s%X"
  }

  sprintf(fmt, prefix, i)
}
