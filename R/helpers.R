# print_dict -------------------------------------------------------------------

#' Helper function to print the dictionary to the console
#' @keywords internal
print_dict <- function(x, maxdepth = 1, compressed = NULL)
{
  if (is.null(compressed)) {
    compressed <- compress_paths(x, maxdepth = maxdepth)
  }

  dict <- kwb.utils::getAttribute(compressed, "dict")

  out <- utils::capture.output(
    kwb.utils::writeDictionary(dict, file = stdout())
  )

  writeLines(out[-(1:12)])
}

# dictionary_size --------------------------------------------------------------

#' Helper function to get the size of compressed paths including dictionary
#' @keywords internal
dictionary_size <- function(x, maxdepth = 1, compressed = NULL)
{
  if (is.null(compressed)) {
    compressed <- compress_paths(x, maxdepth = maxdepth)
  }

  utils::object.size(compressed)
}

# print_compressed -------------------------------------------------------------

#' Helper function to print compressed paths and dictionary
#' @keywords internal
print_compressed <- function(x)
{
  cat("Compressed paths:\n")
  writeLines(as.character(x))
  cat("\nDictionary:\n")
  print_dict(compressed = x)
  cat("\nSize: ", utils::capture.output(dictionary_size(compressed = x)))
}

# named_vector_to_data_frame ---------------------------------------------------
#' Helper function to convert a named vector to a data frame
#' @keywords internal
named_vector_to_data_frame <- function(x)
{
  kwb.utils::noFactorDataFrame(name = names(x), value = as.character(x))
}
