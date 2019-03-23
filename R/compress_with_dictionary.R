# compress_with_dictionary -----------------------------------------------------
compress_with_dictionary <- function(subdirs, dict, fill.value = "")
{
  strings <- names(dict)[match(subdirs, as.character(dict))]

  if (! is.na(fill.value)) {

    strings[is.na(strings)] <- fill.value
  }

  matrix(strings, nrow = nrow(subdirs))
}
