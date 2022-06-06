# compress_one_by_one ----------------------------------------------------------
compress_one_by_one <- function(x, keys = LETTERS[seq_len(n)], n = 10)
{
  lapply(keys, function(key) {

    elapsed <- system.time(x <<- get_next_level(x, key))

    cat(sprintf("Elapsed: %0.1f s\n", elapsed["elapsed"]))

    x
  })
}

# get_next_level ---------------------------------------------------------------
get_next_level <- function(x, key, set.attributes = FALSE, dbg = FALSE)
{
  freqs <- get_subdir_frequencies(paths = x, dbg = dbg)

  allfreqs <- sort(unlist(freqs), decreasing = TRUE)

  dict <- structure(list(names(allfreqs[1])), names = key)

  result <- use_dictionary(x, dict, method = "part")

  if (set.attributes) {

    attr(result, "freqs") <- freqs

    attr(result, "dict") <- dict
  }

  result
}
