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

  result <- x

  if (any(ok)) {

    dict_old <- if (depth > 1) dicts[[depth - 1]] else NULL

    y <- compress(x = dirs[ok], dict = dict_old, prefix = letters[depth])

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

    result[ok] <- file.path(as.character(y), basename(x[ok]))

  } else {

    dict_new = list()
  }

  structure(result, dict = dict_new)
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
  dict_new <- to_dictionary(x[! is_placeholder(x)], prefix)

  x <- use_dictionary(x, dict_new)

  stopifnot(length(intersect(names(dict_new), names(dict))) == 0)

  structure(x, dict = if (extend.dict) c(dict, dict_new) else dict_new)
}

# to_dictionary ----------------------------------------------------------------
to_dictionary <- function(x, prefix = "a", leading.zeros = FALSE)
{
  dict <- as.list(names(sorted_importance(x)))

  keys <- to_dictionary_key(seq_along(dict), prefix, leading.zeros)

  stats::setNames(dict, keys)
}

# is_placeholder ---------------------------------------------------------------
is_placeholder <- function(x)
{
  grepl("^<[^<>]+>$", x)
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

# compress_one_by_one ----------------------------------------------------------
compress_one_by_one <- function(x, keys = LETTERS[1:n], n = 10)
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

# compress_with_dictionary -----------------------------------------------------
compress_with_dictionary <- function(subdirs, dict, fill.value = "")
{
  strings <- names(dict)[match(subdirs, as.character(dict))]

  if (! is.na(fill.value)) {

    strings[is.na(strings)] <- fill.value
  }

  matrix(strings, nrow = nrow(subdirs))
}
