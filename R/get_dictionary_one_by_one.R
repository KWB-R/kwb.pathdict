# get_dictionary_one_by_one ----------------------------------------------------

#' Get a Path Dictionary
#'
#' @param paths vector of character strings representing file or folder paths
#' @param n number of compression levels
#' @importFrom kwb.utils moveColumnsToFront
get_dictionary_one_by_one <- function(paths, n = 10)
{
  # Get the frequencies of the directory paths
  frequencies <- get_subdir_frequencies(paths = paths, first.only = TRUE)

  frequency_data_raw <- to_frequency_data(frequencies)

  frequency_data <- frequency_data_raw

  dictionary <- list()

  while (length(dictionary) < n) {

    # Next key to be used in the dictionary
    key <- to_dictionary_key(length(dictionary) + 1)

    # How many characters will the placeholder occupy?
    placeholder_size = nchar(to_placeholder(key))

    # Rescore and reorder frequency_data
    frequency_data <- rescore_and_reorder_frequency_data(
      frequency_data, placeholder_size
    )

    print_path_frequencies(frequency_data)

    # Which is the "winning" path?
    winner <- frequency_data[1, ]

    # Remove the winning path
    frequency_data <- frequency_data[-1, ]

    # Put the winning path into the dictionary
    dictionary[[key]] <- winner$path

    winner <- cbind(i = length(dictionary), key = key, winner)

    winner <- kwb.utils::moveColumnsToFront(winner, main_columns_winner())

    print(winner, row.names = FALSE)

    # Update the length (reduce by "shortage", the number of saved characters)
    frequency_data <- update_frequency_data_length(frequency_data, winner, key)
  }

  dictionary
}

# get_subdir_frequencies -------------------------------------------------------
#' @importFrom kwb.file split_paths
#' @importFrom kwb.utils catIf collapsed printIf
#' @importFrom graphics hist
get_subdir_frequencies <- function(
  subdirs = kwb.file::split_paths(paths), paths = NULL, first.only = TRUE,
  dbg = TRUE
)
{
  n_levels <- lengths(subdirs)

  if (dbg) {

    main <- "Distribution of path depths"

    graphics::hist(n_levels, main = main)

    kwb.utils::printIf(dbg, table(n_levels), main)
  }

  lapply(seq_len(max(n_levels)), function(i) {

    is_long_enough <- n_levels >= i

    kwb.utils::catIf(dbg, sprintf("i = %d, n = %d...\n", i, sum(is_long_enough)))

    x <- sapply(subdirs[is_long_enough], function(xx) {

      kwb.utils::collapsed(xx[seq_len(i)], "/")
    })

    y <- sorted_importance(x)

    kwb.utils::printIf(dbg, utils::head(y))

    if (first.only) y[1] else y
  })
}

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

# to_frequency_data ------------------------------------------------------------
#' @importFrom kwb.utils noFactorDataFrame
to_frequency_data <- function(freqs)
{
  sorted_frequencies <- sort(unlist(freqs), decreasing = TRUE)

  path_length <- nchar(names(sorted_frequencies))

  kwb.utils::noFactorDataFrame(
    path = names(sorted_frequencies),
    score = sorted_frequencies,
    length = path_length,
    count = sorted_frequencies / path_length,
    row.names = NULL
  )
}

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

# rescore_and_reorder_frequency_data -------------------------------------------
#' Rescore and Reorder Frequency Data
#'
#' @param frequency_data data frame with columns \code{length} and \code{count}
#' @param placeholder_size size of placeholder in number of characters. The path
#'   length will be reduced by this value before being multiplied with the count
#'   to calculate the score.
#' @importFrom kwb.utils resetRowNames selectColumns
rescore_and_reorder_frequency_data <-function(frequency_data, placeholder_size)
{
  # Get the path lengths
  lengths <- kwb.utils::selectColumns(frequency_data, "length")

  # Get the path counts
  counts <- kwb.utils::selectColumns(frequency_data, "count")

  # Calculate the "effective" score
  frequency_data$score2 <- (lengths - placeholder_size) * counts

  # Order decreasingly by this "effective" score
  order_decreasingly_by(frequency_data, "score2")
}

# print_path_frequencies -------------------------------------------------------
print_path_frequencies <- function(x, maxchar = 80)
{
  x$path <- substr(x$path, 1, maxchar)

  print(x)
}

# main_columns_winner ----------------------------------------------------------
main_columns_winner <- function()
{
  c("i", "key", "score", "count", "length")
}

# update_frequency_data_length -------------------------------------------------
#' @importFrom kwb.utils selectColumns
update_frequency_data_length <- function(frequency_data, winner, key)
{
  winner_length <- kwb.utils::selectColumns(winner, "length")
  winner_path <- kwb.utils::selectColumns(winner, "path")
  data_length <- kwb.utils::selectColumns(frequency_data, "length")
  data_path <- kwb.utils::selectColumns(frequency_data, "path")

  shortage <- winner_length - nchar(to_placeholder(key))

  matching <- (substr(data_path, 1, winner_length) == winner_path)

  frequency_data$length[matching] <- data_length[matching] - shortage

  frequency_data
}
