# get_dictionary_one_by_one ----------------------------------------------------

#' Get a Path Dictionary
#'
#' @param paths vector of character strings representing file or folder paths
#' @param n number of compression levels
#' @importFrom kwb.utils moveColumnsToFront
get_dictionary_one_by_one <- function(paths, n = 10)
{
  main_columns_winner <- c("i", "key", "score", "count", "length")

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

    winner <- kwb.utils::moveColumnsToFront(winner, main_columns_winner)

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

# update_frequency_data_length -------------------------------------------------
#' @importFrom kwb.utils selectColumns
update_frequency_data_length <- function(frequency_data, winner, key)
{
  get_column <- kwb.utils::selectColumns

  winner_length <- get_column(winner, "length")
  winner_path <- get_column(winner, "path")
  data_length <- get_column(frequency_data, "length")
  data_path <- get_column(frequency_data, "path")

  shortage <- winner_length - nchar(to_placeholder(key))

  matching <- (substr(data_path, 1, winner_length) == winner_path)

  frequency_data$length[matching] <- data_length[matching] - shortage

  frequency_data
}
