# random_paths -----------------------------------------------------------------

#' Create Random File Paths Using English Words
#'
#' @param max_depth maximum path depth
#' @param min_chars least number of characters per folder or file name
#' @param max_elements maximum number of elements (files or subfolders) in a
#'   folder
#' @param depth_to_leaf_weight function that calculates a weight from the given
#'   path depth. The weight is used to increase the probability of a folder
#'   element to be a file and not a subdirectory. By default the weight is
#'   calculated by 1.2^depth, i.e. for a folder in depth 10 it is about six
#'   times (1.2^10 = 6.19) more probable of its elements to be files instead of
#'   subfolders
#' @export
#' @examples
#' # Make this example reproducible
#' set.seed(12059)
#'
#' # Create random paths
#' paths <- kwb.pathdict::random_paths(max_depth = 5)
#'
#' # Show the random paths
#' paths
#'
#' # Frequency of path depths
#' table(lengths(kwb.file::split_paths(paths)))
#'
random_paths <- function(
  max_depth = 5, min_chars = 5, max_elements = 10,
  depth_to_leaf_weight = function(depth) 1.2^depth
)
{
  random_paths_(max_depth, min_chars, max_elements, depth_to_leaf_weight)
}

# random_paths_ ----------------------------------------------------------------

#' @keywords internal
random_paths_ <- function(
  max_depth = 5, min_chars = 5, max_elements = 10,
  depth_to_leaf_weight = function(depth) 1.2^depth, depth = 0, leaf = FALSE,
  debug_depth = 0
)
{
  if (leaf || depth == max_depth) {
    return(random_filenames(min_chars, size = 1))
  }

  parent <- sample(english_words(min_chars), 1)

  (n_elements <- sample(max_elements, 1))

  prob <- c(depth_to_leaf_weight(depth), 1)

  is_leaf <- sample(c(TRUE, FALSE), n_elements, replace = TRUE, prob = prob)

  paste0(parent, "/", unlist(lapply(seq_along(is_leaf), function(i) {

    kwb.utils::catIf(
      depth < debug_depth,
      paste(rep("  ", depth), collapse = ""),
      "Creating node ", i, "/", length(is_leaf), "\n"
    )

    random_paths_(
      max_depth = max_depth,
      min_chars = min_chars,
      max_elements = max_elements,
      depth_to_leaf_weight = depth_to_leaf_weight,
      depth = depth + 1,
      leaf = is_leaf[i]
    )

  })))
}

# random_filenames -------------------------------------------------------------

random_filenames <- function(min_chars = 4, max_elements = 10, size = NULL)
{
  words <- english_words(min_chars)

  extensions <- c("pdf", "doc", "xls", "R", "png", "jpg")

  size <- kwb.utils::defaultIfNULL(size, sample(max_elements, 1))

  paste0(sample(words, size), ".", sample(extensions, size, replace = TRUE))
}

# english_words ----------------------------------------------------------------

english_words <- function(min_chars = 0)
{
  words <- qdapDictionaries::Fry_1000
  words <- gsub("'", "_", words)
  words[nchar(words) >= min_chars]
}
