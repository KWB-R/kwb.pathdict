# to_cumulative_id -------------------------------------------------------------

#' Give Each Field in a Subdirectory Matrix an ID
#'
#' In the \code{subdir} matrix, each row represents a file path. The different
#' parts of the paths (the folder names) appear in the different columns. For
#' example, the paths "a/b/c" and "d/e" are represented by a matrix with values
#' "a", "b", "c" in the first and "d", "e", "" in the second row. Each cell of
#' the \code{subdir} matrix that is not empty gets a number. If two cells of one
#' column have the same number, this means that the paths to the cells are the
#' same. See example.
#'
#' @param subdirs matrix of subdirectory names, as returned by \code{
#'   \link[kwb.file]{to_subdir_matrix}}
#'
#' @examples
#' # Create a very simple subdirectory matrix
#' (subdirs <- matrix(byrow = TRUE, ncol = 4, c(
#'   "a", "b", "c", "d",
#'   "a", "b", "d", "",
#'   "a", "c", "d", "e"
#' )))
#'
#' # Give each non-empty cell of the matrix an ID
#' kwb.pathdict:::to_cumulative_id(subdirs)
#'
#' # You can read the matrix column by column. The highest number represents the
#' # number of different paths that reach up to the corresponding path level.
#' # 1st column: The starting parts of the paths in depth 1 are the same: "a".
#' #  All cells have ID = 1.
#' # 2nd column: There are two different paths to the folders in depth 2:
#' #   "a/b" (ID = 1) and "a/c" (ID = 2).
#' # 3rd column: There are three different paths to the folders in depth 3:
#' #   "a/b/c" (ID = 1), "a/b/d" (ID = 2), "a/c/d" (ID = 3).
#' # 4th column: There are only two out of three paths that reach depth 4:
#' #   "a/b/c/d" (ID = 1), "a/c/d/e" (ID = 2)
#'
to_cumulative_id <- function(subdirs)
{
  stopifnot(length(dim(subdirs)) == 2)

  cumpaths <- matrix(nrow = nrow(subdirs), ncol = ncol(subdirs))

  cumids <- cumpaths

  cat("depth: 00")

  for (depth in seq_len(ncol(subdirs))) {

    cat(sprintf("\b\b%2d", depth))

    reached <- ! kwb.utils::isNaOrEmpty(subdirs[, depth])

    last_reached_subdir <- subdirs[reached, depth]

    cumpaths[reached, depth] <- if (depth > 1) {

      paste0(cumpaths[reached, depth - 1], last_reached_subdir)

    } else {

      last_reached_subdir
    }

    cumids[, depth] <- as.integer(as.factor(cumpaths[, depth]))
  }

  cat("\n")

  cumids
}
