# data_frame_to_paths ----------------------------------------------------------
#' @importFrom kwb.utils pasteColumns
data_frame_to_paths <- function(df)
{
  # Paste columns and remove all trailing slashes
  gsub("/+$", "", kwb.utils::pasteColumns(df, sep = "/"))
}

# get_subdirs_by_frequence -----------------------------------------------------
get_subdirs_by_frequence <- function(subdirs, cumid, freqinfo, dbg = TRUE)
{
  kwb.utils::printIf(dbg, freqinfo)

  rows <- which(cumid[, freqinfo$depth] == freqinfo$n.x)[1]

  subdirs[rows, seq_len(freqinfo$depth)]
}

# lookup_in_dictionary ---------------------------------------------------------
lookup_in_dictionary <- function(x, dict)
{
  #x <- old_dirs; dict <- old_dict

  ready <- x %in% to_placeholder(names(dict))

  out <- x

  out[! ready] <- to_placeholder(names(dict[match(x[! ready], dict)]))

  out
}

# replace_subdirs --------------------------------------------------------------
replace_subdirs <- function(s, r, p)
{
  selected <- starts_with_parts(s, r)

  cols <- seq(length(r) + 1, ncol(s))

  fillright <- matrix(nrow = sum(selected), ncol = length(r) -1)

  s[selected, ] <- cbind(p, s[selected, cols, drop = FALSE], fillright)

  # Remove empty columns
  maxcol <- max(which(apply(s, 2, function(x) sum(! is.na(x))) > 0))

  s[, seq_len(maxcol)]
}

# starts_with_parts ------------------------------------------------------------

#' Do Subfolder List Elements Start with Given Folder Names?
#'
#' @param parts list of list of character as returned by
#'   \code{\link[kwb.file]{split_paths}} or a matrix of character representing
#'   the subfolder names at the different folder depths as returned by
#'   \code{\link[kwb.file]{to_subdir_matrix}}.
#' @param elements vector of character giving the sequence of strings to be
#'   found in \code{parts}
#' @return vector of logical as long as \code{parts} containing \code{TRUE} at
#'   positions \code{i} for which \code{all(parts[[i]][seq_along(elements)] ==
#'   elements)} is \code{TRUE}
#'
#' @export
#'
#' @examples
#' parts <- strsplit(c("a/b/c", "a/b/d", "b/c"), "/")
#' starts_with_parts(parts, elements = c("a", "b"))
#' starts_with_parts(parts, elements = c("b", "c"))
#'
#' subdir_matrix <- kwb.file::to_subdir_matrix(parts)
#' starts_with_parts(subdir_matrix, elements = c("a", "b"))
#' starts_with_parts(subdir_matrix, elements = c("b", "c"))
#'
starts_with_parts <- function(parts, elements)
{
  stopifnot(is.list(parts) || is.matrix(parts))

  stopifnot(all(! is.na(elements)))

  length_out <- if (is.list(parts)) length(parts) else nrow(parts)

  selected_at_level <- lapply(seq_along(elements), function(i) {

    if (is.list(parts)) {

      sapply(parts, "[", i) == elements[i]

    } else {

      ! is.na(parts[, i]) & (parts[, i] == elements[i])
    }
  })

  Reduce(`&`, selected_at_level, init = rep(TRUE, length_out))
}


# to_placeholder ---------------------------------------------------------------
to_placeholder <- function(x)
{
  paste0("<", x, ">")
}
