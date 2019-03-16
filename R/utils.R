# stop_ ------------------------------------------------------------------------
stop_ <- function(...)
{
  stop(..., call. = FALSE)
}

# use_function_instead ---------------------------------------------------------
use_function_instead <- function(function_new, function_old)
{
  if (! is.character(function_new)) {
    function_new <- deparse(substitute(function_new))
  }

  if (! is.character(function_old)) {
    function_old <- deparse(substitute(function_old))
  }

  warning(call. = FALSE, sprintf(
    "Please use %s() instead of %s()", function_new, function_old
  ))
}
