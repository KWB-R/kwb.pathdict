# is_placeholder ---------------------------------------------------------------
is_placeholder <- function(x)
{
  grepl("^<[^<>]+>$", x)
}

# to_placeholder ---------------------------------------------------------------
to_placeholder <- function(x)
{
  paste0("<", x, ">")
}
