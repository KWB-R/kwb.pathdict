context("path functions")

x1 <- rep("hallo", 3)
d1 <- list(a1 = "hallo")
c1 <- rep("<a1>", 3)

x2 <- c(rep("hallo", 3), rep("hallihallo", 2))
d2 <- list(a1 = "hallihallo", a2 = "hallo")
c21 <- rep("<p1>", 3)
c22 <- c(rep("<a2>", 3), rep("<a1>", 2))

x3 <- c("monday", "tuesday", "sunday", "monday")
d3 <- list(a1 = "monday", a2 = "tuesday", a3 = "sunday")

test_that("placeholders are recognised correctly", {

  expect_identical(
    is_placeholder(c("a", "<a", "<a>", "<>", "<ab<cd>ef>")),
    c(F, F, T, F, F)
  )
})

test_that("simple dictionaries are created correctly", {

  expect_identical(to_dictionary(x1), d1)
  expect_identical(to_dictionary(x2), d2)
  expect_identical(to_dictionary(x3), d3)
})

test_that("compress works for simple path vectors", {

  expect_identical(as.character(compress(x1)), c1)

  dict <- list("p1" = "hallo")
  expect_identical(as.character(compress(x = x1, dict = dict)), c21)

  expect_identical(as.character(compress(x2)), c22)

  #kwb.pathdict:::compress(x3)
  #kwb.pathdict:::compress(x3, dict = list(tag = "monday"))
})

# Further tests (to be implemented...) -----------------------------------------
if (FALSE)
{
  dict <- list(p1 = "abc", p2 = "def")
  lookup_in_dictionary("abc", dict) == "<p1>"
  lookup_in_dictionary("def", dict) == "<p2>"
  lookup_in_dictionary("ghi", dict) == "<NA>"
  lookup_in_dictionary("abc/def", dict) == "<NA>"

  y <- compress(x = c("hallo", "hallo", "hallo")) # -> error
  y == c("<p1>", "<p1>", "<p1>")

  x <- c("a", "b")
  y <- compress(x)
  identical(as.character(y), x)
  length(getAttribute(y, "dict")) == 0

  x <- c("abc/d", "abc/d/e", "abc/d/e/f")
  y <- compress(x)
  x; as.character(y); getAttribute(y, "dict")

  old_dict <- list(
    p1 = "<p4>/project-archive",
    p2 = "<a1>/project-1",
    p3 = "<a1>/project-2",
    p4 = "<a2>/hidden",
    a1 = "<b1>/closed_contracts",
    a2 = "//my_server",
    b1 = "//my_server/hidden/contracts"
  )

  compress(x = unique(dirname(as.character(old_dict))), old_dict)

  x <- c("longPath/a", "longPath/b", "longPath/other/xyz/abc")
  x <- c("abc/longPath/a", "b/longPath/c")
  y <- compress_paths(x, maxdepth = 14, dbg = FALSE)

  log_result_if(TRUE, x, y)
}
