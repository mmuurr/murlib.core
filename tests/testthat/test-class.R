test_that("NUL", {

  x <- prepend_class(NULL, c("foo", "bar", "foo"))
  expect_identical(class(x), c("foo", "bar"))
  expect_true(is.list(x))

  x <- append_class(x, c("baz", NULL, "qux"))
  expect_true(is.list(x))
  expect_identical(class(x), c("foo", "bar", "baz", "qux"))
  
  x <- prepend_class(x, "first")
  expect_true(is.list(x))
  expect_identical(class(x), c("first", "foo", "bar", "baz", "qux"))

  ## this should 'bump' "foo" to the front
  x <- prepend_class(x, "foo")
  expect_true(is.list(x))
  expect_identical(class(x), c("foo", "first", "bar", "baz", "qux"))
})


