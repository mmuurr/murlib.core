test_that("bad predicate functions", {

  expect_error(NULL |> default_ifnot(\() TRUE))
  expect_error(NULL |> default_ifnot(TRUE))
  expect_error(NULL |> default_ifnot(~TRUE))  
})


test_that("normal behavior", {

  expect_identical(1L |> default_ifnot(is.integer, "foo"), 1L)
  expect_identical(NULL |> default_ifnot(is.integer, "foo"), "foo")

  expect_identical(NULL |> default_ifnot(is.null, "foo"), NULL)
  expect_identical(1L |> default_ifnot(is.null, NULL), NULL)

  expect_error(NULL |> default_ifnot(is.integer, stop()))
})


test_that("alias", {
  expect_identical(default_ifnot, default_if_not)
})
