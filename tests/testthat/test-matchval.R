test_that("matchval", {

  letters
  integers <- seq_along(letters)

  expect_identical(
    matchval(4:5, letters, integers, nomatch = "foo"),
    c("foo","foo")
  )

  expect_identical(
    matchval(4:5, integers, letters, nomatch = "foo"),
    c("d","e")
  )
})
