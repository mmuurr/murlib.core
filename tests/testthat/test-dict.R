test_that("dict", {
  expect_identical(
    names(dict()),
    character(0)
  )
  dict() |> expect_no_error()
  dict(list()) |> expect_error()
  dict(foo = list()) |> expect_no_error()
  dict(NULL) |> expect_error()
  dict(foo = NULL) |> expect_no_error()
  dict("foo" = "bar", "baz" = "qux") |> expect_no_error()
  dict("foo" = "bar", "foo" = "baz") |> expect_error()
  dict(1:3) |> expect_error()
  dict(foo = 1:3) |> expect_no_error()
})

test_that("is_dict", {
	is_dict(NULL) |> expect_false()
	is_dict(list()) |> expect_false()
	is_dict(character(0)) |> expect_false()
	is_dict(c("foo" = "bar", "baz" = "qux")) |> expect_false()
	is_dict(c("foo" = "bar", "baz" = "qux") |> as.list()) |> expect_true()
	is_dict() |> expect_error()
})
