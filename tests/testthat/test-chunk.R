test_that("errors", {

  ## can't have both arg02 and arg03 as non-null:
  expect_error(chunk(1:10, 3, 3))

  ## can't have both arg02 and arg03 as null:
  expect_error(chunk(1:10))

  ## method must be one of seq, mod, rand:
  expect_error(chunk(1:10, 3, method = "foo"))
})


test_that("method = seq", {

  vals <- letters[1:10]

  ## default method = "seq"
  expect_identical(
    chunk(vals, chunk_count = 3),
    list(c("a","b","c","d"), c("e","f","g"), c("h","i","j"))
  )

  expect_identical(
    chunk(vals, chunk_count = 2, method = "seq"),
    list(c("a","b","c","d","e"), c("f","g","h","i","j"))
  )

  expect_identical(
    chunk(vals, chunk_max_length = 5, method = "seq"),
    list(c("a","b","c","d","e"), c("f","g","h","i","j"))
  )

  expect_identical(
    chunk(vals, chunk_count = 3, method = "seq"),
    list(c("a","b","c","d"), c("e","f","g"), c("h","i","j"))
  )

  expect_identical(
    chunk(vals, chunk_max_length = 4, method = "seq"),
    list(c("a","b","c","d"), c("e","f","g"), c("h","i","j"))
  )

  expect_identical(
    chunk_int(10, chunk_count = 3, method = "seq"),
    list(1:4,5:7,8:10) |> lapply(as.integer)
  )

  expect_identical(
    chunk_int(10, chunk_count = 2, method = "seq"),
    list(1:5,6:10) |> lapply(as.integer)
  )

  expect_identical(
    chunk_int(10, chunk_max_length = 4, method = "seq"),
    list(1:4,5:7,8:10) |> lapply(as.integer)
  )
})


test_that("method = mod", {

  vals <- letters[1:5]

  expect_identical(
    chunk(vals, chunk_count = 2, method = "mod"),
    list(c("a","c","e"), c("b","d"))
  )

  expect_identical(
    chunk_int(5, chunk_count = 2, method = "mod"),
    list(c(1,3,5), c(2,4)) |> lapply(as.integer)
  )

  expect_identical(
    chunk(vals, chunk_max_length = 3, method = "mod"),
    list(c("a","c","e"), c("b","d"))
  )

  expect_identical(
    chunk_int(5, chunk_max_length = 3, method = "mod"),
    list(c(1,3,5), c(2,4)) |> lapply(as.integer)
  )    
})



test_that("method = rand", {

  vals <- letters[1:5]

  x <- chunk(vals, chunk_count = 2, method = "rand")
  expect_length(x, 2)
  expect_identical(x |> unlist() |> sort(), vals)

  x <- chunk(vals, chunk_max_length = 2, method = "rand")
  expect_length(x, 3)
  expect_identical(x |> unlist() |> sort(), vals)

  x <- chunk_int(5, chunk_count = 2, method = "rand")
  expect_length(x, 2)
  expect_identical(x |> unlist() |> sort(), 1:5)
  
  x <- chunk_int(5, chunk_max_length = 2, method = "rand")
  expect_length(x, 3)
  expect_identical(x |> unlist() |> sort(), 1:5)
  
})
