test_that("itself", {
  
  expect_identical(
    itself() |> expect_invisible(),
    NULL
  )

  expect_identical(itself(42L), 42L)
  expect_identical(itself(42L, foo, bar, baz), 42L)  ## ignored additional ...args
})
