test_that("identical infix", {
  
  expect_false(1 %===% 1L)
  expect_true(1L %===% 1L)
})
