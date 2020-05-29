test_that("varG", {
  expect_equal(varG(p = c(.9,.1), x = c(0,10)),  9)
  expect_equal(varG(p = c(.5,.5), x = c(0,12)), 36)
  expect_equal(varG(p = c(.9999,.0001), x = c(-1,9999)),  9999)
  expect_equal(varG(p = c(.9999,.0001), x = c(1,-9999)),  9999)
  expect_equal(varG(p = rbind(c(.9999,.0001), c(.5,.5)), x = rbind(c(1,-9999), c(0,12))), c(9999, 36))

  expect_error(varG(p = c(.9,.1), x = c(0)))
  expect_error(varG(p = c(.9), x = c(0,1)))
  expect_error(varG(p = as.matrix(c(.9,.1)), x = c(0,1)))
})