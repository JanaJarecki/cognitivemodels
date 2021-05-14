test_that('Predicted Values for one time block', {
  expect <- function(c, target) {
    D <- data.frame(a = rep(0L,4), b = 1L, y = c(0,0,1,1))
    M <- shift_d(~ a + b, D, fix = c(c=c))
    expect_equal(predict(M), target, tol = .001)
  }
  expect(c = 0,   c(1,1,1,1))
  expect(c = 1.5, c(0,1,1,1))
  expect(c = 2.5, c(0,0,1,1))
  expect(c = 3.5, c(0,0,0,1))
  expect(c = 4.5, c(0,0,0,0))
})

test_that('Predicted Values for two time blocks', {
  expect <- function(c, target) {
    D <- data.frame(a = rep(0L,8), b = 1L, time = c(1:4,1:4))
    M <- shift_d(~ a + b, D, ~time, fix = c(c=c))
    expect_equal(predict(M), target, tol = .001)
  }
  expect(c = 0,   c(1,1,1,1,1,1,1,1))
  expect(c = 1.5, c(0,1,1,1,0,1,1,1))
  expect(c = 2.5, c(0,0,1,1,0,0,1,1))
  expect(c = 3.5, c(0,0,0,1,0,0,0,1))
  expect(c = 4.5, c(0,0,0,0,0,0,0,0))
})


test_that('Fitted parameters', {
  expect <- function(y, c) {
    D <- data.frame(a = rep(0L,4), b = 1L, y = y)
    M <- shift_d(y ~ a + b, D)
    expect_equivalent(ceiling(coef(M)), c)
  }
  expect(y = c(1,1,1,1), c = 1)
  expect(c(0,1,1,1), c = 2)
  expect(c(0,0,1,1), c = 3)
  expect(c(0,0,0,1), c = 4)
  expect(c(0,0,0,0), c = 5)
})