test_that("1 Parameter, 1 constraint", {
  x <- L_constraint(rbind(1), rhs = 0, dir = "==", names = letters[1])
  expect_equal(cognitivemodels:::.solve_constraints(x, b = x$rhs), c(a=0))
  x <- L_constraint(rbind(1), rhs = 0.1, dir = "==", names = letters[1])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), c(a=0.1))
  x <- L_constraint(rbind(1), rhs = -0.1, dir = "==", names = letters[1])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), c(a=-0.1))
})

test_that("2 Parameter, 2 constraints", {
  x <- L_constraint(rbind(c(1,0), c(0,1)), rhs = c(0,0), dir = c("==", "=="), names = letters[1:2])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), c(a=0,b=0))
  x <- L_constraint(rbind(c(1,0), c(0,1)), rhs = c(0.1,0), dir = c("==", "=="), names = letters[1:2])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), c(a=0.1,b=0))
  x <- L_constraint(rbind(c(1,0), c(0,1)), rhs = c(0,0.1), dir = c("==", "=="), names = letters[1:2])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), c(a=0,b=0.1))

})