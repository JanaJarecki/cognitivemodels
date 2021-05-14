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
  
  # no solution
  x <- L_constraint(cbind(1,1), rhs = 1, dir = c("=="), names = letters[1:2])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), NULL)
})


test_that("3 Parameters, 2 sum-constrained", {
  # a, b sum-constrained, c unconstrained
  x <- L_constraint(rbind(c(1,0,0), c(1,1,0)), rhs = c(0.3,1), dir = eq(2), names = letters[1:3])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), c(a=0.3,b=0.7))
  # different order or rows
  x <- L_constraint(rbind(c(1,1,0), c(1,0,0)), rhs = c(1,0.3), dir = eq(2), names = letters[1:3])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), c(a=0.3,b=0.7))
  # all constrained
  x <- L_constraint(rbind(c(1,0,0), c(1,1,0), c(0,0,1)), rhs = c(0.3,1,5), dir = eq(3), names = letters[1:3])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), c(a=0.3,b=0.7,c=5))
  # only c constrained
  x <- L_constraint(rbind(c(1,1,0), c(0,0,1)), rhs = c(1,5), dir = eq(2), names = letters[1:3])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), c(c=5))
  x <- L_constraint(rbind(c(0,1,0), c(0,0,1), c(0,1,1)), rhs = c(1,1,2), dir = eq(3), names = letters[1:3])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), c(b=1,c=1))
})


test_that("4 Parameters, 2 sum-constrained", {
  # a, b sum-constrained AND fully constrained, c unconstrained
  x <- L_constraint(rbind(c(1,0,0,0), c(1,1,0,0), c(0,1,0,0)), rhs = c(0.3,1,0.7), dir = eq(3), names = letters[1:4])
  expect_equal(cognitivemodels:::.solve_constraints(x, x$rhs), c(a=0.3,b=0.7))
})