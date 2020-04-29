# Mock data for all tests
D <- data.frame(x1 = runif(10), x2 = runif(10), x3 = runif(10), x4 = runif(10), x5 = runif(10), x6 = runif(10), y = runif(10), z = runif(10))

test_that('Dimensions of the input', {
  expect_dim_equal <- function(formula, target) {
    M <- Cm$new(formula, D, make_parspace(), mode ="discrete", choicerule = "none")
    expect_equal(dim(M$input), c(10, target))
  }
  expect_dim_equal(y ~ x1, c(1, 1))
  expect_dim_equal(y ~ x1 + x2, c(2, 1))
  expect_dim_equal(y ~ x1 + x2 | x3, c(2,2))
  expect_dim_equal(y ~ x1 + x2 | x3 + x4, c(2,2))
  expect_dim_equal(y ~ x1 + x2 | x3 + x4 | x5 + x6, c(2,3))
  expect_dim_equal(y + z ~ x1 + x2 | x3 + x4, c(2,2))
})


test_that('Dimensions of the result', {
  expect_dim_equal <- function(formula, target) {
    M <- Cm$new(formula, D, make_parspace(), mode ="discrete", choicerule = "none")
    expect_equal(dim(M$res), c(10, target))
  }
  expect_dim_equal(y ~ x1, 1)
  expect_dim_equal(y ~ x1 + x2, 1)
  expect_dim_equal(y ~ x1 + x2 | x3, 1)
  expect_dim_equal(y + z ~ x1 + x2 | x3 + x4, 2)
})


test_that("M$nobs and nobs(M)", {
  expect_n_equal <- function(formula, target) {
    M <- Cm$new(formula, D, make_parspace(), mode ="discrete", choicerule = "none")
    expect_equal(M$nobs, nrow(D))
    expect_equal(nobs(M), M$nobs)
  }
  expect_n_equal(y ~ x1)
  expect_n_equal(y ~ x1 + x2)
  expect_n_equal(y ~ x1 + x2 | x3)
  expect_n_equal(y ~ x1 + x2 | x3 + x4)
  expect_n_equal(y ~ x1 + x2 | x3 + x4 | x5 + x6)
  expect_n_equal(y + z ~ x1 + x2 | x3 + x4)
})


test_that("M$nstim and nstim(M)", {
  expect_n_equal <- function(formula, target) {
    M <- Cm$new(formula, D, make_parspace(), mode ="discrete", choicerule = "none")
    expect_equal(M$nstim, target)
    expect_equal(nstim(M), M$nstim)
  }
  expect_n_equal(y ~ x1, 1)
  expect_n_equal(y ~ x1 + x2, 1)
  expect_n_equal(y ~ x1 + x2 | x3, 2)
  expect_n_equal(y ~ x1 + x2 | x3 + x4, 2)
  expect_n_equal(y ~ x1 + x2 | x3 + x4 | x5 + x6, 3)
  expect_n_equal(y + z ~ x1 + x2 | x3 + x4, 2)
})


test_that("Number of attributes", {
  expect_n_equal <- function(formula, target) {
    M <- Cm$new(formula, D, make_parspace(), mode ="discrete", choicerule = "none")
    expect_equal(M$natt, target)
    expect_equal(natt(M), M$natt)
  }
  expect_n_equal(y ~ x1, 1)
  expect_n_equal(y ~ x1 + x2, 2)
  expect_n_equal(y ~ x1 + x2 | x3, c(2,1))
  expect_n_equal(y ~ x1 + x2 | x3 + x4, c(2,2))
  expect_n_equal(y ~ x1 + x2 | x3 + x4 + x5, c(2,3))
  expect_n_equal(y ~ x1 + x2 | x3 + x4 | x5 + x6, c(2,2,2))
  expect_n_equal(y + z ~ x1 + x2 | x3 + x4, c(2,2))
})

test_that("Number of parameters", {
  expect_par_equal <- function(parspace, target) {
    M <- Cm$new(y~x1, D, parspace, mode="discrete", choicerule = "none", options = list(fit=FALSE))
    expect_equal(M$npar(), target)
    expect_equal(M$npar(), npar(M))
  }
  expect_par_equal(make_parspace(), 0L)
  expect_par_equal(make_parspace(alpha=0:1), 1L)
  expect_par_equal(make_parspace(a=0:1, b=0:1), 2L)
  expect_par_equal(make_parspace(a=0:1, b=0:1, c=0:1), 3L)
  expect_par_equal <- function(fix, target) {
    M <- Cm$new(y~x1, D, parspace = make_parspace(a=0:1, b=0:1, c=0:1, d=0:1), mode="discrete", choicerule = "none", fix = fix, options = list(fit=FALSE))
    expect_equal(M$npar(), 4L)
    expect_equal(M$npar("free"), 4L - target)
    expect_equal(M$npar("fix"), target)
  }
  expect_par_equal(NULL, 0)
  expect_par_equal(c(a=1), 1L)
  expect_par_equal(c(a="b"), 1L)
  expect_par_equal(c(a=1, b=0), 2L)
  expect_par_equal(list(a=1, b="a"), 2L)
  expect_par_equal(list(a=1, b=1, c=1), 3L)
  expect_par_equal(list(a=1, b=1, c="a"), 3L)
  expect_par_equal(list(a=1, b="a", c="a"), 3L)
  expect_par_equal(list(a=1, b=1, c=1, d=1), 4L)
})



test_that("Number of constraints", {
  expect_n_equal <- function(fix, target) {
    M <- Cm$new(y~x1, D, parspace = make_parspace(a=0:1, b=0:1, c=0:1, d=0:1), mode="discrete", choicerule = "none", fix = fix, options = list(fit=FALSE))
    expect_equal(M$ncon, target)
  }
  expect_n_equal(NULL, 0)
  expect_n_equal(c(a=1), 1L)
  expect_n_equal(c(a="b"), 1L)
  expect_n_equal(c(a=1, b=0), 2L)
  expect_n_equal(list(a=1, b="a"), 2L)
  expect_n_equal(list(a=1, b=1, c=1), 3L)
  expect_n_equal(list(a=1, b=1, c="a"), 3L)
  expect_n_equal(list(a=1, b="a", c="a"), 3L)
  expect_n_equal(list(a=1, b=1, c=1, d=1), 4L)
})



test_that("Names of constrained parameters", {
  expect_parnames_equal <- function(fix, target) {
    M <- Cm$new(y~x1, D, parspace=make_parspace(a=c(-1,1,0.5,0.01), b=c(-1,1,0.5,0.02), c=c(-2,2,0,0.03), d=c(-2,0,-1,-0.04)), mode="discrete", choicerule = "none", fix=fix, options = list(fit = FALSE))
    expect_equal(names(M$get_par("free")), target)
  }
  expect_parnames_equal(list(a=0), c("b", "c", "d"))
  expect_parnames_equal(list(b=0), c("a", "c", "d"))
  expect_parnames_equal(list(a=0,b=0), c("c", "d"))
  expect_parnames_equal(list(c=0,b=0), c("a", "d"))
  expect_parnames_equal(list(a=0,b=0,c=0,d=0), NULL)
  # @todo check get_par("free") for a = b
  # @body: If we have equality-constraints, with the RHS of the constraint being a parameter, that is free, should the contrained parameter on the LSH be counted as free or as fixed? Technically it is fixed, but it would need to be included in the free parameters in the model to be fitted (solnp)...
  expect_parnames_equal(fix = list(a="b"), c("a", "b", "c", "d"))
  expect_parnames_equal(fix = list(a="b", b="c"), c("a", "b", "c", "d"))
  expect_parnames_equal(fix = list(a="b", b="c", c="d"), c("a", "b", "c", "d"))
  expect_parnames_equal(fix = list(a="b", b="c", c="d", d=0), NULL)
  expect_parnames_equal(fix = list(c="b"), c("a", "b", "c", "d"))

  expect_parnames_equal(fix = list(a=NA), c("b", "c", "d"))
  expect_parnames_equal(fix = list(a=NA, b=NA), c("c", "d"))
  expect_parnames_equal(fix = list(a=NA, b=NA, c=NA), c("d"))
  expect_parnames_equal(fix = list(a="b", b="c", c=0, d=NA), NULL)
  expect_parnames_equal(fix = list(c="b"), c("a", "b", "c", "d"))

  expect_parnames_equal(fix = list(a=1, b="a"), c("c", "d"))
  expect_parnames_equal(fix = list(a=1, b=NA), c("c", "d"))
  expect_parnames_equal(fix = list(a=NA, b=NA), c("c", "d"))
  expect_parnames_equal(fix = list(a=NA, b="a"), c("c", "d"))
  expect_parnames_equal(fix = list(a=NA, b=0), c("c", "d"))
  expect_parnames_equal(fix = list(a=1, b="a", c = 0), c("d"))
  expect_parnames_equal(fix = list(a="b", c=0), c("a", "b", "d"))
})


test_that("Values of constrained parameters", {
  expect_par_equal <- function(fix, target) {
    M <- Cm$new(y~x1, D, parspace=make_parspace(a=c(0,1,0.5,0.01), b=c(-1,1,0.5,0.02), c=c(-2,2,0,0.03), d=c(-2,0.5,-1,-0.04)), mode="discrete", choicerule = "none", fix=fix, options = list(fit = FALSE))
    expect_equal(M$get_par(), target)
  }
  expect_par_equal(list(a="b"), c(a=0.5, b=0.5, c=0, d=-1))
  expect_par_equal(list(a="c"), c(a=0, b=0.5, c=0, d=-1))
  expect_par_equal(list(b="a"), c(a=0.5, b=0.5, c=0, d=-1))
  expect_par_equal(list(b="c"), c(a=0.5, b=0, c=0, d=-1))
  expect_par_equal(list(b="d"), c(a=0.5, b=-1, c=0, d=-1))
  expect_par_equal(list(c="a"), c(a=0.5, b=0.5, c=0.5, d=-1))
  expect_par_equal(list(c="b"), c(a=0.5, b=0.5, c=0.5, d=-1))
  expect_par_equal(list(c="d"), c(a=0.5, b=0.5, c=-1, d=-1))
  expect_par_equal(list(d="c"), c(a=0.5, b=0.5, c=0, d=0))
  # Two parameter equal each other
  expect_par_equal(list(a="b", c="d"), c(a=0.5, b=0.5, c=-1, d=-1))
  expect_par_equal(list(a="b", d="c"), c(a=0.5, b=0.5, c=0, d=0))
  expect_par_equal(list(a="c", b="d"), c(a=0, b=-1, c=0, d=-1))
  expect_par_equal(list(a="c", d="b"), c(a=0, b=0.5, c=0, d=0.5))
  # One parameter equal one fix
  expect_par_equal(list(a="b",b=1), c(a=1, b=1, c=0, d=-1))
  expect_par_equal(list(a="c",c=0.1), c(a=0.1, b=0.5, c=0.1, d=-1))
  expect_par_equal(list(b="a",a=.9), c(a=.9, b=.9, c=0, d=-1))
  expect_par_equal(list(b="c",c=.2), c(a=0.5, b=.2, c=.2, d=-1))
  expect_par_equal(list(b="d",d=0), c(a=0.5, b=0, c=0, d=0))
  expect_par_equal(list(c="a",a=.4), c(a=0.4, b=0.5, c=0.4, d=-1))
  expect_par_equal(list(c="b",b=.7), c(a=0.5, b=0.7, c=0.7, d=-1))
  expect_par_equal(list(c="d",d=0), c(a=0.5, b=0.5, c=0, d=0))
  expect_par_equal(list(d="c",c=0), c(a=0.5, b=0.5, c=0, d=0))
  # Two parameter equal each other
  expect_par_equal(list(a="b", c="d", b=0.99), c(a=0.99, b=0.99, c=-1, d=-1))
  expect_par_equal(list(a="b", c="d", b=0.99, d=0), c(a=0.99, b=0.99, c=0, d=0))
  expect_par_equal(list(a="b", d="c", b=0.99), c(a=0.99, b=0.99, c=0, d=0))
  expect_par_equal(list(a="b", d="c", b=0.99, c=0.1), c(a=0.99, b=0.99, c=0.1, d=0.1))
  expect_par_equal(list(a="c", b="d", c=0.1), c(a=0.1, b=-1, c=0.1, d=-1))
  expect_par_equal(list(a="c", b="d", c=0.1, d=0.1), c(a=0.1, b=0.1, c=0.1, d=0.1))
  })

test_that('Error messages', {
  expect_fix_error <- function(fix) {
    expect_error(Cm$new(y~x1, D, make_parspace(a=c(0,1)), fix=fix, mode="discrete", choicerule = "none"))
  }
  expect_fix_error(list(b=1))
  expect_fix_error(list(b="c"))
  expect_fix_error(list(b=1,b=2))
  expect_fix_error(list(a=-1))
  expect_fix_error(list(a=c(0,1)))
})

test_that("Grid nsteps with parameters between 0 - 1", {
  PS <- make_parspace(a=c(0,1), b=c(0,1), c=c(0,1), d=c(0,1), e = c(0,1))
  O <- list(fit_solver = c("grid", "solnp"), fit = FALSE)
  M <- Cm$new(y ~ x1, D, PS[1,],mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$fit_control$nsteps, 3)

  M <- Cm$new(y ~ x1, D, PS[1:2,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$fit_control$nsteps, rep(3,2))

  M <- Cm$new(y ~ x1, D, PS[1:3,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$fit_control$nsteps, rep(3,3))

  M <- Cm$new(y ~ x1, D, PS[1:4,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$fit_control$nsteps, rep(4,4))

  M <- Cm$new(y ~ x1, D, PS[1:5,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$fit_control$nsteps, rep(5,5))
})

test_that("Grid nsteps with parameters between 0 - 20", {
  PS <- make_parspace(a=c(0,1), b=c(0,10), c=c(0,1), d=c(0,20), e = c(0,1))
  O <- list(fit_solver = c("grid", "solnp"), fit = FALSE)

  M <- Cm$new(y ~ x1, D, PS[1:2,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$fit_control$nsteps, c(3,5))

  M <- Cm$new(y ~ x1, D, PS[1:3,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$fit_control$nsteps, c(3,5,3))

  M <- Cm$new(y ~ x1, D, PS[1:4,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$fit_control$nsteps, c(4,6,4,8))

  M <- Cm$new(y ~ x1, D, PS[1:5,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$fit_control$nsteps, c(5,7,5,10,5))
})

test_that("Sum constraints equal identities", {
  PS <- make_parspace(a=c(0,1), b=c(0,1), c=c(0,1), d=c(0,1))

  M <- Cm$new(y ~ x1, D, PS, mode="discrete", choicerule = "none", options = list(fit = FALSE))
  expect_equivalent(M$constraints, NULL)
  expect_equivalent(M$ncon, 0L)
  expect_equivalent(M$ncon, M$npar("fix"))

  M <- Cm$new(y ~ x1, D, PS, fix=list(a=0.5), mode="discrete", choicerule = "none", options = list(fit = FALSE))
  expect_equivalent(M$constraints, L_constraint(L = rbind(c(1,0,0,0)), rhs = 0.5, dir = "==", 
                                 names = c('a', 'b', 'c', 'd')))
  expect_equivalent(M$ncon, 1L)
  expect_equivalent(M$ncon, M$npar("fix"))

  M <- Cm$new(y ~ x1, D, PS, fix=list(a=0.5, b=0.7), mode="discrete", choicerule = "none", options = list(fit = FALSE))
  expect_equivalent(M$constraints, L_constraint(L = rbind(c(1,0,0,0), c(0,1,0,0)), rhs = c(0.5, 0.7), dir = c("==", "=="), names = names(M$get_par())))
  expect_equivalent(M$ncon, 2L)
  expect_equivalent(M$ncon, M$npar("fix"))

  M <- Cm$new(y ~ x1, D, PS, fix =list(a="d", b=0.5), mode="discrete", choicerule = "none", options = list(fit = FALSE))
  expect_equivalent(M$constraints, rbind(L_constraint(rbind(c(0,1,0,0)), "==", 0.5, names(M$get_par())), L_constraint(rbind(c(1,0,0,-1)), "==", 0, names = names(M$get_par())), use.names = TRUE))
  expect_equivalent(M$ncon, 2L)
  expect_equivalent(M$ncon, M$npar("fix"))

  M <- Cm$new(y ~ x1, D, PS, fix=list(a=1,b=0.99,c="d"), mode="discrete", choicerule = "none", options = list(fit = FALSE))
  expect_equivalent(M$constraints, rbind(
    L_constraint(rbind(c(1,0,0,0), c(0,1,0,0)), c("==","=="), c(1,0.99), names(M$get_par())),
    L_constraint(rbind(c(0,0,1,-1)), "==", 0, names(M$get_par())), use.names = TRUE))
  expect_equivalent(M$ncon, 3L)
  expect_equivalent(M$ncon, M$npar("fix"))
})

test_that("Zero RHS of formula", {
  expect_equal(Cm$new( ~ x1, D, mode="discrete", choicerule = "none")$res, NULL)
  expect_equivalent(Cm$new(y ~ x1, D, mode="discrete", choicerule = "none")$res, D[, "y", drop=FALSE])
})