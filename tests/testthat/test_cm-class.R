# Mock data for all tests
make_data <- function() {
  set.seed(2234)
  return(data.frame(x1 = runif(10), x2 = runif(10), x3 = runif(10), x4 = runif(10), x5 = runif(10), x6 = runif(10), y = runif(10), z = runif(10)))
}


test_that('Dimensions of the input', {
  expect_dim_equal <- function(formula, target) {
    M <- Cm$new(formula, make_data(), make_parspace(), mode ="discrete", choicerule = "none")
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
    M <- Cm$new(formula=y ~ x1, make_data(), make_parspace(), mode ="discrete", choicerule = "none")
    expect_equal(dim(M$res), c(10, target))
  }
  expect_dim_equal(y ~ x1, 1)
  expect_dim_equal(y ~ x1 + x2, 1)
  expect_dim_equal(y ~ x1 + x2 | x3, 1)
  expect_dim_equal(formula = y + z ~ x1 + x2 | x3 + x4, 1)
})

40990771


test_that("Number of observations: nobs methods", {
  expect_n_equal <- function(formula, target) {
    M <- Cm$new(formula, make_data(), make_parspace(), mode ="discrete", choicerule = "none")
    expect_equal(M$nobs, nrow(make_data()))
    expect_equal(nobs(M), M$nobs)
  }
  expect_n_equal(y ~ x1)
  expect_n_equal(y ~ x1 + x2)
  expect_n_equal(y ~ x1 + x2 | x3)
  expect_n_equal(y ~ x1 + x2 | x3 + x4)
  expect_n_equal(y ~ x1 + x2 | x3 + x4 | x5 + x6)
  expect_n_equal(y + z ~ x1 + x2 | x3 + x4)
})


test_that("Number of stimuli: nstim methods", {
  expect_n_equal <- function(formula, target) {
    M <- Cm$new(formula, make_data(), make_parspace(), mode ="discrete", choicerule = "none")
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


test_that("Number of attributes: natt methods", {
  expect_n_equal <- function(formula, target) {
    M <- Cm$new(formula, make_data(), make_parspace(), mode ="discrete", choicerule = "none")
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

test_that("Number of parameters: npar methods", {
  expect_par_equal <- function(parspace, target) {
    M <- Cm$new(y~x1, make_data(), parspace, mode="discrete", choicerule = "none", options = list(fit=FALSE))
    expect_equal(M$npar(), target)
    expect_equal(M$npar(), npar(M))
  }
  expect_par_equal(make_parspace(), 0L)
  expect_par_equal(make_parspace(alpha=0:1), 1L)
  expect_par_equal(make_parspace(a=0:1, b=0:1), 2L)
  expect_par_equal(make_parspace(a=0:1, b=0:1, c=0:1), 3L)

  expect_par_equal <- function(fix, n_fixed) {
    M <- Cm$new(y~x1, make_data(), parspace = make_parspace(a=0:1, b=0:1, c=0:1, d=0:1), mode="discrete", choicerule = "none", fix = fix, options = list(fit=FALSE))
    expect_equal(M$npar("all"), 4L)
    expect_equal(M$npar(), 4L - n_fixed)
  }
  expect_par_equal(NULL, 0)
  expect_par_equal(fix = c(a=1), 1L)
  expect_par_equal(c(a="b"), 1L)
  expect_par_equal(c(a=1, b=0), 2L)
  expect_par_equal(list(a=1, b="a"), 2L)
  expect_par_equal(list(a=1, b=1, c=1), 3L)
  expect_par_equal(list(a=1, b=1, c="a"), 3L)
  expect_par_equal(list(a=1, b="a", c="a"), 3L)
  expect_par_equal(fix = list(a=1, b=1, c=1, d=1), 4L)
})


test_that("Number of constraints: ncons methods", {
  expect_n_equal <- function(fix, target) {
    M <- Cm$new(y~x1, make_data(), parspace = make_parspace(a=0:1, b=0:1, c=0:1, d=0:1), mode="discrete", choicerule = "none", fix = fix, options = list(fit=FALSE))
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
    M <- Cm$new(y~x1, make_data(), parspace=make_parspace(a=c(-1,1,0.5,0.01), b=c(-1,1,0.5,0.02), c=c(-2,2,0,0.03), d=c(-2,0,-1,-0.04)), mode="discrete", choicerule = "none", fix=fix, options = list(fit = FALSE))
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
    M <- Cm$new(y~x1, make_data(), parspace=make_parspace(a=c(0,1,0.5,0.01), b=c(-1,1,0.5,0.02), c=c(-2,2,0,0.03), d=c(-2,0.5,-1,-0.04)), mode="discrete", choicerule = "none", fix=fix, options = list(fit = FALSE))
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
    expect_error(Cm$new(y~x1, make_data(), make_parspace(a=c(0,1)), fix=fix, mode="discrete", choicerule = "none"))
  }
  expect_fix_error(list(b=1))
  expect_fix_error(list(b="c"))
  expect_fix_error(list(b=1,b=2))
  expect_fix_error(list(a=-1))
  expect_fix_error(list(a=c(0,1)))
})

test_that("Grid nsteps with parameters between 0 - 1", {
  PS <- make_parspace(a=c(0,1), b=c(0,1), c=c(0,1), d=c(0,1), e = c(0,1))
  O <- list(solver = c("grid", "solnp"), fit = FALSE)
  M <- Cm$new(y ~ x1, make_data(), PS[1, ,drop = FALSE], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$solver_args$nsteps, 3)

  M <- Cm$new(y ~ x1, make_data(), PS[1:2,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$solver_args$nsteps, rep(3,2))

  M <- Cm$new(y ~ x1, make_data(), PS[1:3,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$solver_args$nsteps, rep(3,3))

  M <- Cm$new(y ~ x1, make_data(), PS[1:4,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$solver_args$nsteps, rep(4,4))

  M <- Cm$new(y ~ x1, make_data(), PS[1:5,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$solver_args$nsteps, rep(5,5))
})

test_that("Grid nsteps with parameters between 0 - 20", {
  PS <- make_parspace(a=c(0,1), b=c(0,10), c=c(0,1), d=c(0,20), e = c(0,1))
  O <- list(solver = c("grid", "solnp"), fit = FALSE)

  M <- Cm$new(y ~ x1, make_data(), PS[1:2,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$solver_args$nsteps, c(3,5))

  M <- Cm$new(y ~ x1, make_data(), PS[1:3,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$solver_args$nsteps, c(3,5,3))

  M <- Cm$new(y ~ x1, make_data(), PS[1:4,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$solver_args$nsteps, c(4,6,4,8))

  M <- Cm$new(y ~ x1, make_data(), PS[1:5,], mode="discrete", choicerule = "none", options = O)
  expect_equivalent(M$options$solver_args$nsteps, c(5,7,5,10,5))
})

test_that("Sum constraints and equality constraints on parameters", {
  expect_cons_equivalent <- function(fix, target, rhs = unlist(fix)) {
    PS <- make_parspace(a=c(0,1), b=c(0,1), c=c(0,1), d=c(0,1))
    M <- Cm$new(y ~ x1, make_data(), PS, fix = fix, mode="discrete", choicerule = "none", options = list(fit = FALSE))
    if (is.null(target)) { C <- NULL } else {
      C <- L_constraint(L = target, rhs = rhs, dir = rep("==", length(rhs)), names = c('a', 'b', 'c', 'd'))
    }
    M$constraints$L[["dimnames"]] <- C$L[["dimnames"]] <- NULL
    expect_equivalent(M$constraints, C)
  }

  expect_cons_equivalent(fix = NULL, NULL)
  expect_cons_equivalent(list(a = 0.5), rbind(c(1,0,0,0)))
  expect_cons_equivalent(list(a = 0.5, b = 1), rbind(c(1,0,0,0), c(0,1,0,0)))
  #expect_cons_equivalent(list(a = "d", b = 1), rbind(c(0,1,0,0), c(1,0,0,-1)), c(1,0))
  expect_cons_equivalent(list(a=1,b=0.99,c="d"), rbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,-1)), c(1,0.99,0))
})


test_that("Zero RHS of formula", {
  expect_equal(Cm$new( ~ x1, make_data(), mode="discrete", choicerule = "none")$res, NULL)
  expect_equivalent(Cm$new(y ~ x1, make_data(), mode="discrete", choicerule = "none")$res, make_data()[, "y", drop=FALSE])
})



test_that("Adding the choicerule", {
  # Fix choicerule parameter
  expect_crpar_equal <- function(choicerule, target) {
    M <- Cm$new( ~ x1, make_data(),
      mode="discrete", choicerule=choicerule, fix=target)
    expect_equal(M$npar("all"), 1)
    expect_equal(M$par, target)
  }
  expect_crpar_equal(choicerule = "softmax", list(tau = 1))
  expect_crpar_equal(choicerule = "softmax", list(tau = 10))
  expect_crpar_equal(choicerule = "epsilon", list(eps = 1))
  expect_crpar_equal(choicerule = "epsilon", list(eps = 0.5))  

  # Free choicerule parameter but dont fit
  M <- Cm$new(~ x1, make_data(), mode="discrete", choicerule="softmax", options = list(fit = FALSE))
  expect_equal(coef(M), c(tau = 0.5))
  M <- Cm$new(~ x1, make_data(), mode="discrete", choicerule="epsilon", options = list(fit = FALSE))
  expect_equal(coef(M), c(eps = 0.2))
})