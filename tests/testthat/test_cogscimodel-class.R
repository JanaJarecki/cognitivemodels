context("Cogscimodel-class")

# Mock data for all tests
D <- data.frame(x1 = runif(10), x2 = runif(10), x3 = runif(10), x4 = runif(10), x5 = runif(10), x6 = runif(10), y = runif(10), z = runif(10))

M <- Cogscimodel$new(y ~ x1, D, make_parspace(), mode ="discrete")
test_that('Dimensions for y ~ x1', {
  expect_equal(dim(M$input), c(10, 1, 1))
  expect_equal(dim(M$res), c(10, 1))
  expect_equal(M$nobs, nobs(M))
  expect_equal(M$nobs, 10)
  expect_equal(M$nstim, nstim(M))
  expect_equal(M$nstim, 1)
  expect_equal(M$natt, natt(M))
  expect_equal(M$natt, 1)
  expect_equal(M$nres, 1)
  expect_equal(M$ncon, 0L)
})

M <- Cogscimodel$new(y ~ x1 + x2, D, make_parspace(), mode="discrete")
test_that('Dimensions for y ~ x1 + x2', {
  expect_equal(dim(M$input), c(10, 2, 1))
  expect_equal(dim(M$res), c(10, 1))
  expect_equal(M$nobs, nobs(M))
  expect_equal(M$nobs, 10)
  expect_equal(M$nstim, nstim(M))
  expect_equal(M$nstim, 1)
  expect_equal(M$natt, natt(M))
  expect_equal(M$natt, 2)
  expect_equal(M$nres, 1)
  expect_equal(M$ncon, 0L)
})

M <- Cogscimodel$new(y ~ x1 + x2 | x3, D, make_parspace(), mode="discrete")
test_that('Dimensions for y ~ x1 + x2 | x3', {
  expect_equal(dim(M$input), c(10, 2, 2))
  expect_equal(dim(M$res), c(10, 1))
  expect_equal(M$nobs, nobs(M))
  expect_equal(M$nobs, 10)
  expect_equal(M$nstim, nstim(M))
  expect_equal(M$nstim, 2)
  expect_equal(M$natt, natt(M))
  expect_equal(M$natt, c(2,1))
  expect_equal(M$nres, 1)
  expect_equal(M$ncon, 0L)
})

M <- Cogscimodel$new(y ~ x1 + x2 | x3 + x4, D, make_parspace(), mode="discrete")
test_that('Dimensions for y ~ x1 + x2 | x3 + x4', {
  expect_equal(dim(M$input), c(10, 2, 2))
  expect_equal(dim(M$res), c(10, 1))
  expect_equal(M$nobs, nobs(M))
  expect_equal(M$nobs, 10)
  expect_equal(M$nstim, nstim(M))
  expect_equal(M$nstim, 2)
  expect_equal(M$natt, natt(M))
  expect_equal(M$natt, c(2,2))
  expect_equal(M$nres, 1)
  expect_equal(M$ncon, 0L)
})

M <- Cogscimodel$new(y + z ~ x1 + x2 | x3 + x4, D, make_parspace(), mode="discrete")
test_that('Dimensions for y + yy ~ x1 + x2 | x3 + x4', {
  expect_equal(dim(M$input), c(10, 2, 2))
  expect_equal(dim(M$res), c(10, 2))
  expect_equal(M$nobs, nobs(M))
  expect_equal(M$nobs, 10)
  expect_equal(M$nstim, nstim(M))
  expect_equal(M$nstim, 2)
  expect_equal(M$natt, natt(M))
  expect_equal(M$natt, c(2,2))
  expect_equal(M$nres, 2)
  expect_equal(M$ncon, 0L)
})

M <- Cogscimodel$new(y~x1, D, make_parspace(), mode="discrete")
test_that('par methods with 0 parameter', {
  expect_equal(M$npar(), npar(M))
  expect_equal(M$npar(), 0)
  expect_equal(M$get_par(), NULL)
})

M <- Cogscimodel$new(y~x1, D, make_parspace(alpha=c(0,1)), mode="discrete", options = list(fit=FALSE))
test_that('par methods with 1 parameter', {
  expect_equal(M$npar(), npar(M))
  expect_equal(M$npar(), M$npar('free'))
  expect_equal(M$npar(), 1)
  expect_equal(M$npar(), M$npar('free'))
  expect_equal(M$npar('fix'), 0)
  expect_equal(M$npar('equal'), 0)
  expect_equal(M$npar('constrained'), 0)
  expect_equal(M$npar('constant'), 0)

  expect_equal(M$get_par(), c(alpha=0.5))
  expect_equal(M$get_par(), M$get_par('free'))
  expect_equal(length(M$get_par('fix')), 0)
  
  expect_equal(names(M$get_par()), 'alpha')
  expect_equal(names(M$get_par('fix')), NULL)
  expect_equal(names(M$get_par('equal')), NULL)
  expect_equal(names(M$get_par('constrained')), NULL)
})

M <- Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1), b=c(0,1)), mode="discrete", options = list(fit = FALSE))
test_that('par methods with 2 parameters', {
  expect_equal(M$npar(), npar(M))
  expect_equal(M$npar(), M$npar('free'))
  expect_equal(M$npar(), 2)
  expect_equal(M$npar(), M$npar('free'))
  expect_equal(M$npar('fix'), 0)
  expect_equal(M$npar('equal'), 0)
  expect_equal(M$npar('constrained'), 0)

  expect_equal(M$get_par(), c(a=0.5,b=0.5))
  expect_equal(M$get_par(), M$get_par('free'))
  expect_equal(length(M$get_par('fix')), 0)
  
  expect_equal(names(M$get_par()), c('a','b'))
  expect_equal(names(M$get_par('fix')), NULL)
  expect_equal(names(M$get_par('equal')), NULL)
  expect_equal(names(M$get_par('constrained')), NULL)
  expect_equal(names(M$get_par('constant')), NULL)
})


M <- Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1), b=c(0,1)), fix=c(a=1), mode="discrete", options = list(fit = FALSE))
test_that('par methods with 2 parameters, 1 fixed', {
  expect_equal(M$npar(), npar(M))
  expect_equal(M$npar(), 2)
  expect_equal(M$npar(), M$npar('all'))
  expect_equal(M$npar('fix'), 1)
  expect_equal(M$ncon, 1L)
  expect_equal(M$npar('free'), 1)
  expect_equal(M$npar('equal'), 0)
  expect_equal(M$npar('constrained'), 1)
  expect_equal(M$npar('constant'), 1)
  

  expect_equal(M$get_par(), c(a=1,b=0.5))
  expect_equal(M$get_par('fix'), c(a=1))
  expect_equal(M$get_par('free'), c(b=0.5))
  expect_equal(M$get_par('equal'), NULL)
  expect_equal(M$get_par('constant'), M$get_par('fix'))
  
  expect_equal(names(M$get_par()), c('a','b'))
  expect_equal(names(M$get_par('fix')), 'a')
  expect_equal(names(M$get_par('free')), 'b')
  expect_equal(names(M$get_par('equal')), NULL)
  expect_equal(names(M$get_par('constrained')), 'a')
  expect_equal(names(M$get_par('constant')), names(M$get_par('fix')))
})


M <- Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1), b=c(0,100)), fix=c(a=1, b=100), mode="discrete")
test_that('par methods with 2 parameters, 2 fixed', {
  expect_equal(M$npar(), npar(M))
  expect_equal(M$npar(), 2)
  expect_equal(M$npar(), M$npar('all'))
  expect_equal(M$npar('free'), 0)
  expect_equal(M$npar('fix'), 2)
  expect_equal(M$npar('constant'), M$npar('fix'))
  expect_equal(M$npar('equal'), 0)
  expect_equal(M$npar('constrained'), 2)
  expect_equal(M$npar('constant'), 2)
  expect_equal(M$ncon, 2L)

  expect_equal(M$get_par(), c(a=1,b=100))
  expect_equal(M$get_par('fix'), c(a=1,b=100))
  expect_equal(M$get_par('constant'), M$get_par('fix'))
  expect_equal(length(M$get_par('free')), 0)
  expect_equal(length(M$get_par('equal')), 0)
  
  expect_equal(names(M$get_par()), c('a','b'))
  expect_equal(names(M$get_par('fix')), c('a','b'))
  expect_equal(names(M$get_par('constant')), names(M$get_par('fix')))
  expect_equal(names(M$get_par('free')), NULL)
  expect_equal(names(M$get_par('equal')),NULL)
  expect_equal(names(M$get_par('constrained')), c('a','b'))
})

M <- Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1), b=c(0,1)), fix=list(b='a'), mode="discrete", options = list(fit = FALSE))
test_that('par methods with 2 parameters, 1 equality-constrained', {
  expect_equal(M$npar(), npar(M))
  expect_equal(M$npar(), 2)
  expect_equal(M$npar(), M$npar('all'))
  expect_equal(M$npar('free'), 1)
  expect_equal(M$npar('fix'), 1)
  expect_equal(M$npar('equal'), 1)
  expect_equal(M$npar('constrained'), 2)
  expect_equal(M$npar('ignored'), 0)
  expect_equal(M$npar('constant'), 0)
  expect_equal(M$ncon, 1L)

  expect_equal(M$get_par(), c(a=0.5,b=0.5))
  expect_equal(M$get_par('fix'), c(b=0.5))
  expect_equal(M$get_par('free'), c(a=0.5))
  expect_equal(M$get_par('equal'), c(b=0.5))
  expect_equal(M$get_par('constant'), NULL)
  
  expect_equal(names(M$get_par()), c('a','b'))
  expect_equal(names(M$get_par('fix')), c('b'))
  expect_equal(names(M$get_par('free')), c('a'))
  expect_equal(names(M$get_par('equal')), c('b'))
  expect_equal(names(M$get_par('constrained')), c('a','b'))
  expect_equal(names(M$get_par('constant')), NULL)
})

M <- Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1), b=c(0,1), c=c(-2,2), d=c(-2,0)), fix=list(b='a', d='c'), mode="discrete", options = list(fit = FALSE))
test_that('par methods with 4 parameters, 2 equality-constrained', {
  expect_equal(M$npar(), npar(M))
  expect_equal(M$npar(), 4)
  expect_equal(M$npar(), M$npar('all'))
  expect_equal(M$npar('free'), 2)
  expect_equal(M$npar('fix'), 2)
  expect_equal(M$npar('constant'), 0)
  expect_equal(M$npar('equal'), 2)
  expect_equal(M$npar('constrained'), 4)
  expect_equal(M$npar('ignored'), 0)
  expect_equal(M$ncon, 2L)

  expect_equal(M$get_par(), c(a=0.5,b=0.5,c=0,d=0))
  expect_equal(M$get_par('fix'), c(b=0.5,d=0))
  expect_equal(M$get_par('equal'), M$get_par('fix'))
  expect_equal(M$get_par('free'), c(a=0.5,c=0))
  expect_equal(M$get_par('ignored'), NULL)
  expect_equal(M$get_par('constant'), NULL)
  expect_equal(M$get_par('constrained'), c(a=0.5, b=0.5, c=0, d=0))
  
  expect_equal(names(M$get_par()), c('a','b','c','d'))
  expect_equal(names(M$get_par('fix')), c('b','d'))
  expect_equal(names(M$get_par('free')), c('a','c'))
  expect_equal(names(M$get_par('equal')), names(M$get_par('fix')))
  expect_equal(names(M$get_par('constant')), NULL)
  expect_equal(names(M$get_par('constrained')), c('a','b','c','d'))
})

M <- Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1), b=c(0,1), c=c(-2,2), d=c(-2,0)), fix=list(a=1, b='a', d='c'), mode="discrete", options = list(fit = FALSE))
test_that('par methods with 4 parameters, 1 fix to a number, 2 equality-constrained', {
  expect_equal(M$npar(), npar(M))
  expect_equal(M$npar(), 4)
  expect_equal(M$npar(), M$npar('all'))
  expect_equal(M$npar('free'), 1)
  expect_equal(M$npar('fix'), 3)
  expect_equal(M$npar('equal'), 2)
  expect_equal(M$npar('constrained'), 4)
  expect_equal(M$npar('ignored'), 0)
  expect_equal(M$npar('constant'), 1)
  expect_equal(M$ncon, 3L)

  expect_equal(M$get_par(), c(a=1,b=1,c=0,d=0))
  expect_equal(M$get_par('fix'), c(a=1,b=1,d=0))
  expect_equal(M$get_par('equal'), c(b=1,d=0))
  expect_equal(M$get_par('free'), c(c=0))
  expect_equal(M$get_par('ignored'), NULL)
  expect_equal(M$get_par('constant'), c(a=1))
  
  expect_equal(names(M$get_par()), c('a','b','c','d'))
  expect_equal(names(M$get_par('fix')), c('a','b','d'))
  expect_equal(names(M$get_par('free')), c('c'))
  expect_equal(names(M$get_par('equal')), c('b','d'))
  expect_equal(names(M$get_par('ignored')), NULL)
  expect_equal(names(M$get_par('constant')), c('a'))
})

test_that("Constraints", {
  # Constrained to a constant (5)
  PS <- make_parspace(a=c(0,1,0.5,0.01), b=c(0,1,0.5,0.02), c=c(-2,2,0,0.03), d=c(-2,0,-1,-0.04))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(b=5))
  expect_equal(names(M$get_par("free")), c("a", "c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(b=5, c=5))
  expect_equal(names(M$get_par("free")), c("a", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a=5, b=5, c=5, d=5))
  expect_equal(names(M$get_par("free")), NULL)
  # Constrained to another parameter
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(b="c"))
  expect_equal(names(M$get_par("free")), c("a", "c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(b="c", a="d"))
  expect_equal(names(M$get_par("free")), c("c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a="b", b="c", c="d"))
  expect_equal(names(M$get_par("free")), c("d"))
  # Constrained to a constant and a parameter
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(b=5, a="d"))
  expect_equal(names(M$get_par("free")), c("c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(b=5, a="b"))
  expect_equal(names(M$get_par("free")), c("c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(b=5, a="d"))
  expect_equal(names(M$get_par("free")), c("c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(b="a", a=5))
  expect_equal(names(M$get_par("free")), c("c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a="b", b="c", c="d", d=0.5))
  expect_equal(names(M$get_par("free")), NULL)
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a="b", b=1, c=2, d=0.5))
  expect_equal(names(M$get_par("free")), NULL)
  # Constrained to na
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a=NA))
  expect_equal(names(M$get_par("free")), c("b", "c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a=NA, b=NA))
  expect_equal(names(M$get_par("free")), c("c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a=NA, b=NA, c=NA, d=NA))
  expect_equal(names(M$get_par("free")), NULL)
  # Constrained to a constant and NA
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(b=5, a=NA))
  expect_equal(names(M$get_par("free")), c("c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a="b", b="c", c="d", d=NA))
  expect_equal(names(M$get_par("free")), NULL)
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a=NA, b="c", c="d", d="a"))
  expect_equal(names(M$get_par("free")), NULL)
  # Constrained to a constant and NA
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(b=5, a=NA))
  expect_equal(names(M$get_par("free")), c("c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(b=5, a=NA))
  expect_equal(names(M$get_par("free")), c("c", "d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a=NA, b=NA, c=NA, d=0.5))
  expect_equal(names(M$get_par("free")), NULL)
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a=NA, b=1, c=2, d=0.5))
  expect_equal(names(M$get_par("free")), NULL)
  # Constrained to a constant, NA, and a parameter
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a=5, b=NA, c="a"))
  expect_equal(names(M$get_par("free")), c("d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a=5, b="a", c=NA))
  expect_equal(names(M$get_par("free")), c("d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a=NA, b=5, c=NA))
  expect_equal(names(M$get_par("free")), c("d"))
  M <- Cogscimodel$new(y~x1, parspace=PS, mode="discrete", fix=list(a=NA, b="a", c=5))
  expect_equal(names(M$get_par("free")), c("d"))
})

test_that('Error messages', {
  expect_error(Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1)), fix=list(b=1), mode="discrete"))
   expect_error(Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1)),fix=list(b='c'), mode="discrete"))
  expect_error(Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1)),fix=list(b=1,b=2)), mode="discrete")
  expect_error(Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1)),fix=list(a=10), mode="discrete"))
  expect_error(Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1,2)),fix=list(a=1), mode="discrete"))
  expect_error(Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1,-2)),fix=list(a=1), mode="discrete"))
  expect_error(Cogscimodel$new(y~x1, D, make_parspace(a=c(1,0)),fix=list(a=1), mode="discrete"))
  expect_error(Cogscimodel$new(y~x1, D, make_parspace(a=c(0,1), b=c(5,6)), fix=list(a='b'), mode="discrete"))
})

test_that("Grid nsteps with parameters between 0 - 1", {
  PS <- make_parspace(a=c(0,1), b=c(0,1), c=c(0,1), d=c(0,1), e = c(0,1))
  O <- list(fit_solver = c("grid", "solnp"), fit = FALSE)
  M <- Cogscimodel$new(y ~ x1, D, PS[1,], mode = "discrete", options = O)
  expect_equivalent(M$options$fit_control$nsteps, 3)

  M <- Cogscimodel$new(y ~ x1, D, PS[1:2,], mode = "discrete", options = O)
  expect_equivalent(M$options$fit_control$nsteps, rep(3,2))

  M <- Cogscimodel$new(y ~ x1, D, PS[1:3,], mode = "discrete", options = O)
  expect_equivalent(M$options$fit_control$nsteps, rep(3,3))

  M <- Cogscimodel$new(y ~ x1, D, PS[1:4,], mode = "discrete", options = O)
  expect_equivalent(M$options$fit_control$nsteps, rep(4,4))

  M <- Cogscimodel$new(y ~ x1, D, PS[1:5,], mode = "discrete", options = O)
  expect_equivalent(M$options$fit_control$nsteps, rep(5,5))
})

test_that("Grid nsteps with parameters between 0 - 20", {
  PS <- make_parspace(a=c(0,1), b=c(0,10), c=c(0,1), d=c(0,20), e = c(0,1))
  O <- list(fit_solver = c("grid", "solnp"), fit = FALSE)

  M <- Cogscimodel$new(y ~ x1, D, PS[1:2,], mode = "discrete", options = O)
  expect_equivalent(M$options$fit_control$nsteps, c(3,5))

  M <- Cogscimodel$new(y ~ x1, D, PS[1:3,], mode = "discrete", options = O)
  expect_equivalent(M$options$fit_control$nsteps, c(3,5,3))

  M <- Cogscimodel$new(y ~ x1, D, PS[1:4,], mode = "discrete", options = O)
  expect_equivalent(M$options$fit_control$nsteps, c(4,6,4,8))

  M <- Cogscimodel$new(y ~ x1, D, PS[1:5,], mode = "discrete", options = O)
  expect_equivalent(M$options$fit_control$nsteps, c(5,7,5,10,5))
})

test_that("Sum constraints equal identities", {
  PS <- make_parspace(a=c(0,1), b=c(0,1), c=c(0,1), d=c(0,1))

  M <- Cogscimodel$new(y ~ x1, D, PS, mode = "discrete", options = list(fit = FALSE))
  expect_equivalent(M$constraints, NULL)
  expect_equivalent(M$ncon, 0L)
  expect_equivalent(M$ncon, M$npar("fix"))

  M <- Cogscimodel$new(y ~ x1, D, PS, fix=list(a=0.5), mode = "discrete", options = list(fit = FALSE))
  expect_equivalent(M$constraints, L_constraint(L = rbind(a=c(1,0,0,0)), rhs = 0.5, dir = "==", names = names(M$get_par())))
  expect_equivalent(M$ncon, 1L)
  expect_equivalent(M$ncon, M$npar("fix"))

  M <- Cogscimodel$new(y ~ x1, D, PS, fix=list(a=0.5, b=0.7), mode = "discrete", options = list(fit = FALSE))
  expect_equivalent(M$constraints, L_constraint(L = rbind(a=c(1,0,0,0), b=c(0,1,0,0)), rhs = c(0.5, 0.7), dir = c("==", "=="), names = names(M$get_par())))
  expect_equivalent(M$ncon, 2L)
  expect_equivalent(M$ncon, M$npar("fix"))

  M <- Cogscimodel$new(y ~ x1, D, PS, fix =list(a="d", b=0.5), mode="discrete", options = list(fit = FALSE))
  expect_equivalent(M$constraints, rbind(L_constraint(rbind(b=c(0,1,0,0)), "==", 0.5, names(M$get_par())), L_constraint(rbind(c(1,0,0,-1)), "==", 0, names = names(M$get_par())), use.names = TRUE))
  expect_equivalent(M$ncon, 2L)
  expect_equivalent(M$ncon, M$npar("fix"))

  M <- Cogscimodel$new(y ~ x1, D, PS, fix=list(a=1,b=0.99,c="d"), mode = "discrete", options = list(fit = FALSE))
  expect_equivalent(M$constraints, rbind(
    L_constraint(rbind(a=c(1,0,0,0),b=c(0,1,0,0)), c("==","=="), c(1,0.99), names(M$get_par())),
    L_constraint(rbind(c(0,0,1,-1)), "==", 0, names(M$get_par())), use.names = TRUE))
  expect_equivalent(M$ncon, 3L)
  expect_equivalent(M$ncon, M$npar("fix"))
})

test_that("Zero RHS of formula", {
  expect_equal(Cogscimodel$new( ~ x1, D, mode = "discrete")$res, NULL)
  expect_equivalent(Cogscimodel$new(y ~ x1, D, mode = "discrete")$res, D[, "y", drop=FALSE])
})

C <- ROI::L_constraint(
  rbind(c(1,1,0,0), c(0,0,1,0), c(0,0,0,0)),
  rep("==", 3),
  c(1,2,0),
  letters[1:4])
class(C) <- "csm_constraint"
C
qrcoef <- qr.coef(qr(C$L), C$rhs)
fitpar <- C$names[qrcoef==0 | is.na(qrcoef)]

fitpar

