context("baseline")
library(cogscimodels)

D <- data.frame(y = c(1,1,0), x = c(1,2,3), z = c(0,4,2))
test_that("Predicted Values of type=onstant Model ", {
  M <- baseline_const(y ~ ., D[1, ], 0.5)
  M2 <- baseline(y ~ ., D[1, ], "constant", 0.5)
  expect_equal(M$predict(), cbind(pred_y=0.5))
  expect_equal(M$predict(), M2$predict())

  M <- baseline_const(y ~ ., data = D, const = 0.7)
  M2 <- baseline(y ~ ., data = D, type = "constant", const = 0.7)
  expect_equal(M$predict(), cbind(pred_y=rep(0.7, nrow(D))))
  expect_equal(M$predict(), M2$predict())

  M <- baseline_const(y ~ ., D[1,], 0.3)
  expect_equal(M$predict(), cbind(pred_y=0.3))
}
)


D <- data.frame(y = c(1,1,0), x = c(1,2,3), z = c(0,4,2))
test_that("Predicted Values of type=mean Model ", {
  M <- baseline_mean(y ~ ., data = D[1, ])
  M2 <- baseline(y ~ ., data = D[1, ], type = "mean")
  expect_equal(M$predict(), cbind(pred_y=1))
  expect_equal(M$predict(), M2$predict())
  expect_equal(M$npar(), 1)

  M <- baseline_mean(y ~ ., data = D)
  M2 <- baseline(y ~ ., data = D, type = "mean")
  expect_equal(M$predict(), cbind(pred_y=rep(2/3, nrow(D))))
  expect_equal(M$predict(), M2$predict())
  expect_equal(M$npar(), 1)
}
)