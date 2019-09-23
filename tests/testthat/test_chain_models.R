context("chain models")


test_that("Model chain - prediced value identities", {
  D <- data.frame(x1 = 1, x2 = 2, obsx = c(0,0,0), y1=1, y2=0, obsy = c(1,0,1), y=1)
  cpar <- c(alpha=0.88, beta=0.88, lambda=2.25, gammap=0.61, gamman=0.69)
  bpar <- list(delta=1, priorpar = c(1,1,1,1))

  M <- start(data = D) %+%
    bayes_beta(~ obsx | obsy, fix = bpar) %>%
    end()
  expect_equal(M$predict(), cbind(pred_obsx = c(0.5,0.33,0.25), pred_obsy = c(0.5, 0.66,.50)), tol = 0.01)

  M <- start(data = D) %+§lsp%
    bayes_beta(~ obsx | obsy, fix = bpar) %+%
    softmax(~ pred_obsx | pred_obsy, fix = c(tau = 1)) %>%
    end()
  expect_equal(M$predict(), cbind(pred_prd_bsx = c(0.5,0.42,0.43), pred_prd_bsy = c(0.5, 0.58,.56)), tol = 0.01)

  M <- start(data = D) %+%
    bayes_beta(~ obsx | obsy, fix = bpar) %+%
    cpt(~ x1 + pred_obsx + x2 | y1 + pred_obsy + y2, ref = 0L, fix = cc) %>%
    end()
  expect_equal(M$predict(), cbind(pred_x = c(1.35,1.43,1.46), pred_y = c(0.42,0.51,0.42)), tol = 0.01)

  M <- start(data = D) %+%
    bayes_beta(~ obsx | obsy, fix = bpar) %+%
    cpt(~ x1 + pred_obsx + x2 | y1 + pred_obsy + y2, ref = 0L, fix = cc) %+%
    luce(~ pred_x | pred_y) %>%
    end()
  expect_equal(M$predict(), cbind(pred_prd_x = c(0.76,0.73,0.77), pred_prd_y = c(0.23,0.26,0.22)), tol = 0.01)
})

M <- start(data = D) %+%
    bayes_beta(y ~ obsx | obsy, fix = bpar) %+%
    softmax(y ~ pred_obsx | pred_obsy, fix = c(tau = 1)) %>%
    fun(function(pred, par) { pred - par["tau"] } ) %>%
    end()

    yy <- cbind(pred_prd_bsx = c(0.5,0.42,0.43), pred_prd_bsy = c(0.5, 0.58,.56))

    yy-2

M$predict()

   sqrt(M$predict())