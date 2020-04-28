#' Models for Choice Rules (decision rules for action selection)
#' 
#' \code{softmax()} soft-maximizes, \code{argmax()} maximizes deterministically, \code{epsilon_greedy()} is epsilon-greedy, \code{epsilon()} is probabilistic-epsilon, \code{luce()} selects proportionally (Luce's rule).
#' 
#' @importFrom cognitiveutils cr_softmax
#' @importFrom cognitiveutils cr_epsilon
#' @importFrom cognitiveutils cr_argmax
#' @importFrom cognitiveutils cr_luce
#' 
#' @inheritParams Cm
#' @rdname choicerules
#' @param formula A \code{\link{formula}} like responses ~ action-values (e.g.,  \code{y ~ a | b}). \emph{Note}, that bars (\code{|}) separate different actions.
#' 
#' @return An object of class R6 holding a model.
#' 
#' @author Jana B. Jarecki
#' 
#' @section Parameter Space:
#' \tabular{llrcllr}{\tab \verb{   }\strong{Name} \tab \verb{    }\strong{LB} \tab  \strong{-} \tab \strong{UB}\verb{    } \tab \strong{Description} \tab \strong{Start Value}\cr
#' \code{softmax()} \tab \verb{   }\code{tau} \tab  0.0001 \tab  - \tab  10 \tab  Temparature, small \code{tau} yields maximizing, high yields randomizing \tab  1\cr
#' \code{epsilon()} \tab \verb{   }\code{eps} \tab  0 \tab  - \tab  1 \tab  Exploration probability; small \code{eps} yields original input, high yields randomizing \tab  0.5 \cr
#' \code{epsilon_greedy()} \tab \verb{   }\code{eps} \tab  0 \tab  - \tab  1 \tab  Exploration probability; small \code{eps} yields maximizing, high yields randomizing \tab  0.5
#' }
#' 
#' @details This is how the model predicts and treats observations:
#' \itemize{
#'    \item{For \code{y ~ a} it predicts 1 response column (\code{pred_a}) and interprets observations in \code{y} as \emph{0 = \bold{not}-a}, \emph{1 = a}}
#'    \item{For \code{y ~ a | b} it predicts 2 response columns (\code{pred_a, pred_b}) and interprets observations in \code{y} as \emph{0 = a}, \emph{1 = b}}
#' \item{For \code{y ~ a | b | c} it predicts 3 response columns (\code{pred_a, pred_b, pred_c}) and interprets observations in \code{y} as \emph{0 = a}, \emph{1 = b}, \emph{2 = c}}
#' \item{etc.}
#' }
#' 
#' \code{softmax()} picks action \eqn{i} with \eqn{exp[v(i)]/\tau / \sum exp[v]/\tau}. It equals a logistic action selection for binary actions and \eqn{\tau=1}.
#' 
#' @family Choicerules for cognitive models
#' 
#' @references Sutton, R. S., & Barto, A. G. (2018). \emph{Reinforcement Learning: An Introduction (2nd Ed.)}. MIT Press, Cambridge, MA. \url{http://incompleteideas.net/book/the-book-2nd.html}
#' @references Luce, R. D. (1959). On the possible psychophysical laws. \emph{Psychological Review, 66(2)}, 81-95. \url{https://doi.org/10.1037/h0043178}
#' 
#' @examples
#' # Make some fake data
#' D <- data.frame(a = c(.1,.4,.5),       # value of option A
#'                 b = c(.7,.6,.5),       # value of option B
#'                 y = c(0,1,1))          # respondent's choice (0=A, 1=B)
#' 
#' M <- softmax(y ~ a | b, D, c(tau=1))   # creates soft-max model w tau=1
#' 
#' predict(M)                             # predict action selection
#' M$predict()                            # -- (same) --
#' summary(M)                             # summarize
#' anova(M)                               # anova-like table
#' coef(M)                                # free parameter (NULL)
#' M$get_par()                            # fixed parameter (tau = 1)
#' M$npar()                               # 1 parameter
#' M$MSE()                                # mean-squared error
#' logLik(M)                              # log likelihood
#' 
#' 
#' ### Parameter specification and fitting
#' # -------------------------------------
#' softmax(y ~ a | b, D, fix="start")     # fix 'tau' to its start value
#' softmax(y ~ a | b, D, fix=c(tau=0.2))  # fix 'tau' to 0.2
#' softmax(y ~ a | b, D)                  # fit 'tau' to data y in D
#' 
#' 
#' ### The different choice rules
#' # -----------------------------------
#' softmax(y ~ a | b, D,  fix=c(tau=0.5)) # fix 'tau' to 0.5
#' softmax(y ~ a | b, D)                  # fit 'tau' to y
#' @export
softmax <- function(formula, data, fix = list(), options = NULL) {
  .args <- as.list(match.call()[-1])
  .args[["type"]] <- "softmax"
   return(do.call(what = Choicerule$new, args = .args, envir = parent.frame()))
}


#' @rdname choicerules#' 
#' @family Choicerules for cognitive models
#' @details
#' \code{epsilon_greedy()} picks the best action with probability \eqn{1 - \epsilon}, and with \eqn{\epsilon} it picks randomly from all actions, including the best.
#' @examples
#' epsilon_greedy(y~a | b, D, c(eps=0.1)) # fix 'eps' to 10 %
#' epsilon_greedy(y~a | b, D )            # fit 'eps' to y
#' @export
epsilon_greedy <- function(formula, data, fix = list(), options = NULL) {
  .args <- as.list(match.call()[-1])
  .args[["type"]] <- "epsilongreedy"
   return(do.call(what = Choicerule$new, args = .args, envir = parent.frame()))
}


#' @inheritParams softmax
#' @rdname choicerules#' 
#' @family Choicerules for cognitive models
#' @details
#' \code{epsilon()} picks action \eqn{i} with probability \eqn{(1 - \epsilon)*p(i)} and with \eqn{\epsilon} it picks randomly from all actions. For \eqn{\epsilon = 0} it gives \eqn{p(i)}, that is the  original probabilistic policy.
#' @examples
#' epsilon(y ~ a | b, D, c(eps=0.1))      # fix 'eps' to 0.1
#' epsilon(y ~ a | b, D)                  # fit 'eps' to y
#' @export
epsilon <- function(formula, data, fix = list(), options = NULL) {
  .args <- as.list(match.call()[-1])
  .args[["type"]] <- "epsilon"
   return(do.call(what = Choicerule$new, args = .args, envir = parent.frame()))
}

#' @rdname choicerules
#' @family Choicerules for cognitive models
#' @details
#' \code{luce()} picks action \eqn{i} with probability \eqn{v(i) / \sum v}.
#' @examples
#' luce(y ~ a | b, D)                     # Luce's choice rule
#' @export
luce <- function(formula, data, ...) {
  .args <- as.list(match.call()[-1])
  .args[["type"]] <- "luce"
   return(do.call(what = Choicerule$new, args = .args, envir = parent.frame()))
}

#' @rdname choicerules
#' @family Choicerules for cognitive models
#' @details
#' \code{argmax()} picks the highest-values action with probability 1, and in case of ties it picks equi-probable
#' @examples
#' argmx(y ~ a | b, D)                    # Argmax choice rule
#' @export
argmax <- function(formula, data, ...) {
  .args <- as.list(match.call()[-1])
  .args[["type"]] <- "argmax"
   return(do.call(what = Choicerule$new, args = .args, envir = parent.frame()))
}


#' @noRd
#' @export
Choicerule <- R6Class("choicerule",
  inherit = Cm,
  public = list(
    type = NULL,
    initialize = function(formula, data = NULL, type, fix = NULL, options=list()) {
      type <- match.arg(type, c("softmax", "argmax", "luce", "epsilon", "epsilongreedy"))
      self$type <- type
      parspace <- switch(type,
        epsilon =       make_parspace(eps = c(0,1,0.5,0)),
        epsilongreedy = make_parspace(eps = c(0,1,0.5,0)),
        softmax =       make_parspace(tau = c(0.0001,10,2,NA)),
        make_parspace()
      )
      super$initialize(
        title = type,
        formula = formula,
        data = data,
        parspace = parspace,
        mode = "discrete",
        fix = fix,
        options = options,
        choicerule = "none"
      )
    },
    predict = function(type = c("response"), newdata = NULL) {
      if (is.null(newdata) | missing(newdata)) {
        D <- self$input
      } else {
        D <- private$get_input(d = newdata)
      }
      fun <- switch(self$type,
        softmax = cognitiveutils::cr_softmax,
        argmax = cognitiveutils::cr_argmax,
        luce = cognitiveutils::cr_luce,
        epsilon = cognitiveutils::cr_epsilon,
        epsilongreedy = function(x, eps) {
          cognitiveutils::cr_epsilon(
            x = matrix(
              data = cognitiveutils::cr_argmax(x),
              nrow = self$nobs),
            eps = self$get_par())
        }
      )
      .args <- list(x = abind::adrop(aperm(D, c(1,3,2)), 3))
      .args <- c(.args, self$par)

      res <- do.call(what = fun, args = .args, envir = parent.frame())
      res <- matrix(res, nrow = dim(D)[1])
      colnames(res) <- paste0("pr_", private$get_stimnames())
      return(res[, 1:ncol(res)])
    }
  )
)