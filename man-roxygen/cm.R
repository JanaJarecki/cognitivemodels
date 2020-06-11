#' @param formula A [stats::formula()], the variables in `data` to be modelled. For example, `y ~ x1 + x2 | z1 + z2` models the response `y` as function of one stimulus with features `x1`, `x2` and one stimulus with features `z1`, `z2`. Horizontal lines (`|`) separate different stimuli.
#' @param data A data frame.
#' 
#' @return Returns a cognitive model object, which has the class `cm`(cm). A model, that is saved as `m`, can be summarized with `summary(m)` or `anova(m)`. The parameter space can be viewed using `parspace(m)`, constraints can be viewed using `constraints(m)`.
#' @family cognitive models
