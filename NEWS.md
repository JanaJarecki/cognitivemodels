# cognitivemodels 0.0.10

**New features**

* Added an optional argument `prior_sum` to Bayesian learning models to set and control the sum constraints of the prior (hyper-)parameter, see `?bayes`
* Added a new model `shift_c()` that models a change point in time for shifting between two values or two predictions. See `?shift`
* In `predict.cm` changed the order or arguments such that the argument `newdata` is at the second position to comply with `predict.glm` methods

**Bugfixes**

* Fixed bugs in ROI optimization
* Fixed bug in `add_constraints()` in `cognitivemodel` lego variant
* Made dropping of choicerule error and warning easier to understand.
* Declared `solvers()` as depreciated with a warning, from now on use `cm_solvers()` to show the optimization solvers.

# cognitivemodels 0.0.9

* Added a `NEWS.md` file to track changes to the package.
