# Cognitivemodels

Cognitivemodels is an R packages containing a growing number of cognitive models using a formula interface similar to `lm. The models can be fit to data by maximum likelihood, minimum MSE, RMSE, and more fit measures using various optimization routines. Cognitivemodls also provides a back-end for making it easy to write your own cognitive models.

## Getting Started and Installation
To use this package, ensure you have the prerequesites (working installation of R) and the Rcpp package (`install.packages("Rcpp")`), and install the package

    ```r
    # Install  development version from github
    install.packages("devtools")
    devtools::install_github("janajarecki/cognitivemodels")
    ```r

To use the package, you need to load it `library(cogscimodels)`

#### Installation Problems
If you use Rstudio you may encounter the "Error: Could not find tools necessary to compile a package". If so, following [this solution](https://stackoverflow.com/questions/37776377/error-when-installing-an-r-package-from-github-could-not-find-build-tools-neces), you can run `pkgbuild::check_build_tools(debug = TRUE)` to diagnose the problem; and most likely you need to run `options(buildtools.check = function(action) TRUE )` to solve the problem.
    
## Models in this Package
The cognitive models in this package follow the `lm`-type syntax, which you may be familiar with. The following models are implemented:

Model | Reference | Documentation of the Function
------------ | ------------- | -------------
Memory-based preference model | Jarecki & Rieskamp (submitted) | `?mem`
Generalized context model | Medin & Schaffer (1976); Nosofsky (1986) | `?ebm`
Optimal risk-sensitive foraging model | Houston & McNamara (1988) | `?hm1988`
Cumulative prospect theory | Kahneman & Tversky (1979); Tversky & Kahneman () 1992) | `?cpt`
Bayesian cognitive model | Hoffart, Jarecki, Duthil, & Rieskamp (under review) | `?bayes`
Power utility | Tversky (1967); Wakker (2008)| `?utility_pow`
Soft-max choice rule | Sutton & Barto (1998) | `?softmax`
Arg-max choice rule | | `?argmax`
Epsilon-greedy choice rule |  | `?epsilon`
Baseline model | | `?baseline`
Shortfall model | Andraszewicz (2014) | `?shortfall`

    
# Authors
Jana B. Jarecki,  
Florian I. Seitz

# License
This project is licensed under CC-By Attribution 4.0 International
