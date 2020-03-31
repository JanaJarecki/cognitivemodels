# Cognitivemodels

This is an R packages containing various cognitive models.

## Getting started
To use this package, ensure you have the prerequesites and then follow the installing code.

### Prerequisites
You need a working installation of R, and the Rcpp package:

    install.packages("Rcpp")

### Installing this R package
To use the cognitive models library, install and load it like so:

    # Install (once)
    install.packages("devtools")
    devtools::install_github("janajarecki/cognitivemodels")

    # Load to use
    library(cogscimodels)

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
Baseline model | | `?baseline`
Shortfall model | Andraszewicz (2014) | `?shortfall`

    
# Authors
Jana B. Jarecki
Florian I. Seitz

# License
This project is licensed under CC-By Attribution 4.0 International
