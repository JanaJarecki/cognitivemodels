# Cognitivemodels
Cognitivemodels is an R packages that provides a number of cognitive models, using a formula interface similar to the `aov`- or `lm`-syntax. The models can be fit to data by maximum likelihood, minimum MSE, and more fit measures using  optimization routines like rsolnp or Nelder-Mead, and various others. Also, the package provides a model development back end: a class that you can use to develop new cognitive models easily.


## News
For new features in the latest package version, see [NEWS](NEWS.md).

## Getting Started and Installation
To use this package, ensure you have a working installation of R and the **Rcpp** package, `install.packages("Rcpp")`, for problems see [Installation Troubleshooting](#Installation-Troubleshooting)

```R
    library(devtools)
    install.packages("matlib")
    install.packages("Rcpp")

    # ---------------------------------------------
    # Restart the R session after installing matlib
    # ----------------------------------------------
    
    devtools::install_github("janajarecki/cognitivemodels")
```

You will see a prompt, please type `Yes` into the console.

```
    Do you want to install from sources the packages which need compilation? (Yes/no/cancel) 
```

(Optional) This installs the latest development version of the package:

```R
    devtools::install_github("janajarecki/cognitivemodels" ref = "development")
```


To use the package:

```R
    library(cognitivemodels)
```

#### Installation Troubleshooting
Here are some error messages during the installation and how to troubleshoot them:

* Error: `Failed to install 'cognitivemodels' from GitHub:   Could not find tools necessary to compile a package Call `pkgbuild::check_build_tools(debug = TRUE)` to diagnose the problem.` **Solution**: In R, you must run `options(buildtools.check = function(action) TRUE )` to solve the problem, [more details see here](https://stackoverflow.com/questions/37776377/error-when-installing-an-r-package-from-github-could-not-find-build-tools-neces)
* Error: `xcrun: error: invalid active developer path (/Library/Developer/CommandLineTools), missing xcrun at:` **Solution** You need to install xcode which you do on Mac by opening the terminal and running `xcode-select --install
` to install the command lines tools package [more details on stackoverflow](https://apple.stackexchange.com/questions/254380/why-am-i-getting-an-invalid-active-developer-path-when-attempting-to-use-git-a)
    
## Models in this Package
The following models are implemented:

Model | Reference | Documentation of the Function
------------ | ------------- | -------------
Memory-based preference model | Jarecki & Rieskamp (submitted) | `?mem`
Generalized context model | Medin & Schaffer (1976); Nosofsky (1986) | `?ebm`
Optimal risk-sensitive foraging model | Houston & McNamara (1988) | `?hm1988`
Cumulative prospect theory | Kahneman & Tversky (1979); Tversky & Kahneman (1992) | `?cpt`
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
