# <img src="man/figures/logo_inverted.png" align="right" />

The `cognitivemodels` package offers a user-friendly collection of machine-learning  algorithms to train and test models of human learning, behavior, and cognition.

* Its syntax uses a formula interface resembling the `aov(y ~ x)`- or `lm(y ~ x)`-syntax.
* The models can be fit to data by maximum likelihood, minimum MSE, and other fit measures
* Chose optimization routines like rsolnp or Nelder-Mead, among others
* Also, the package provides a model development back end, i.e. a class to develop new cognitive models easily


## News
You can see the latest package version's new features in [NEWS](NEWS.md).

## Algorithms
Find a list of the cognitive machine-learning algorithms in this package under [MODELS](MODELS.md). You can use them for forecasting individual-based decisions about risks and about probabilities, modeling and predicting customer and consumer preferences, analyzing human learning based on prior information, and person-specific utilities.


## Installing this Package
To use this package, ensure that you have a working installation of R and the **Rcpp** package, `install.packages("Rcpp")`, for help with problems see [Installation Troubleshooting](#Installation-Troubleshooting)

### Installation
```R
    library(devtools)
    install.packages("matlib")
    install.packages("Rcpp")
    # Restart the R session after installing matlib! 
    devtools::install_github("janajarecki/cognitivemodels")
```

You will see a prompt `Do you want to install from sources the packages which need compilation?`, please type `Yes` into the console.

### Using the package
To use the package, run:

```R
    library(cognitivemodels)
```

### Installation of the latest version
(Optional) This installs the newest version (development version) of this package:

```R
    devtools::install_github("janajarecki/cognitivemodels" ref = "development")
```


## Getting Started with Behavior Modeling
You can read a quick introduction to how the algorithms in this package work in the [ICCM](https://psyarxiv.com/6kb4w/) article; and you can go through an example code below.

_Example._ Let's fit data from a supervised categorization task and use the trained model to predict categorizations. 

_Background._ The categorization task had people learn to categorize many lines that differed in two features (size and tilting angle) into two categories, providing feedback about the true category (Nosofsky, 1989). The collected data can be loaded ba running `data(nosofsky1989long)`. Let's model data in one condition from this data set called "size".

_Code._ The syntax below loads the data and sets up the model, it is explained below the code.

```R
  # Use the 'size' condition in the data
  data(nosofsky1989long)
  DT <- nosofsky1989long
  DT <- DT[DT$condition=="size", ]
  D  <- DT[!is.na(DT$true_cat), ]
  
  # Fit the model to the data D
  model <- gcm(
    formula = response ~ angle + size,
    class = ~ true_cat,
    data = D,
    choicerule = "none")

    # Make a prediction
    predict(model)
```

#### Installation Troubleshooting
Below find some messages during the installation and how to troubleshoot them:

* Error: `Failed to install 'cognitivemodels' from GitHub:   Could not find tools necessary to compile a package Call `pkgbuild::check_build_tools(debug = TRUE)` to diagnose the problem.` **Solution**: In R, you must run `options(buildtools.check = function(action) TRUE )` to solve the problem, [more details see here](https://stackoverflow.com/questions/37776377/error-when-installing-an-r-package-from-github-could-not-find-build-tools-neces)
* Error: `xcrun: error: invalid active developer path (/Library/Developer/CommandLineTools), missing xcrun at:` **Solution** You need to install xcode which you do on Mac by opening the terminal and running `xcode-select --install
` to install the command lines tools package [more details on stackoverflow](https://apple.stackexchange.com/questions/254380/why-am-i-getting-an-invalid-active-developer-path-when-attempting-to-use-git-a)
    
# Authors
Jana B. Jarecki,  
Florian I. Seitz

# License
This project is licensed under CC-By Attribution 4.0 International
