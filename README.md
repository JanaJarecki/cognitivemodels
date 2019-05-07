# cogscimodels

R packages containing cognitive models.

## Getting started
This explains how to use the models. 

## Prerequisites
Working installation of R and the devtools and Rcpp packages (devtools version 2.0.2, `install.packages("devtools")`, Rcpp version 1.0.1 `install.packages("Rcpp")`).

## Installing
Install the cogscimodelss and the cogsciutils packages (the latter is needed for goodness of fit and choicerule functions)

    devtools::install_github("janajarecki/cogscimodels")
    devtools::install_github("janajarecki/cogsciutils")
    
Load the package

    library(cogscimodels)
    
## Usage
### 1. Memory-based preference model used in Jarecki & Rieskamp (submitted)
#### Usage
    jr19mem(choices ~ a1 + a2 | subj_values | prices, data = dt)
see `?jr19mem`
#### Example
    # Create data
    attributes <- expand.grid(a1=0:1,a2=0:1)[rep(1:4,2),]
    prices <- rep(c(0,.5,.5,1),2)
    subj_values <- rep(c(2,0.2,0.1,2), 2)
    choices <- rep(c(1,0,0,1),2)
    dt <- data.frame(choices, attributes, prices, subj_values)

    # Fit the model
    M <- jr19mem(choices ~ a1 + a2 | subj_values | prices, data = dt)
    
    # View results
    M
    M$predict() # predict data
    M$coef() # view coefficients
    
    #     lambda        tau 
    # 10.0000000  0.7982114
### 2. Generalized context model (Medin & Schaffer, 1976; Nosofsky, 1986)
#### Usage
    ebm(formula, data, ...) #ebm for examplar-based model
see `?ebm`
### 3. Optimal risk-sensitive foraging model (Houston & McNamara, 1988)
#### Usage
    hm1988(env, formula, data, choicerule, fixed = NULL)
see `?hm1988`
### 4. Cumulative prospect theory model (Kahneman & Tversky, 1979; Tversky & Kahneman, 1992)
#### Usage
    cpt(rp ~ x1 + x2 + px + I(1-px) | y1 + y2 + py + I(1-py), nopt = 2, nout = 2, ref = 0,
        choicerule = "softmax", data = dt)
see `?cpt`
    
# Authors
Jana B. Jarecki

# License
This project is licensed under CC-By Attribution 4.0 International
