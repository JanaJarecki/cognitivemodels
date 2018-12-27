Sys.setenv(USE_CXX14 = 'g++ -std=c++1y')
library(rstan)
library(data.table)
data(swiss)
swiss <- as.data.table(swiss)
summary(swiss)
library(ggplot2)
library(themejj)
theme_set(themejj())
# Define the x and y variables separately
x <- swiss$Education
y <- swiss$Fertility
N <- nrow(swiss)

# Make a datalist for stan
swiss_stan <- list(N = N, x = x, y = y)

# Write the stan model here in R
write('// Stan Model

  data {
    int < lower = 1 > N; // participants
    vector[N] x; // the x var
    vector[N] y; // the y var
  }

  parameters {
    real alpha; // b0
    real beta; //
    real < lower = 0> sigma;
  }

  model {
    y ~ normal(alpha + beta * x , sigma);
  }

  generated quantities {
  } // just toe posterior distribution
  ',
  'swiss_stan_model.stan'
  )


# Check the model
stanc("swiss_stan_model.stan")

stan_model1 <- "swiss_stan_model.stan" # save the filepath

# Run the model
fit <- stan(file = stan_model1, data = swiss_stan, warmup = 500, iter = 1000, chains = 4, cores = 4, thin = 1)

# get the posterior
posterior <- extract(fit)
str(posterior)

# Plot all the traces with ggplot and my theme
stan_trace(fit) +themejj()

# Parameter summary
stan_dens(fit, fill = 'grey70', alpha = .1) +themejj()
