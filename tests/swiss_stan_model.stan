// Stan Model

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
  
