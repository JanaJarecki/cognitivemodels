---
title: "Models"
date: "10/2022"
---

# Models implemented in the package
The following models for decision making under risk, categorization, preferential choices, and learning are implemented:

Model | Usage | Reference | Function
------------ | ------------- | -------------| -------------
Generalized context | Classification learning and inferences | (Medin & Schaffer, 1976; Nosofsky, 1986) | `?ebm`
Memory preference | Forecasting consumer preferences | (Jarecki & Rieskamp, 2022) | `?mem`
Shortfall | Decision making about risks | (Andraszewicz, 2014) | `?shortfall`
Optimal risk-sensitive foraging | Decision making about risk with goals| (Houston & McNamara, 1988) | `?hm1988`
Cumulative prospect | Decision making under risks | (Kahneman & Tversky, 1979; Tversky & Kahneman, 1992) | `?cpt`
Bayesian cognitive | Information learning with prior information | (Hoffart, Jarecki, Duthil, & Rieskamp, 2022) | `?bayes`
Power utility | Economic utility and preferences | (Tversky, 1967; Wakker, 2008)| `?utility`
Soft-max | Probabilistic action selection | (Sutton & Barto, 1998) | `?softmax`
Arg-max | Deterministic action selection | | `?argmax`
Epsilon-greedy | Constant-error action selection| | `?epsilon`
A baseline model | Random decisions | | `?baseline`