# Tidy Fit Indices for a Fitted Clustering

Returns the fit statistics of the selected clustering as one tidy row,
or one row per compared candidate with `compare = TRUE`. LPA solutions
report the complete information-criterion family (log-likelihood, AIC,
BIC, SABIC, CAIC, AWE, CLC, KIC, ICL — all on the conventional
lower-is-better scale), normalized entropy, and the smallest and largest
average posterior class probabilities. Hard clustering methods report
their own objectives (total within-cluster sum of squares, PAM
objective). Silhouette and group sizes are always included, and columns
that do not apply to the fitted method are dropped.

## Usage

``` r
fit_indices(x, ...)

# S3 method for class 'vasstra_states'
fit_indices(x, compare = FALSE, n_states = NULL, ...)

# S3 method for class 'vasstra_trajectories'
fit_indices(x, compare = FALSE, n_trajectories = NULL, ...)

# S3 method for class 'vasstra'
fit_indices(x, step = c("states", "trajectories"), compare = FALSE, ...)
```

## Arguments

- x:

  A `vasstra_states`, `vasstra_trajectories`, or complete `vasstra`
  object.

- ...:

  Method-specific arguments.

- compare:

  Return one row per compared candidate instead of only the fitted
  solution.

- n_states:

  Candidate state counts used when `compare = TRUE`. Defaults to the
  stored automatic comparison when one exists, otherwise 2 through 6.

- n_trajectories:

  Candidate trajectory counts used when `compare = TRUE`. Defaults to
  the stored automatic comparison when one exists, otherwise 2 through
  6.

- step:

  Which clustering of a complete fit to summarize: `"states"` (default)
  or `"trajectories"`.

## Value

A `vasstra_fit_indices` data frame: one row for the fitted solution, or
one row per candidate (with `best` and `fitted` markers) when
`compare = TRUE`.

## Examples

``` r
set.seed(1)
data <- expand.grid(student = 1:12, course = 1:3)
level <- rep(c(2, 8, 16), length.out = nrow(data))
data$views <- level + rnorm(nrow(data), sd = 0.4)
data$duration <- level * 3 + rnorm(nrow(data), sd = 0.4)
states <- step1_states(data, n_states = 3)
#> Detected id = "student", time = "course", variables = 2 numeric indicators.
fit_indices(states)
#> VaSStra fit indices: states (lpa)
#>  n_states method lpa_model log_likelihood n_parameters     aic     bic   sabic
#>         3    lpa       EEI         101.91           10 -183.83 -167.99 -199.23
#>     caic     awe     clc     kic     icl entropy prob_min prob_max silhouette
#>  -157.99 -102.16 -203.83 -170.83 -167.99       1        1        1       0.95
#>  min_size max_size size_ratio
#>        12       12          1
fit_indices(states, compare = TRUE)
#> VaSStra fit indices: states (lpa)
#>  n_states method lpa_model log_likelihood n_parameters     aic     bic   sabic
#>         2    lpa       EEI         -60.80            7  135.59  146.68  124.81
#>         3    lpa       EEI         101.91           10 -183.83 -167.99 -199.23
#>         4    lpa       EEI         102.42           13 -178.84 -158.26 -198.87
#>         5    lpa       EEI         105.40           16 -178.81 -153.47 -203.46
#>         6    lpa       EEI         107.28           19 -176.55 -146.46 -205.82
#>     caic     awe     clc     kic     icl entropy prob_min prob_max silhouette
#>   153.68  192.76  121.61  145.59  146.69   1.000    1.000    1.000      0.776
#>  -157.99 -102.16 -203.83 -170.83 -167.99   1.000    1.000    1.000      0.950
#>  -145.26  -72.67 -202.73 -162.84 -156.14   0.979    0.858    1.000      0.820
#>  -137.47  -48.14 -207.06 -159.81 -149.72   0.968    0.810    1.000      0.604
#>  -127.46  -21.38 -208.96 -154.55 -140.87   0.957    0.851    0.996      0.457
#>  min_size max_size size_ratio eligible  best fitted
#>        12       24          2     TRUE FALSE  FALSE
#>        12       12          1     TRUE  TRUE   TRUE
#>         1       12         12    FALSE FALSE  FALSE
#>         1       12         12    FALSE FALSE  FALSE
#>         1       11         11    FALSE FALSE  FALSE
```
