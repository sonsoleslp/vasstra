# Evaluate a Fitted Clustering

Compares the fitted number of states or trajectories against the
alternative counts for the same clustering method and reports
per-cluster quality. The result contains one tidy candidate row per
compared count, with the fitted and recommended solutions marked, and
one tidy row per fitted cluster with its size and mean silhouette width.

## Usage

``` r
evaluate(x, ...)

# S3 method for class 'vasstra_states'
evaluate(x, n_states = NULL, ...)

# S3 method for class 'vasstra_trajectories'
evaluate(x, n_trajectories = NULL, ...)

# S3 method for class 'vasstra'
evaluate(x, ...)
```

## Arguments

- x:

  A `vasstra_states`, `vasstra_trajectories`, or complete `vasstra`
  object.

- ...:

  Method-specific arguments.

- n_states:

  Candidate state counts to compare. Defaults to the stored automatic
  comparison when one exists, otherwise 2 through 6.

- n_trajectories:

  Candidate trajectory counts to compare. Defaults to the stored
  automatic comparison when one exists, otherwise 2 through 6.

## Value

A `vasstra_evaluation` object (or a `vasstra_evaluations` pair for a
complete fit).
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the candidate comparison;
[`summary()`](https://rdrr.io/r/base/summary.html) returns the
per-cluster quality table.

## Examples

``` r
set.seed(1)
data <- expand.grid(student = 1:12, course = 1:3)
level <- rep(c(2, 8, 16), length.out = nrow(data))
data$views <- level + rnorm(nrow(data), sd = 0.4)
data$duration <- level * 3 + rnorm(nrow(data), sd = 0.4)
states <- step1_states(data, n_states = 3)
#> Detected id = "student", time = "course", variables = 2 numeric indicators.
evaluate(states)
#> VaSSTra clustering evaluation: states (lpa)
#>   Fitted: 3 states | silhouette 0.950
#>   Candidates:
#>  n_states method lpa_model silhouette      bic      aic classification_entropy
#>         2    lpa       EEI      0.776  146.676  135.592                  1.000
#>         3    lpa       EEI      0.950 -167.994 -183.830                  1.000
#>         4    lpa       EEI      0.820 -158.259 -178.844                  0.979
#>         5    lpa       EEI      0.604 -153.472 -178.808                  0.968
#>         6    lpa       EEI      0.457 -146.465 -176.552                  0.957
#>  min_size max_size size_ratio eligible  best fitted
#>        12       24          2     TRUE FALSE  FALSE
#>        12       12          1     TRUE  TRUE   TRUE
#>         1       12         12    FALSE FALSE  FALSE
#>         1       12         12    FALSE FALSE  FALSE
#>         1       11         11    FALSE FALSE  FALSE
#>   Fitted states:
#>    state  n proportion silhouette
#>  State 1 12      0.333      0.939
#>  State 2 12      0.333      0.950
#>  State 3 12      0.333      0.961
```
