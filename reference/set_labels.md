# Relabel Fitted States or Trajectories

Renames the groups of an already-fitted object and propagates the new
labels through every derived table. Supply either a complete label
vector ordered from the first to the last group, or named renames such
as `c("State 1" = "Disengaged")` to change only some labels.

## Usage

``` r
set_labels(x, ...)

# S3 method for class 'vasstra_states'
set_labels(x, labels, ...)

# S3 method for class 'vasstra_trajectories'
set_labels(x, labels, ...)

# S3 method for class 'vasstra'
set_labels(x, states = NULL, trajectories = NULL, ...)
```

## Arguments

- x:

  A `vasstra_states`, `vasstra_trajectories`, or complete `vasstra`
  object.

- ...:

  Method-specific arguments.

- labels:

  New labels: a complete vector or named partial renames.

- states:

  New state labels for a complete fit (complete vector or named
  renames). The rename is propagated in place through the fitted states,
  sequences, trajectories, and description — including the recorded
  positive and negative states — without changing any fitted value.

- trajectories:

  New trajectory labels for a complete fit.

## Value

The relabeled object, with the same class as `x`.

## Examples

``` r
set.seed(1)
data <- expand.grid(student = 1:12, course = 1:3)
level <- rep(c(2, 8, 16), length.out = nrow(data))
data$views <- level + rnorm(nrow(data), sd = 0.4)
data$duration <- level * 3 + rnorm(nrow(data), sd = 0.4)
states <- step1_states(data, n_states = 3)
#> Detected id = "student", time = "course", variables = 2 numeric indicators.
states <- set_labels(states, c("Low", "Average", "High"))
summary(states)
#>     state variable       mean         sd  n
#> 1     Low    views -1.1057572 0.06169540 12
#> 2 Average    views -0.1201196 0.05889274 12
#> 3    High    views  1.2258768 0.06116813 12
#> 4     Low duration -1.1155620 0.02822082 12
#> 5 Average duration -0.1063358 0.01675835 12
#> 6    High duration  1.2218978 0.01448978 12
```
