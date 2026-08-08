# Step 1: Turn Variables into States

Standardizes numeric indicators and clusters each subject-time
observation. The default method is Gaussian-mixture latent profile
analysis (LPA); reproducible k-means, PAM, and hierarchical clustering
are available through `method`. Clusters are ordered by their average
standardized indicator level, then optionally given user-supplied
labels.

## Usage

``` r
step1_states(
  data,
  id = NULL,
  time = NULL,
  variables = NULL,
  n_states = "auto",
  labels = NULL,
  state = "state",
  standardize = NULL,
  missing = NULL,
  time_levels = NULL,
  n_start = 25L,
  seed = 123L,
  method = c("lpa", "kmeans", "pam", "ward.D2", "ward.D", "complete", "average",
    "single", "mcquitty", "median", "centroid"),
  lpa_model = "EEI"
)
```

## Arguments

- data:

  A data frame with one row per subject and time point.

- id:

  Name of the subject identifier column. May be omitted when the data
  carry VaSStra role metadata.

- time:

  Name of the time or ordering column. May be omitted when the data
  carry VaSStra role metadata.

- variables:

  Character vector naming numeric state indicators. May be omitted when
  the data carry VaSStra role metadata.

- n_states:

  Number of states to estimate. One number fits exactly that count,
  several numbers (for example `2:4`) compare those candidates with
  [`state_choices()`](https://pak.dynasite.org/VaSStra/reference/state_choices.md)
  and fit the recommended count, and `"auto"` (default) compares 2
  through 6 — or simply matches `labels` when labels are supplied.
  Automatic comparison never selects a solution whose smallest state
  holds under 5 percent of the observations. Compared candidates are
  kept in `diagnostics$selection` and every automated choice is reported
  with a message.

- labels:

  Optional unique labels, ordered from the lowest to the highest average
  standardized profile.

- state:

  Name of the state column created in the returned data.

- standardize:

  One of `"time"` (default for ordinary data), `"global"`, or `"none"`.
  Analysis-ready package data may provide its own default through
  metadata.

- missing:

  One of `"error"` (default for ordinary data) or `"median"`.
  Analysis-ready package data may provide its own default through
  metadata. Median imputation is performed within time point, with the
  global variable median as a fallback for an entirely missing time
  point.

- time_levels:

  Explicit chronological time values. Required for character or
  unordered-factor time columns.

- n_start:

  Number of random k-means starts.

- seed:

  Reproducible random seed.

- method:

  State-clustering method. Choose `"lpa"` (default, Gaussian-mixture
  latent profile analysis via mclust), `"kmeans"`, `"pam"`, or a
  hierarchical method: `"ward.D2"`, `"ward.D"`, `"complete"`,
  `"average"`, `"single"`, `"mcquitty"`, `"median"`, or `"centroid"`.

- lpa_model:

  Covariance model used only by `method = "lpa"`. The default `"EEI"`
  corresponds to tidyLPA model 1: equal variances and zero covariances.

## Value

A `vasstra_states` object. Its `data` element is the original data with
a factor state column; `profiles` is a tidy state-by-indicator table.

## Examples

``` r
set.seed(1)
data <- expand.grid(student = 1:12, course = 1:3)
level <- rep(c(2, 8, 16), length.out = nrow(data))
data$views <- level + rnorm(nrow(data), sd = 0.4)
data$duration <- level * 3 + rnorm(nrow(data), sd = 0.4)
states <- step1_states(
  data,
  id = "student",
  time = "course",
  variables = c("views", "duration"),
  labels = c("Low", "Average", "High")
)
#> Using n_states = 3 to match the supplied labels.
states
#> VaSStra Step 1: Variables -> States
#>   36 rows | 12 subjects | 3 times | 3 states | lpa
#>   Average silhouette: 0.950
#>   State sizes: Low=12, Average=12, High=12
```
