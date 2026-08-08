# Run the Complete VaSStra Workflow

A one-call wrapper around
[`step1_states()`](https://pak.dynasite.org/VaSStra/reference/step1_states.md),
[`step2_sequences()`](https://pak.dynasite.org/VaSStra/reference/step2_sequences.md),
[`step3_trajectories()`](https://pak.dynasite.org/VaSStra/reference/step3_trajectories.md),
and
[`step4_describe()`](https://pak.dynasite.org/VaSStra/reference/step4_describe.md).
Every argument has a working default, so `vasstra(data)` runs a complete
analysis: subject, time, and indicator roles come from explicit
arguments, attached role metadata, or common column names, and the
numbers of states and trajectories are selected automatically unless
given. Every automated decision is reported with a message and recorded
in the fit.

## Usage

``` r
vasstra(
  data,
  id = NULL,
  time = NULL,
  variables = NULL,
  state = NULL,
  n_states = "auto",
  n_trajectories = "auto",
  state_labels = NULL,
  trajectory_labels = NULL,
  state_name = "state",
  standardize = NULL,
  indicator_missing = NULL,
  sequence_missing = c("error", "explicit", "keep"),
  missing_label = "Missing",
  time_levels = NULL,
  dissimilarity = c("hamming", "osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard",
    "jw"),
  cluster_method = c("pam", "ward.D2", "ward.D", "complete", "average", "single",
    "mcquitty", "median", "centroid"),
  backend = c("Nestimate", "base"),
  positive_states = NULL,
  negative_states = NULL,
  omega = 1,
  n_start = 25L,
  seed = 123L,
  state_method = c("lpa", "kmeans", "pam", "ward.D2", "ward.D", "complete", "average",
    "single", "mcquitty", "median", "centroid"),
  lpa_model = "EEI"
)
```

## Arguments

- data:

  A longitudinal data frame.

- id:

  Subject identifier column. May be omitted when the data carry VaSStra
  role metadata or use a common identifier name.

- time:

  Time or ordering column. May be omitted when the data carry VaSStra
  role metadata or use a common time name.

- variables:

  Numeric state indicators. Use either `variables` or `state`, but not
  both. When both are omitted, the numeric non-role columns are used
  (columns ending in `_z` are preferred when present).

- state:

  Existing state column. Use either `state` or `variables`.

- n_states:

  Number of states when `variables` is supplied: one number, a candidate
  vector such as `2:4` to compare and fit the recommended count, or
  `"auto"` (default). Supplied `state_labels` determine the count under
  `"auto"`.

- n_trajectories:

  Number of trajectory groups: one number, a candidate vector to
  compare, or `"auto"` (default). Supplied `trajectory_labels` determine
  the count under `"auto"`.

- state_labels:

  Optional labels ordered from low to high profile.

- trajectory_labels:

  Optional labels for stable trajectory groups.

- state_name:

  Output column name when states are estimated.

- standardize:

  State-indicator standardization: `"time"`, `"global"`, or `"none"`.
  Analysis-ready package data may provide its own default.

- indicator_missing:

  Indicator missingness policy passed to step 1. Analysis-ready package
  data may provide its own default.

- sequence_missing:

  Structural sequence-gap policy passed to step 2.

- missing_label:

  Explicit missing-state label.

- time_levels:

  Explicit chronological values when needed.

- dissimilarity:

  Sequence distance passed to
  [`step3_trajectories()`](https://pak.dynasite.org/VaSStra/reference/step3_trajectories.md).

- cluster_method:

  Trajectory clustering method.

- backend:

  Clustering backend: `"Nestimate"` (default) or `"base"`.

- positive_states:

  Optional states considered positive.

- negative_states:

  Optional states considered negative.

- omega:

  Later-time weighting exponent for step 4.

- n_start:

  Number of k-means starts.

- seed:

  Reproducible base seed.

- state_method:

  State-clustering method passed to
  [`step1_states()`](https://pak.dynasite.org/VaSStra/reference/step1_states.md).
  The default `"lpa"` estimates Gaussian-mixture latent profiles.

- lpa_model:

  mclust covariance model used when `state_method = "lpa"`. The default
  `"EEI"` is tidyLPA model 1.

## Value

A `vasstra` object containing all four fitted step objects.

## Examples

``` r
set.seed(1)
data <- expand.grid(student = 1:15, course = 1:4)
latent <- ceiling(data$student / 5)
data$views <- latent * 5 + rnorm(nrow(data), sd = 0.4)
data$duration <- latent * 10 + rnorm(nrow(data), sd = 0.4)

# Fully automated: roles are detected and the counts are selected.
fit <- vasstra(data)
#> Detected id = "student", time = "course", variables = 2 numeric indicators.
#> Selected n_states = 3 (lpa, bic = -184.086); see `diagnostics$selection`.
#> Selected n_trajectories = 3 (hamming + pam, silhouette = 1.000); see `diagnostics$selection`.
fit
#> VaSStra Analysis
#>   15 subjects | 4 times | 3 states | 3 trajectories
#>   hamming + pam | silhouette 1.000

# Labels imply the counts; other choices named explicitly.
fit <- vasstra(
  data,
  state_labels = c("Low", "Average", "High"),
  positive_states = "High",
  negative_states = "Low"
)
#> Detected id = "student", time = "course", variables = 2 numeric indicators.
#> Using n_states = 3 to match the supplied labels.
#> Selected n_trajectories = 3 (hamming + pam, silhouette = 1.000); see `diagnostics$selection`.
fit
#> VaSStra Analysis
#>   15 subjects | 4 times | 3 states | 3 trajectories
#>   hamming + pam | silhouette 1.000
```
