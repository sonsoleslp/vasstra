# Getting started with VaSSTra

VaSSTra turns repeated multivariate measurements into states, state
sequences, and groups of similar trajectories. The whole analysis is one
call, and every part of it can also be run as an explicit step.

This guide uses the built-in `engagement` data: 142 students measured on
eight indicators at eight course positions, from the [VaSSTra
chapter](https://lamethods.org/book1/chapters/ch11-vasstra/ch11-vasstra.html)
of *Learning Analytics Methods and Tutorials*. It ships analysis-ready
with attached role metadata, so no column plumbing is needed.

``` r

library(VaSSTra)
data("engagement", package = "VaSSTra")
```

## One call

[`vasstra()`](https://sonsoles.me/vasstra/reference/vasstra.md) detects
the subject, time, and indicator roles from the attached role metadata
(or common column names), compares two through six states and
trajectories, and fits the recommended counts. Every automated decision
is reported as it is made and recorded inside the fit. Here three state
labels fix three states; omit them and the state count is selected too.

``` r

fit <- vasstra(engagement, state_labels = c("Disengaged", "Average", "Active"))
#> Using n_states = 3 to match the supplied labels.
#> Selected n_trajectories = 3 (hamming + pam, silhouette = 0.435); see `diagnostics$selection`.
fit
#> VaSSTra Analysis
#>   142 subjects | 8 times | 3 states | 3 trajectories
#>   hamming + pam | silhouette 0.435
```

Name any decision to take it over; automation only fills what you omit.
Labels are the shortest way to fix a count — three labels mean three
states — and a candidate vector such as `n_states = 2:4` compares
exactly those counts and fits the recommended one. Two more arguments
control how states display everywhere: `state_order` fixes the order
states appear in across every plot, and `state_colors` sets a state
palette that is stored on the fit and reused by every plot.

``` r

fit <- vasstra(
  engagement,
  state_labels = c("Disengaged", "Average", "Active"),
  trajectory_labels = c("Mostly active", "Mostly average", "Mostly disengaged"),
  dissimilarity = "lcs",
  cluster_method = "ward.D2",
  positive_states = "Active",
  negative_states = "Disengaged"
)
```

## Evaluate the clustering

[`evaluate()`](https://sonsoles.me/vasstra/reference/evaluate.md)
compares the fitted numbers of states and trajectories against the
alternative counts and reports per-cluster quality. Printing shows both
tables; plotting draws the selection curve, per-cluster silhouette
widths, and group sizes.

``` r

evaluation <- evaluate(fit)
```

``` r

plot(evaluation)
```

![](get-started_files/figure-html/evaluate-plot-1.png)

The same verb works on a single step, with any candidate range:

``` r

state_evaluation <- evaluate(fit$states, n_states = 2:5)
summary(state_evaluation)
#>        state   n proportion silhouette
#> 1 Disengaged 266  0.2341549  0.3245448
#> 2    Average 551  0.4850352  0.2287734
#> 3     Active 319  0.2808099  0.2607385
```

## Tidy fit indices

[`fit_indices()`](https://sonsoles.me/vasstra/reference/fit_indices.md)
reports the fit statistics of the selected clustering as one tidy row,
or one row per compared candidate with `compare = TRUE`. LPA models
report the complete information-criterion family together with entropy
and the extreme average posterior class probabilities; hard-clustering
methods report their own objectives instead, and columns that do not
apply are dropped.

``` r

fit_indices(fit)
#> VaSSTra fit indices: states (lpa)
#>  n_states method lpa_model log_likelihood n_parameters      aic      bic
#>         3    lpa       EEI       -9940.47           34 19948.93 20120.13
#>     sabic     caic      awe      clc      kic      icl entropy prob_min
#>  20012.14 20154.13 20461.33 20144.89 19985.93 20384.09   0.894    0.945
#>  prob_max silhouette min_size max_size size_ratio
#>     0.956       0.26      266      551       2.07
fit_indices(fit, compare = TRUE)
#> VaSSTra fit indices: states (lpa)
#>  n_states method lpa_model log_likelihood n_parameters      aic      bic
#>         2    lpa       EEI      -10751.56           25 21553.12 21679.00
#>         3    lpa       EEI       -9940.47           34 19948.93 20120.13
#>         4    lpa       EEI       -9529.86           43 19145.73 19362.25
#>         5    lpa       EEI       -9321.57           52 18747.13 19008.96
#>         6    lpa       EEI       -9085.82           61 18293.64 18600.79
#>     sabic     caic      awe      clc      kic      icl entropy prob_min
#>  21599.59 21704.00 21929.88 21655.20 21581.12 21831.08   0.903    0.973
#>  20012.14 20154.13 20461.33 20144.89 19985.93 20384.09   0.894    0.945
#>  19225.66 19405.25 19793.76 19397.81 19191.73 19700.33   0.893    0.928
#>  18843.80 19060.96 19530.80 19113.95 18802.13 19479.79   0.871    0.873
#>  18407.04 18661.79 19212.95 18633.20 18357.64 19062.35   0.887    0.876
#>  prob_max silhouette min_size max_size size_ratio eligible  best fitted
#>     0.975      0.351      509      627       1.23     TRUE FALSE  FALSE
#>     0.956      0.260      266      551       2.07     TRUE FALSE   TRUE
#>     0.969      0.203      152      414       2.72     TRUE FALSE  FALSE
#>     0.964      0.197      155      367       2.37     TRUE  TRUE  FALSE
#>     0.973      0.181       33      350      10.61    FALSE FALSE  FALSE
fit_indices(fit, step = "trajectories", compare = TRUE)
#> VaSSTra fit indices: trajectories (hamming + pam)
#>  n_trajectories dissimilarity method silhouette mean_within_distance min_size
#>               2       hamming    pam      0.350                 4.03       47
#>               3       hamming    pam      0.435                 3.02       32
#>               4       hamming    pam      0.376                 2.83       15
#>               5       hamming    pam      0.347                 2.67       15
#>               6       hamming    pam      0.335                 2.49       12
#>  max_size size_ratio eligible  best fitted
#>        95       2.02     TRUE FALSE  FALSE
#>        68       2.12     TRUE  TRUE   TRUE
#>        59       3.93     TRUE FALSE  FALSE
#>        54       3.60     TRUE FALSE  FALSE
#>        40       3.33     TRUE FALSE  FALSE
```

## Name groups after inspecting them

[`set_labels()`](https://sonsoles.me/vasstra/reference/set_labels.md)
renames fitted states or trajectories in place — nothing is refitted, no
value changes — and the new names flow through every table, sequence,
and plot. Rename everything with a full vector, or only some groups by
name.

``` r

fit <- set_labels(
  fit,
  trajectories = c("Mostly active", "Mostly average", "Mostly disengaged")
)
summary(fit)
#>          trajectory  n proportion mean_within_distance n_observed unique_states
#> 1     Mostly active 42  0.2957746             2.874564          8      1.880952
#> 2    Mostly average 68  0.4788732             3.115013          8      1.985294
#> 3 Mostly disengaged 32  0.2253521             3.002016          8      1.843750
#>   transitions   entropy complexity volatility integrative_potential
#> 1    1.928571 0.4240368  0.3348902  0.4512472                   NaN
#> 2    2.220588 0.4625493  0.3735486  0.4894958                   NaN
#> 3    2.062500 0.4260927  0.3491121  0.4546131                   NaN
#>   negative_exposure
#> 1               NaN
#> 2               NaN
#> 3               NaN
```

## The same analysis in four explicit steps

Each step accepts the result of the previous step and also runs alone
with the same automated defaults. Use the steps when intermediate
results are needed, or when states come from another method.

``` r

states <- step1_states(engagement, n_states = 3,
                       labels = c("Disengaged", "Average", "Active"))
sequences <- step2_sequences(states)
trajectories <- step3_trajectories(sequences, n_trajectories = 3)
description <- step4_describe(
  trajectories,
  positive_states = "Active",
  negative_states = "Disengaged"
)
summary(description)
#>     trajectory  n proportion mean_within_distance n_observed unique_states
#> 1 Trajectory 1 42  0.2957746             2.874564          8      1.880952
#> 2 Trajectory 2 68  0.4788732             3.115013          8      1.985294
#> 3 Trajectory 3 32  0.2253521             3.002016          8      1.843750
#>   transitions   entropy complexity volatility integrative_potential
#> 1    1.928571 0.4240368  0.3348902  0.4512472             0.7638889
#> 2    2.220588 0.4625493  0.3735486  0.4894958             0.1156046
#> 3    2.062500 0.4260927  0.3491121  0.4546131             0.0000000
#>   negative_exposure
#> 1       0.009259259
#> 2       0.118464052
#> 3       0.766493056
```

Existing states from LPA, LCA, or any other source enter at step 2: pass
a data frame whose state column is detected automatically (or named with
the `state` argument).

## State plots

State results support profile, bar, heatmap, and size plots without
adding a plotting dependency, plus a combined overview.

``` r

plot(states, type = "all")
```

![](get-started_files/figure-html/state-plots-1.png)

``` r

plot(states, type = "profile")
```

![](get-started_files/figure-html/state-profile-1.png)

## Sequence and trajectory plots

Every plot containing state sequences is rendered by Nestimate:

``` r

plot(trajectories, type = "index")
```

![](get-started_files/figure-html/sequence-plot-1.png)

``` r

plot(trajectories, type = "distribution")
```

![](get-started_files/figure-html/sequence-plot-2.png)

## Compare candidates in full detail

When automation should be replaced by an explicit comparison,
[`state_choices()`](https://sonsoles.me/vasstra/reference/state_choices.md)
and
[`trajectory_choices()`](https://sonsoles.me/vasstra/reference/trajectory_choices.md)
fit a grid across methods and counts and return one tidy row per
candidate. Nothing is selected silently; pass the inspected
`candidate_id` to fit.

``` r

state_options <- state_choices(
  engagement,
  n_states = 2:4,
  method = c("kmeans", "pam", "ward.D2")
)
state_options
#> VaSSTra state choices
#>   9 candidates | 9 successful | 3 recommended
#>  candidate_id n_states  method lpa_model recommendation_criterion silhouette
#>             1        2  kmeans      <NA>               silhouette  0.3519794
#>             4        2     pam      <NA>               silhouette  0.3497139
#>             7        2 ward.D2      <NA>               silhouette  0.3312179
#>  bic min_size eligible
#>   NA      519     TRUE
#>   NA      543     TRUE
#>   NA      483     TRUE
plot(state_options, metric = "silhouette")
```

![](get-started_files/figure-html/state-choices-1.png)

``` r

chosen_states <- fit_state_choice(
  state_options,
  n_states = 3,
  method = "kmeans",
  labels = c("Disengaged", "Average", "Active")
)
```

`fit_state_choice(state_options)` with no selection fits the recommended
candidate; `candidate_id` remains available for exact row selection.

Average silhouette is the common diagnostic across all methods; larger
values indicate better-separated groups. Recommendations identify the
best eligible candidate separately for each method rather than declaring
one algorithm universally best. LPA candidates are recommended by
conventional BIC, with AIC, silhouette, or native mclust ICL available
through `lpa_criterion`.

``` r

trajectory_options <- trajectory_choices(
  step2_sequences(chosen_states),
  n_trajectories = 2:4,
  dissimilarity = c("hamming", "lcs"),
  method = c("pam", "ward.D2")
)
trajectory_options
#> VaSSTra trajectory choices (Nestimate)
#>   12 candidates | 4 recommended distance-method solutions
#>  candidate_id n_trajectories dissimilarity  method silhouette min_size eligible
#>             2              3       hamming     pam  0.4376591       35     TRUE
#>             5              3           lcs     pam  0.5312267       27     TRUE
#>             8              3       hamming ward.D2  0.3926622       33     TRUE
#>            11              3           lcs ward.D2  0.4733234       40     TRUE
plot(trajectory_options, metric = "silhouette")
```

![](get-started_files/figure-html/trajectory-choices-1.png)

Recommendations are made within each distance-method pair because mean
within-trajectory distances are only comparable within one
dissimilarity.

States default to mclust latent profiles (`"EEI"`, tidyLPA model 1);
base R supplies k-means and hierarchical clustering, `cluster` supplies
PAM, and Nestimate supplies sequence distances, trajectory clustering,
and plots. No TraMineR plotting path or dependency is required.
