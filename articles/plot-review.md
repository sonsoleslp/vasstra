# Plotting the results

This review exercises every VaSSTra plot on the longitudinal engagement
data from [Chapter 11 of *Learning Analytics Methods and
Tutorials*](https://lamethods.org/book1/chapters/ch11-vasstra/ch11-vasstra.html):
the state, sequence, and trajectory views (base graphics, no `ggplot2`
dependency), the combined per-trajectory grid, the
[`evaluate()`](https://sonsoles.me/vasstra/reference/evaluate.md)
panels, and the `cograph`-rendered flow and transition networks. Every
view shares one state palette and one state order, both of which can be
pinned on the fit with `state_colors` and `state_order`.

## The whole analysis in one call

`vasstra(engagement)` needs nothing else. Roles come from the attached
metadata, and the state and trajectory counts are selected by comparing
two through six candidates. Here BIC favours five states, so the fully
automatic fit reports five; the chapter analysis below fixes three for
interpretability. Each decision is reported as a message and recorded in
the fit.

``` r

library(VaSSTra)
data("engagement", package = "VaSSTra")

fit_auto <- vasstra(engagement)
#> Selected n_states = 5 (lpa, bic = 19008.964); see `diagnostics$selection`.
#> Registered S3 method overwritten by 'Nestimate':
#>   method     from   
#>   print.mcml cograph
#> Selected n_trajectories = 3 (hamming + pam, silhouette = 0.222); see `diagnostics$selection`.
fit_auto
#> VaSSTra Analysis
#>   142 subjects | 8 times | 5 states | 3 trajectories
#>   hamming + pam | silhouette 0.222
```

## Evaluate the automatic clustering

[`evaluate()`](https://sonsoles.me/vasstra/reference/evaluate.md)
returns the candidate comparison with `best` and `fitted` markers plus
per-cluster silhouette quality; its plot shows the selection curve, the
per-group silhouette widths, and the group sizes for both clustering
steps. The state and trajectory rows use distinct palettes, so the two
groupings are not read as corresponding.

``` r

evaluation <- evaluate(fit_auto)
evaluation
#> VaSSTra clustering evaluation: states (lpa)
#>   Fitted: 5 states | silhouette 0.197
#>   Candidates:
#>  n_states method lpa_model silhouette      bic      aic classification_entropy
#>         2    lpa       EEI      0.351 21679.00 21553.12                  0.903
#>         3    lpa       EEI      0.260 20120.13 19948.93                  0.894
#>         4    lpa       EEI      0.203 19362.24 19145.73                  0.893
#>         5    lpa       EEI      0.197 19008.96 18747.13                  0.871
#>         6    lpa       EEI      0.181 18600.79 18293.64                  0.887
#>  min_size max_size size_ratio eligible  best fitted
#>       509      627      1.232     TRUE FALSE  FALSE
#>       266      551      2.071     TRUE FALSE  FALSE
#>       152      414      2.724     TRUE FALSE  FALSE
#>       155      367      2.368     TRUE  TRUE   TRUE
#>        33      350     10.606    FALSE FALSE  FALSE
#>   Fitted states:
#>    state   n proportion silhouette
#>  State 1 155      0.136      0.300
#>  State 2 367      0.323      0.172
#>  State 3 230      0.202      0.219
#>  State 4 228      0.201      0.164
#>  State 5 156      0.137      0.167
#> 
#> VaSSTra clustering evaluation: trajectories (hamming + pam)
#>   Fitted: 3 trajectories | silhouette 0.222
#>   Candidates:
#>  n_trajectories dissimilarity method silhouette mean_within_distance min_size
#>               2       hamming    pam      0.221                5.491       49
#>               3       hamming    pam      0.222                5.138       24
#>               4       hamming    pam      0.199                4.817       18
#>               5       hamming    pam      0.210                4.490       17
#>               6       hamming    pam      0.202                4.345       16
#>  max_size size_ratio eligible  best fitted
#>        93      1.898     TRUE FALSE  FALSE
#>        85      3.542     TRUE  TRUE   TRUE
#>        68      3.778     TRUE FALSE  FALSE
#>        53      3.118     TRUE FALSE  FALSE
#>        43      2.688     TRUE FALSE  FALSE
#>   Fitted trajectories:
#>    trajectory  n proportion mean_within_distance silhouette
#>  Trajectory 1 85      0.599                5.353      0.219
#>  Trajectory 2 33      0.232                4.830      0.213
#>  Trajectory 3 24      0.169                4.801      0.246
plot(evaluation)
```

![](plot-review_files/figure-html/evaluate-auto-1.png)

## The chapter analysis, explicit where it matters

The chapter’s substantive choices stay in one readable call. The state
method defaults to LPA with the chapter’s `"EEI"` model, and the three
labels fix the three profiles used from here on; automation only fills
what is omitted.

``` r

fit <- vasstra(
  engagement,
  state_labels = c("Disengaged", "Average", "Active"),
  dissimilarity = "lcs",
  cluster_method = "ward.D2",
  trajectory_labels = c(
    "Mostly active",
    "Mostly average",
    "Mostly disengaged"
  ),
  positive_states = "Active",
  negative_states = "Disengaged",
  seed = 22294
)
#> Using n_states = 3 to match the supplied labels.
#> Using n_trajectories = 3 to match the supplied labels.
fit
#> VaSSTra Analysis
#>   142 subjects | 8 times | 3 states | 3 trajectories
#>   lcs + ward.D2 | silhouette 0.514
```

``` r

plot(evaluate(fit))
```

![](plot-review_files/figure-html/evaluate-explicit-1.png)

## Tidy fit indices

The fitted LPA solution as one tidy row with the complete
information-criterion family, then the comparison against all candidate
counts, then the trajectory clustering.

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
#> VaSSTra fit indices: trajectories (lcs + ward.D2)
#>  n_trajectories dissimilarity  method silhouette mean_within_distance min_size
#>               2           lcs ward.D2      0.440                 6.35       41
#>               3           lcs ward.D2      0.514                 4.54       28
#>               4           lcs ward.D2      0.400                 4.17       28
#>               5           lcs ward.D2      0.370                 3.92        8
#>               6           lcs ward.D2      0.350                 3.59        8
#>  max_size size_ratio eligible  best fitted
#>       101       2.46     TRUE FALSE  FALSE
#>        73       2.61     TRUE  TRUE   TRUE
#>        43       1.54     TRUE FALSE  FALSE
#>        43       5.38     TRUE FALSE  FALSE
#>        43       5.38     TRUE FALSE  FALSE
```

## Relabel after inspection

[`set_labels()`](https://sonsoles.me/vasstra/reference/set_labels.md)
renames groups in place — full vectors or partial named renames — and
every downstream table follows.

``` r

fit_renamed <- set_labels(
  fit,
  states = c("Average" = "Moderate"),
  trajectories = c("Thriving", "Coasting", "Struggling")
)
as.data.frame(fit_renamed, unit = "trajectory")
#>   trajectory  n proportion mean_within_distance n_observed unique_states
#> 1   Thriving 41  0.2887324             4.258537          8      1.878049
#> 2   Coasting 73  0.5140845             4.961187          8      1.986301
#> 3 Struggling 28  0.1971831             3.857143          8      1.821429
#>   transitions   entropy complexity volatility integrative_potential
#> 1    1.829268 0.4189906  0.3251220  0.4436702             0.7696477
#> 2    2.328767 0.4736884  0.3865300  0.4973907             0.1149163
#> 3    1.892857 0.3978570  0.3246998  0.4387755             0.0000000
#>   negative_exposure
#> 1       0.009485095
#> 2       0.131659056
#> 3       0.820436508
```

## Tidy outputs

``` r

head(as.data.frame(fit, unit = "subject"))
#>    user_id        trajectory n_observed unique_states transitions   entropy
#> 1 D2C5F64E     Mostly active          8             3           3 0.9850568
#> 2 D7E3D0DC     Mostly active          8             3           4 0.8194484
#> 3 2926CF64 Mostly disengaged          8             1           0 0.0000000
#> 4 985AFF35 Mostly disengaged          8             2           2 0.6021808
#> 5 F89100C7     Mostly active          8             3           4 0.8868595
#> 6 AB944042 Mostly disengaged          8             2           2 0.3429510
#>   complexity volatility integrative_potential negative_exposure
#> 1  0.6497440  0.7142857             0.4166667        0.08333333
#> 2  0.6842925  0.7857143             0.5833333        0.08333333
#> 3  0.0000000  0.1666667             0.0000000        1.00000000
#> 4  0.4147911  0.4761905             0.0000000        0.55555556
#> 5  0.7118826  0.7857143             0.3888889        0.19444444
#> 6  0.3130271  0.4761905             0.0000000        0.88888889
head(as.data.frame(fit, unit = "observation"))
#>    user_id sequence_position      state position    trajectory
#> 1 D2C5F64E                 1 Disengaged        1 Mostly active
#> 2 D2C5F64E                 2 Disengaged        2 Mostly active
#> 3 D2C5F64E                 3    Average        3 Mostly active
#> 4 D2C5F64E                 4     Active        4 Mostly active
#> 5 D2C5F64E                 5     Active        5 Mostly active
#> 6 D2C5F64E                 6     Active        6 Mostly active
head(as.data.frame(fit, unit = "state_profile"))
#>        state              variable        mean        sd   n
#> 1 Disengaged   course_view_count_z -1.13211349 0.5755714 266
#> 2    Average   course_view_count_z -0.07877558 0.5387538 551
#> 3     Active   course_view_count_z  1.07947227 0.6503939 319
#> 4 Disengaged forum_consume_count_z -1.08450800 0.6669212 266
#> 5    Average forum_consume_count_z -0.01863047 0.6225600 551
#> 6     Active forum_consume_count_z  0.93670820 0.7397460 319
as.data.frame(fit, unit = "trajectory")
#>          trajectory  n proportion mean_within_distance n_observed unique_states
#> 1     Mostly active 41  0.2887324             4.258537          8      1.878049
#> 2    Mostly average 73  0.5140845             4.961187          8      1.986301
#> 3 Mostly disengaged 28  0.1971831             3.857143          8      1.821429
#>   transitions   entropy complexity volatility integrative_potential
#> 1    1.829268 0.4189906  0.3251220  0.4436702             0.7696477
#> 2    2.328767 0.4736884  0.3865300  0.4973907             0.1149163
#> 3    1.892857 0.3978570  0.3246998  0.4387755             0.0000000
#>   negative_exposure
#> 1       0.009485095
#> 2       0.131659056
#> 3       0.820436508
```

## State overview: profile + bars + heatmap + sizes

`type = "all"` draws the four state views together on the shared
palette; the profile heatmap reads with the first state as its top row,
matching the bars, sizes, and legends.

``` r

plot(fit, which = "states", type = "all")
```

![](plot-review_files/figure-html/state-all-1.png)

Each view is also available alone:

``` r

plot(fit, which = "states", type = "profile")
```

![](plot-review_files/figure-html/state-profile-1.png)

``` r

plot(fit, which = "states", type = "bars")
```

![](plot-review_files/figure-html/state-bars-1.png)

``` r

plot(fit, which = "states", type = "heatmap")
```

![](plot-review_files/figure-html/state-heatmap-1.png)

``` r

plot(fit, which = "states", type = "sizes")
```

![](plot-review_files/figure-html/state-sizes-1.png)

## Palette and state order

`colors` recolours a single plot; the states are coloured by position,
so supply one colour per state in state order.

``` r

plot(fit, which = "states", type = "sizes",
     colors = c("#D55E00", "#0072B2", "#009E73"))
```

![](plot-review_files/figure-html/palette-oneoff-1.png)

To reuse a palette everywhere without repeating `colors`, pin it on the
fit with `state_colors` (a named vector is matched by state and survives
reordering and
[`set_labels()`](https://sonsoles.me/vasstra/reference/set_labels.md)).
`state_order` fixes the order the states read in every plot — stacking,
legends, transition and flow nodes — independent of the order the
clusters were discovered in.

``` r

fit_styled <- vasstra(
  engagement,
  state_labels = c("Disengaged", "Average", "Active"),
  state_order  = c("Active", "Average", "Disengaged"),
  state_colors = c(Disengaged = "#D55E00", Average = "#0072B2", Active = "#009E73"),
  dissimilarity = "lcs",
  cluster_method = "ward.D2"
)
#> Using n_states = 3 to match the supplied labels.
#> Selected n_trajectories = 3 (lcs + ward.D2, silhouette = 0.514); see `diagnostics$selection`.
plot(fit_styled, type = "distribution")
```

![](plot-review_files/figure-html/palette-stored-1.png)

## Choice comparisons across methods

The explicit comparison draws a metric-aware plot, on the shared palette
with light gridlines.

``` r

state_options <- state_choices(
  engagement,
  n_states = 2:5,
  method = c("kmeans", "pam", "ward.D2", "lpa"),
  lpa_model = "EEI",
  seed = 22294
)
state_options
#> VaSSTra state choices
#>   16 candidates | 16 successful | 4 recommended
#>  candidate_id n_states  method lpa_model recommendation_criterion silhouette
#>             1        2  kmeans      <NA>               silhouette  0.3519794
#>             5        2     pam      <NA>               silhouette  0.3497139
#>             9        2 ward.D2      <NA>               silhouette  0.3312179
#>            16        5     lpa       EEI                      bic  0.1966979
#>       bic min_size eligible
#>        NA      519     TRUE
#>        NA      543     TRUE
#>        NA      483     TRUE
#>  19008.96      155     TRUE
plot(state_options, metric = "silhouette")
```

![](plot-review_files/figure-html/state-choices-1.png)

``` r

plot(state_options, metric = "bic", groups = "lpa/EEI")
```

![](plot-review_files/figure-html/state-choices-2.png)

## All sequences: heatmap (default), distribution, index

Every plot containing state sequences is rendered by
[`Nestimate::sequence_plot()`](https://saqr.me/Nestimate/reference/sequence_plot.html).
The heatmap is the default view for the complete sequence set.

``` r

plot(fit, which = "sequences")
```

![](plot-review_files/figure-html/sequences-heatmap-1.png)

``` r

plot(fit, which = "sequences", type = "distribution")
```

![](plot-review_files/figure-html/sequences-distribution-1.png)

``` r

plot(fit, which = "sequences", type = "index")
```

![](plot-review_files/figure-html/sequences-index-1.png)

## Flow plots (cograph)

[`flow_plot()`](https://sonsoles.me/vasstra/reference/flow_plot.md)
renders state movement between consecutive time points through cograph,
on the same palette and state order as every other plot, and returns a
`ggplot` object rather than drawing base graphics. Aggregated bands and
individual lines are the two views:

``` r

flow_plot(fit)
```

![](plot-review_files/figure-html/flow-alluvial-1.png)

``` r

flow_plot(fit, type = "individual")
```

![](plot-review_files/figure-html/flow-individual-1.png)

The
[`vignette("flow-plots")`](https://sonsoles.me/vasstra/articles/flow-plots.md)
article covers the rest — colouring by destination or by first/last
state, restricting to one trajectory, line bundling, thresholding
negligible flows, and tuning a figure for a manuscript.

## Transition network (Nestimate builds, cograph draws)

Node size is a transition-network centrality from
[`Nestimate::net_centrality()`](https://saqr.me/Nestimate/reference/net_centrality.html);
the network comes from
[`Nestimate::build_tna()`](https://saqr.me/Nestimate/reference/build_tna.html).
Drawn by
[`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html);
returns the tidy node table invisibly. The paired form places the
sequences beside the network on one shared palette.

``` r

transition_plot(fit)
```

![](plot-review_files/figure-html/transition-default-1.png)

``` r

transition_plot(fit, sequences = TRUE)
```

![](plot-review_files/figure-html/transition-paired-1.png)

The centralities are also available as a tidy table:

``` r

transition_centrality(fit)
#>        state InStrength OutStrength
#> 1     Active  0.1713675   0.2678571
#> 2    Average  0.5726801   0.3020833
#> 3 Disengaged  0.1592262   0.3333333
```

See
[`vignette("flow-plots")`](https://sonsoles.me/vasstra/articles/flow-plots.md)
for the centrality-driven node sizing (`size`, `weights`, `loops`), the
per-trajectory networks, and the other paired views.

## Trajectories: distribution, index, heatmap

``` r

plot(fit, type = "distribution")
```

![](plot-review_files/figure-html/trajectories-distribution-1.png)

``` r

plot(fit, type = "index")
```

![](plot-review_files/figure-html/trajectories-index-1.png)

``` r

plot(fit, type = "heatmap")
```

![](plot-review_files/figure-html/trajectories-heatmap-1.png)

## The combined per-trajectory grid

Passing several views in `type` composes them into one figure with **one
row per trajectory and one column per view**, in the order requested —
the standard per-cluster VaSSTra display, drawn in a single call with a
shared ordered legend. The transition column needs the suggested
`cograph` package.

``` r

plot(fit, type = c("transition", "index", "distribution"))
```

![](plot-review_files/figure-html/trajectories-grid-1.png)
