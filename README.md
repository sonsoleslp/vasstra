# VaSStra

`VaSStra` turns longitudinal multivariate data into interpretable states,
sequences, and trajectory groups. The whole analysis is one call, every
part of it is also a simple explicit step, and every automated decision is
reported and recorded.

## Installation

```r
# install.packages("pak")
pak::pak("mohsaqr/vasstra")
```

Four vignettes ship with the package: `vignette("get-started")` for the
interface, `vignette("vasstra-tutorial")` for a complete worked analysis of
the chapter's longitudinal engagement data, `vignette("engines")` for the
mixture-model engines, and `vignette("flow-plots")` for the alluvial and
individual flow views.

## Interactive app

`launch_app()` opens a Shiny application (requires the suggested
`shiny` and `DT` packages) that runs the whole workflow interactively:
load a CSV or the built-in engagement data, map roles, fit with
automated or explicit counts, read the decisions log, walk the state /
sequence / trajectory / evaluation / fit-index tabs, rename groups
after inspecting them, and export every tidy table.

```r
VaSStra::launch_app()
```

## One call

```r
library(VaSStra)
data("engagement", package = "VaSStra")

fit <- vasstra(engagement)
#> Selected n_states = 5 (lpa, bic = 19008.229); ...
#> Selected n_trajectories = 3 (hamming + pam, silhouette = 0.231); ...
```

States are estimated by latent profile analysis (the latents package,
`"EEI"`, tidyLPA model 1) by default; `engine` switches to another mixture
model or to k-means, PAM, or hierarchical clustering. Automatic selection
compares 2 through 6 candidates and never picks a solution whose smallest
group holds under 5 percent of the observations.

## Engines

The `engine` decides how states, and possibly trajectories, are estimated:

| Engine | Indicators | Trajectories |
|---|---|---|
| `"lpa"` (default) | numeric, optionally with categorical ones | clustered from the state sequences in step 3 |
| `"lca"` | categorical | clustered from the state sequences in step 3 |
| `"multilpa"`, `"multilca"` | numeric / categorical | classes of subjects with their own mix of states, from the same model |
| `"lta"` | numeric or categorical | classes of subjects with their own transitions, from the same model |
| `"kmeans"`, `"pam"`, `"ward.D2"`, ... | numeric | clustered from the state sequences in step 3 |

The mixture models come from the latents package. They keep each
observation's posterior probabilities (`as.data.frame(states, what =
"posteriors")`), accept `indicator_missing = "fiml"` instead of imputation,
and are evaluated by latents itself: `evaluate()` and `fit_indices()`
return latents' enumeration, classification diagnostics and criterion
tables unchanged, and `plot(evaluate(...))` draws latents' plots. A
two-level engine
estimates states and trajectories together, so a subject's uncertain
states carry into its trajectory:

```r
vasstra(engagement, engine = "multilpa", n_states = 3, n_trajectories = 3)
#>   multilpa model | trajectory relative entropy 0.913 | silhouette on hamming 0.541
vasstra(engagement, engine = "lta", n_states = 3, n_trajectories = 3)
#>   lta model | trajectory relative entropy 0.775 | silhouette on hamming 0.286
```

`multilpa` describes a subject by the share of time in each state, `lta` by
how it moves between states. Their BIC tells whether the order matters: on
the engagement data `multilpa` (19368.7) beats `lta` (19373.4), so these
trajectories are about which states students occupy, not the order.

A model fitted with latents can be handed over as it is; its states (and a
two-level model's trajectory classes) are used as estimated:

```r
model <- latents::multilpa(data, vars, "user_id", n_profiles = 3,
                           n_group_classes = 3, time = "sequence_position")
vasstra(data, engine = model)
```

`vasstra()` finds the subject, time, and indicator roles from attached
role metadata or common column names, compares two through six states and
trajectories, and fits the recommended counts. Three natural shortcuts
cover most analyses:

```r
vasstra(engagement, state_labels = c("Disengaged", "Average", "Active"))
#> three labels, so three states — no n_states needed

vasstra(engagement, n_states = 2:4)
#> compares exactly these candidates and fits the recommended one

vasstra(engagement, n_states = 3)
#> fits exactly three states
```

Name any decision to take it over; automation only fills what you omit:

```r
fit <- vasstra(
  engagement,
  state_labels = c("Disengaged", "Average", "Active"),
  dissimilarity = "lcs",
  cluster_method = "ward.D2",
  positive_states = "Active",
  negative_states = "Disengaged"
)
#> the chapter analysis: three labels imply three LPA profiles
```

Tidy tables are returned directly at the requested analysis unit:

```r
as.data.frame(fit)                              # one row per student
as.data.frame(fit, unit = "observation")        # one row per student-time
as.data.frame(fit, unit = "state_profile")      # one row per state-indicator
as.data.frame(fit, unit = "trajectory")         # one row per trajectory
```

## Evaluate the clustering

`evaluate()` compares the fitted counts against the neighboring
candidates and reports per-cluster quality. For clustered states and
trajectories, plotting draws the selection curve, per-cluster silhouette
widths, and group sizes; for a model engine, the evaluation and its plots
are latents' (see `vignette("engines")`).

```r
evaluation <- evaluate(fit)
evaluation                       # candidate and per-cluster tables
as.data.frame(evaluation)        # one tidy row per compared count
plot(evaluation)                 # selection curve + silhouette + sizes
```

The same verb evaluates a single step, with any candidate range:

```r
evaluate(fit$states, n_states = 2:5)
evaluate(fit$trajectories)
```

## Tidy fit indices

`fit_indices()` returns the fit statistics of the selected clustering as
one tidy row. For a model engine it is latents' information-criterion
table unchanged (log-likelihood, AIC, KIC, and BIC, SABIC, CAIC, AWE, ICL,
CLC with groups and individuals as sample size); `compare = TRUE` gives
latents' candidate table. For a clustering engine it reports silhouette,
group sizes and the method's own objective (k-means' within-cluster sum of
squares, PAM's objective), with `best` and `fitted` markers under
`compare = TRUE`.

```r
fit_indices(fit)                            # the fitted state model
fit_indices(fit, compare = TRUE)            # against all candidate counts
fit_indices(fit, step = "trajectories")     # the trajectory clustering
```

## Relabel anything, tidily

`set_labels()` renames fitted groups after inspection — no refitting, no
touched numbers — and propagates the names through every table, sequence,
and plot, including recorded positive and negative states. Use a full
vector or rename only some groups by name:

```r
fit <- set_labels(fit, states = c("State 1" = "Disengaged",
                                  "State 2" = "Average",
                                  "State 3" = "Active"))
fit <- set_labels(fit, trajectories = c("Mostly active",
                                        "Mostly average",
                                        "Mostly disengaged"))
states <- set_labels(states, c("Low", "Average", "High"))
```

## Four explicit steps

Each step runs alone with the same automated defaults, and each accepts
the result of the previous step:

```r
states <- states(data)                  # roles and count automated
states <- states(data, n_states = 3,    # or fully explicit
                       labels = c("Low", "Average", "High"))
sequences <- sequences(states)
trajectories <- trajectories(sequences)
description <- describe(
  trajectories,
  positive_states = "High",
  negative_states = "Low"
)
```

Existing states from LPA, LCA, or another method enter directly at
step 2; the state column of a plain data frame is detected or named:

```r
sequences <- sequences(data_with_states, state = "engagement_state")
```

## Compare candidates in full detail

When automation should be replaced by an explicit comparison,
`state_choices()` and `trajectory_choices()` fit a tidy grid across
methods and counts. Nothing is selected silently; fit the inspected
candidate by its id.

```r
state_options <- state_choices(data, n_states = 2:4,
                               method = c("kmeans", "pam", "ward.D2"))
state_options
plot(state_options, metric = "silhouette")
states <- fit_state_choice(state_options)             # the recommendation
states <- fit_state_choice(state_options,             # or say what you want
                           n_states = 3, method = "pam",
                           labels = c("Low", "Average", "High"))

trajectory_options <- trajectory_choices(sequences, n_trajectories = 2:4,
                                         dissimilarity = c("hamming", "lcs"),
                                         method = c("pam", "ward.D2"))
trajectory_options
plot(trajectory_options, metric = "silhouette")
trajectories <- fit_trajectory_choice(trajectory_options,
                                      n_trajectories = 3,
                                      dissimilarity = "lcs",
                                      method = "ward.D2")
```

Average silhouette is the common diagnostic; recommendations maximize it
within each method (LPA candidates use conventional BIC by default via
`lpa_criterion`), subject to the requested group-size constraints.

## Plots

State results support profile, bar, heatmap, and size plots plus a
combined overview, all without adding a plotting dependency:

```r
plot(states, type = "all")        # profile + bars + heatmap + sizes
plot(states, type = "profile")
plot(states, type = "profile", scale = "original")
```

Every plot containing state sequences is rendered by `Nestimate`:

```r
plot(sequences)                          # heatmap of every aligned sequence
plot(sequences, type = "distribution")
plot(trajectories, type = "index")
plot(trajectories, type = "heatmap")
```

## Flow plots

Sequence plots are organised by subject similarity, so they show who
resembles whom but not where movement goes — and a flat state distribution
cannot distinguish a cohort where nobody moves from one where everybody
swaps. `flow_plot()` draws the movement itself, rendered by
[`cograph`](https://github.com/mohsaqr/cograph) (a suggested package), on
the same palette and state order as every other VaSStra plot:

```r
flow_plot(fit)                              # aggregated alluvial bands
flow_plot(fit, type = "individual")         # one line per subject
flow_plot(fit, color_by = "destination")    # colour bands by where they arrive
flow_plot(fit, group = "Mostly average")    # one trajectory's flows
```

Individual flows are bundled automatically so large cohorts stay legible
(`bundle = FALSE` draws every subject). Unlike the base-graphics `plot()`
methods, `flow_plot()` returns a `ggplot` object.

## Transition networks

`transition_plot()` collapses every time step into one network: states are
nodes, transitions are directed edges, and node size is a centrality of the
transition network. `Nestimate` builds the network and the centralities
(`build_tna()`, `net_centrality()`), and `cograph::splot()` draws it —
recognising the network object and supplying TNA styling, labels, and
initial-probability rings on its own. The default node size is in-strength,
so the largest node is the state the cohort most often moves *into*.

```r
transition_plot(fit)                                    # size = in-strength
transition_plot(fit, weights = "count")                 # raw counts
transition_plot(fit, size = "OutStrength")              # any Nestimate measure
transition_plot(fit, loops = TRUE)                      # count self-transitions
transition_plot(fit, group = "Mostly disengaged")       # one trajectory
transition_plot(fit, sequences = TRUE)                  # sequences + network
```

`sequences = TRUE` draws the state sequences beside the network — the
conventional pairing, where the left panel shows the raw data and the right
summarises its movement. Both panels share one palette, so a state has the
same colour in each; `"heatmap"` and `"distribution"` select the left view.

The same centralities come as a tidy table, without drawing anything:

```r
transition_centrality(fit)                     # in- and out-strength
transition_centrality(fit, measures = "all")   # every Nestimate measure
transition_centrality(fit, weights = "count")
```

Self-transitions are excluded from the centrality by default, matching
`Nestimate::net_centrality()` and `tna::centralities()` — otherwise a
persistent state looks large merely because its members stay put, which is
a different claim from attracting movement. See `vignette("flow-plots")`.

## Dependencies

`latents` fits the mixture-model engines, including the default latent
profile analysis, base R provides k-means and hierarchical clustering,
`cluster` provides PAM, and
`Nestimate` provides sequence distances, trajectory clustering, and all
sequence plots. `cograph` is suggested, not required, and renders the flow
plots. There is no TraMineR plotting path or dependency.

Method overview: [VaSSTra chapter](https://lamethods.org/book1/chapters/ch11-vasstra/ch11-vasstra.html).
