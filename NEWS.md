# VaSSTra 0.5.0

## Plain step names

- The four steps are now `states()`, `sequences()`, `trajectories()` and
  `describe()`. `step1_states()`, `step2_sequences()`,
  `step3_trajectories()` and `step4_describe()` remain as aliases for this
  release and will be removed in the next.
- `describe()` shares its name with `psych::describe()` and
  `Hmisc::describe()`; whichever package is attached last is found first,
  and `VaSSTra::describe()` always reaches this one.

## Engines

- `states()` and `vasstra()` take an `engine`: the mixture models of
  the latents package (`"lpa"`, the default, `"lca"`, `"multilpa"`,
  `"multilca"`, `"lta"`) or the clustering methods (`"kmeans"`, `"pam"`,
  `"ward.D2"`, ...). `method` (step 1) and `state_method` (`vasstra()`) are
  superseded by it and still accepted. See `vignette("engines")`.
- The default latent profile analysis is now fitted with latents instead of
  mclust, from several random starts. On the engagement data it reaches a
  higher log likelihood than mclust's single run (3 states: -9940.167
  against -9940.466), so the automatic selection can differ slightly;
  `vasstra(engagement)` still selects 5 states and 3 trajectories.
- The two-level engines (`"multilpa"`, `"multilca"`, `"lta"`) estimate the
  states and the trajectory classes in one model. `trajectories()`
  takes the classes from the model (`method = "model"`, the default for
  such states) unless a distance or clustering method is named, and
  `n_trajectories` is chosen jointly with `n_states` when either is
  `"auto"`. A model that leaves a trajectory class empty warns.
- A fitted latents model can be passed as `engine`; its states, and a
  two-level model's trajectory classes, are used as estimated, after
  checking that `data` is the data it was fitted to.
- Model engines keep each observation's posterior probabilities
  (`as.data.frame(states, what = "posteriors")`), take categorical
  indicators (`"lca"`, `"multilca"`, or non-numeric columns with the other
  model engines), and accept `missing = "fiml"` instead of median
  imputation.
- A model engine is evaluated by latents, not by VaSSTra: `evaluate()`
  returns latents' enumeration of the other counts
  (`enumerate_lpa()`, `enumerate_lca()`, `enumerate_classes()`) and
  `latents::diagnostics()` of the fitted model, `plot()` draws latents'
  plots (`plot_enumeration()`, the diagnostics panels; needs ggplot2), and
  `fit_indices()` returns latents' information-criterion and candidate
  tables unchanged. AWE thereby includes the classification-entropy term of
  Banfield and Raftery (1993), which the hand-computed version left out.
- `state_choices()` compares a model engine through one latents
  enumeration, and automatic selection uses the chosen candidate as latents
  fitted it, without refitting. It takes `n_trajectories` for the
  two-level engines, and records each candidate's warnings in a `warnings`
  column instead of dropping them.
- State profile plots draw a categorical indicator as one row per category,
  holding the share of each state's observations in it.
- The Shiny app offers every engine.
- `latents (>= 0.9.36)` is imported (installed from GitHub through
  `Remotes:` until that version is on CRAN, which has 0.9.21); mclust moves
  to Suggests and ggplot2 joins Suggests for latents' plots.

# VaSSTra 0.4.0

- `plot()` on states gains `type = "variables"`: grouped boxplots of the
  clustering indicators split by state, one panel per time point, so each
  indicator's separation of the states is visible at every point rather than
  only on average. `by_time = FALSE` pools the time points into a single
  panel, the boxplot counterpart of the profile plot.
- Added `state_descriptives()`: the numeric companion to that plot, a tidy
  per-state, per-indicator table of count, mean, standard deviation, median,
  and range. `scale = "original"` reports the raw indicators and
  `by_time = TRUE` breaks the summary out per time point.
- `plot()` on an `evaluate()` result now colours the state row with the
  palette pinned on the fit (`state_colors`), so it matches the other state
  plots; the trajectory row keeps its distinct palette.

# VaSSTra 0.3.0

- `standardize` gains a `"subject"` option that z-scores each indicator
  within each subject, so states reflect deviations from a person's own
  baseline rather than absolute level. The default is unchanged (`"time"`),
  and `"global"` and `"none"` are still available.
- Added `transition_table()`: the transition counts behind the flow and
  network views as a tidy `from`/`to`/`count`/`probability` data frame,
  aggregated over time by default or broken out per consecutive time pair
  with `by_time = TRUE`. `group` restricts a trajectory fit to one group.
- Added three state-clustering methods, all from the already-imported
  `cluster` package: `"clara"` (sampling-based PAM for larger data),
  `"fanny"` (fuzzy clustering, hardened to its nearest partition), and
  `"diana"` (divisive hierarchical clustering). They are available through
  `state_method` in `vasstra()`, `method` in `step1_states()`, and
  `state_choices()`, and are selected by average silhouette like the other
  non-LPA methods.
- `plot()` on an `evaluate()` result now draws the trajectory groups in a
  palette distinct from the states, so the state and trajectory evaluation
  rows are no longer coloured identically (which read as a correspondence
  between unrelated groupings).
- Added a `state_colors` argument to `vasstra()`, `step1_states()`, and
  `step2_sequences()` that stores a state palette on the fit, so every plot
  reuses it without repeating `colors`. Accepts a named vector (matched by
  state, and preserved through `state_order` and `set_labels()`) or one
  colour per state in state order; an explicit `colors` on a plot still
  wins.
- The state profile heatmap now draws the first state as the top row, so it
  reads in the same order as the profile bars, state sizes, and legends.
- Fixed the state-sizes plot, whose bar heights were taken in cluster
  (discovery) order while the labels followed the display order, mislabelling
  the bars when `state_order` reordered the states.
- Added a `state_order` argument to `vasstra()`, `step1_states()`, and
  `step2_sequences()` that fixes the order the states appear in **every**
  plot -- distribution stacking, sequence and transition legends,
  transition and flow nodes, and the combined grid -- independent of the
  order the clusters were discovered in. Because Nestimate sorts states
  alphabetically, the sequence views recode the states internally to force
  the requested order and draw a matching ordered legend. Colours follow
  the state order; pass `colors` for a fixed state-to-colour mapping.
- `plot()` on trajectories now accepts a vector of views in `type`, for
  example `plot(fit, type = c("transition", "index", "distribution"))`.
  Several views (or the new `"transition"` network) lay out a grid with
  one row per trajectory and one column per requested view, reproducing
  the familiar per-cluster VaSSTra figure in a single call, with one
  shared state legend along the bottom (`legend = "none"` omits it). A
  single sequence view keeps the existing faceted whole-cohort figure, so
  `plot(fit$trajectories)` is unchanged.
- Added `flow_plot()`: alluvial and individual flow views of state
  movement between consecutive time points, rendered by the suggested
  `cograph` package. `type = "alluvial"` draws aggregated bands whose
  width is the number of subjects making each move; `type = "individual"`
  draws one line per subject, bundled automatically so large cohorts stay
  legible. The state palette, state order, and time labels are taken from
  the fit, so a flow plot is directly comparable with the sequence
  heatmap. `color_by` selects the state that colours a flow, and `group`
  restricts a trajectory plot to one group. Unlike the base-graphics
  `plot()` methods, `flow_plot()` returns a `ggplot` object.
- Added `transition_plot()`: the state transition network, with states as
  nodes, transitions as directed edges, and node size encoding a
  centrality. The network is built by `Nestimate::build_tna()` (or
  `build_ftna()` for `weights = "count"`) and the centralities come from
  `Nestimate::net_centrality()`, so they match `tna::centralities()`;
  cograph draws it. `size` selects the measure (`"InStrength"` by
  default), `loops` controls whether self-transitions count toward node
  size (`FALSE` by default, so a persistent state is not large merely for
  retaining its own members), `size_range` sets the node diameters, and
  `group` restricts the network to one trajectory. The call draws the
  network and returns the tidy state-by-centrality table it drew.
- `transition_plot(sequences = TRUE)` draws the state sequences beside the
  transition network on one device — the conventional pairing of raw data
  and movement summary. `TRUE` uses an index plot; `"heatmap"` and
  `"distribution"` select the left panel. Both panels share the state
  palette, so a state has the same colour in each.
- **Bug fix**: `colors` passed to `plot()` on sequences or trajectories was
  applied to states in alphabetical order rather than by state, so every
  state could be drawn in another state's colour. Nestimate assigns
  `state_colors` positionally to alphabetically sorted states and ignores
  factor levels; colours are now re-aligned by state name. Only
  user-supplied `colors` were affected — the default palette was never
  mismapped.
- Added `vignette("flow-plots")`, which uses the flow views to show that
  the engagement cohort's flat state distribution is a balanced exchange
  rather than an absence of change, and that movement between the extreme
  states almost always passes through the middle one.
- Added examples to all eight `plot()` methods, `as.data.frame.vasstra()`,
  and `launch_app()`; every exported topic is now documented with a
  runnable example.
- Corrected the `state_choices()` and `trajectory_choices()` plot
  documentation, which still described the pre-redesign star and cross
  markers instead of the ring and hollow-point markers actually drawn.

# VaSSTra 0.2.0

- The sequence heatmap is now the default plot for complete sequence
  sets (`plot(sequences)` / `plot(fit, which = "sequences")`): it keeps
  every aligned sequence visible at full resolution. Titles are now
  type-specific; `"distribution"` and `"index"` remain one argument
  away, and grouped trajectory plots keep `"index"` as their default.
- Added `launch_app()`: an interactive Shiny application (suggested
  `shiny` + `DT`) covering the complete workflow — data upload or the
  built-in engagement data, role mapping with detection pre-fill,
  automated or explicit counts with an in-app decisions log, state /
  sequence / trajectory plots, evaluation panels, tidy fit-index
  tables, in-app group renaming through `set_labels()`, and tidy CSV
  exports at every analysis unit.
- Added `fit_indices()`: tidy fit statistics for the selected clustering
  (one row) or all compared candidates (`compare = TRUE`). LPA reports
  log-likelihood, AIC, BIC, SABIC, CAIC, AWE, CLC, KIC, ICL (all on the
  conventional lower-is-better scale), normalized entropy, and minimum
  and maximum average posterior class probabilities; hard methods report
  their own objectives; silhouette and group sizes are always included
  and inapplicable columns are dropped.
- The extended information criteria and posterior-probability summaries
  are also recorded in `state_choices()` candidate tables and step-1
  diagnostics.
- Added `set_labels()`: rename fitted states and trajectories in place —
  full vectors or partial named renames such as
  `c("State 1" = "Disengaged")` — propagated through every derived
  table, sequence, and recorded positive/negative state, without
  refitting or changing any value.
- Latent profile analysis is now the default state method
  (`state_method = "lpa"`, mclust `"EEI"` — tidyLPA model 1, the model
  used in the VaSSTra chapter). k-means, PAM, and hierarchical
  clustering remain available; `mclust` moved from Suggests to Imports.
- Automatic count selection applies a 5 percent minimum group share, so
  spuriously small states or trajectories are never auto-selected;
  explicit `state_choices()`/`trajectory_choices()` comparisons keep
  their permissive defaults.
- Labels imply the count: with `n_states`/`n_trajectories` left on
  `"auto"`, supplying three labels fits three groups.
- `n_states` and `n_trajectories` also accept a candidate vector (for
  example `2:4`) to compare exactly those counts and fit the recommended
  one.
- `fit_state_choice()`/`fit_trajectory_choice()` fit the recommended
  candidate when called with only the choices object, and select
  candidates by `n_states`/`method`/`lpa_model` (or
  `n_trajectories`/`dissimilarity`/`method`) instead of `candidate_id`.
- Redesigned line plots: state profiles use solid weighted lines with
  direct labels at the line ends (no legend, no symbol rotation); choice
  curves use solid palette lines with ring markers for metric optima,
  hollow points for ineligible candidates, and a subtitle for the metric
  direction.

- `vasstra(data)` now runs with no other arguments: subject, time, and
  indicator roles are resolved from explicit arguments, attached role
  metadata, or common column names, and the numbers of states and
  trajectories default to `"auto"`. Every automated decision is reported
  with a message and recorded in `diagnostics$selection`.
- `step1_states()`, `step2_sequences()`, and `step3_trajectories()` gain
  the same automation, so each step also runs alone with minimal
  arguments; `step2_sequences()` detects a single categorical state
  column in plain data frames.
- Added the `evaluate()` verb for states, trajectories, and complete
  fits: one tidy row per compared cluster count with `best` and `fitted`
  markers, plus a per-cluster quality table with mean silhouette widths.
- Added evaluation plots (`plot(evaluate(fit))`): selection curve,
  per-cluster silhouette widths, and group sizes in one layout.
- Added state plot types `"bars"` (grouped indicator means) and `"all"`
  (profile + bars + heatmap + sizes overview).
- Unified all base-graphics plots on one colorblind-safe palette and a
  lighter shared style; sequence plots remain delegated to Nestimate.
- Automatic selection surfaces the underlying error when every candidate
  fails (for example missing indicator values with `missing = "error"`).
- `vasstra()` now errors only when both `variables` and `state` are
  supplied; when neither is given, indicator columns are resolved
  automatically.

# VaSSTra 0.1.0

- Added a four-step, pipe-friendly VaSSTra workflow.
- Added a complete one-call `vasstra()` interface.
- Added tidy S3 results and summaries.
- Added the ready-to-load `engagement` chapter data with clear raw and
  course-standardized indicator names plus attached VaSSTra data roles.
- Added metadata-aware one-call workflows and tidy complete-fit tables at the
  subject, observation, state-profile, and trajectory units.
- Added `state_choices()` and `fit_state_choice()` for explicit comparison
  and fitting of k-means, PAM, hierarchical, and optional mclust LPA states.
- Added `trajectory_choices()` and `fit_trajectory_choice()` for tidy
  Nestimate comparisons across sequence distances, clustering methods, and
  trajectory counts.
- Added lightweight state profile, heatmap, size, and metric-aware choice
  plots; sequence-based visualizations remain delegated to Nestimate.
- Added sequence clustering and all sequence plots through `Nestimate`.
- Added a base-R state-profile plot; no TraMineR dependency is required.
