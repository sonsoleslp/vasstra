# Changelog

## VaSStra 0.3.0

- Added
  [`flow_plot()`](https://pak.dynasite.org/VaSStra/reference/flow_plot.md):
  alluvial and individual flow views of state movement between
  consecutive time points, rendered by the suggested `cograph` package.
  `type = "alluvial"` draws aggregated bands whose width is the number
  of subjects making each move; `type = "individual"` draws one line per
  subject, bundled automatically so large cohorts stay legible. The
  state palette, state order, and time labels are taken from the fit, so
  a flow plot is directly comparable with the sequence heatmap.
  `color_by` selects the state that colours a flow, and `group`
  restricts a trajectory plot to one group. Unlike the base-graphics
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods,
  [`flow_plot()`](https://pak.dynasite.org/VaSStra/reference/flow_plot.md)
  returns a `ggplot` object.
- Added
  [`transition_plot()`](https://pak.dynasite.org/VaSStra/reference/transition_plot.md):
  the state transition network, with states as nodes, transitions as
  directed edges, and node size encoding a centrality. The network is
  built by
  [`Nestimate::build_tna()`](https://saqr.me/Nestimate/reference/build_tna.html)
  (or `build_ftna()` for `weights = "count"`) and the centralities come
  from
  [`Nestimate::net_centrality()`](https://saqr.me/Nestimate/reference/net_centrality.html),
  so they match `tna::centralities()`; cograph draws it. `size` selects
  the measure (`"InStrength"` by default), `loops` controls whether
  self-transitions count toward node size (`FALSE` by default, so a
  persistent state is not large merely for retaining its own members),
  `size_range` sets the node diameters, and `group` restricts the
  network to one trajectory. The call draws the network and returns the
  tidy state-by-centrality table it drew.
- `transition_plot(sequences = TRUE)` draws the state sequences beside
  the transition network on one device — the conventional pairing of raw
  data and movement summary. `TRUE` uses an index plot; `"heatmap"` and
  `"distribution"` select the left panel. Both panels share the state
  palette, so a state has the same colour in each.
- **Bug fix**: `colors` passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on sequences
  or trajectories was applied to states in alphabetical order rather
  than by state, so every state could be drawn in another state’s
  colour. Nestimate assigns `state_colors` positionally to
  alphabetically sorted states and ignores factor levels; colours are
  now re-aligned by state name. Only user-supplied `colors` were
  affected — the default palette was never mismapped.
- Added
  [`vignette("flow-plots")`](https://pak.dynasite.org/VaSStra/articles/flow-plots.md),
  which uses the flow views to show that the engagement cohort’s flat
  state distribution is a balanced exchange rather than an absence of
  change, and that movement between the extreme states almost always
  passes through the middle one.
- Added examples to all eight
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods,
  [`as.data.frame.vasstra()`](https://pak.dynasite.org/VaSStra/reference/as.data.frame.vasstra.md),
  and
  [`launch_app()`](https://pak.dynasite.org/VaSStra/reference/launch_app.md);
  every exported topic is now documented with a runnable example.
- Corrected the
  [`state_choices()`](https://pak.dynasite.org/VaSStra/reference/state_choices.md)
  and
  [`trajectory_choices()`](https://pak.dynasite.org/VaSStra/reference/trajectory_choices.md)
  plot documentation, which still described the pre-redesign star and
  cross markers instead of the ring and hollow-point markers actually
  drawn.

## VaSStra 0.2.0

- The sequence heatmap is now the default plot for complete sequence
  sets (`plot(sequences)` / `plot(fit, which = "sequences")`): it keeps
  every aligned sequence visible at full resolution. Titles are now
  type-specific; `"distribution"` and `"index"` remain one argument
  away, and grouped trajectory plots keep `"index"` as their default.

- Added
  [`launch_app()`](https://pak.dynasite.org/VaSStra/reference/launch_app.md):
  an interactive Shiny application (suggested `shiny` + `DT`) covering
  the complete workflow — data upload or the built-in engagement data,
  role mapping with detection pre-fill, automated or explicit counts
  with an in-app decisions log, state / sequence / trajectory plots,
  evaluation panels, tidy fit-index tables, in-app group renaming
  through
  [`set_labels()`](https://pak.dynasite.org/VaSStra/reference/set_labels.md),
  and tidy CSV exports at every analysis unit.

- Added
  [`fit_indices()`](https://pak.dynasite.org/VaSStra/reference/fit_indices.md):
  tidy fit statistics for the selected clustering (one row) or all
  compared candidates (`compare = TRUE`). LPA reports log-likelihood,
  AIC, BIC, SABIC, CAIC, AWE, CLC, KIC, ICL (all on the conventional
  lower-is-better scale), normalized entropy, and minimum and maximum
  average posterior class probabilities; hard methods report their own
  objectives; silhouette and group sizes are always included and
  inapplicable columns are dropped.

- The extended information criteria and posterior-probability summaries
  are also recorded in
  [`state_choices()`](https://pak.dynasite.org/VaSStra/reference/state_choices.md)
  candidate tables and step-1 diagnostics.

- Added
  [`set_labels()`](https://pak.dynasite.org/VaSStra/reference/set_labels.md):
  rename fitted states and trajectories in place — full vectors or
  partial named renames such as `c("State 1" = "Disengaged")` —
  propagated through every derived table, sequence, and recorded
  positive/negative state, without refitting or changing any value.

- Latent profile analysis is now the default state method
  (`state_method = "lpa"`, mclust `"EEI"` — tidyLPA model 1, the model
  used in the VaSSTra chapter). k-means, PAM, and hierarchical
  clustering remain available; `mclust` moved from Suggests to Imports.

- Automatic count selection applies a 5 percent minimum group share, so
  spuriously small states or trajectories are never auto-selected;
  explicit
  [`state_choices()`](https://pak.dynasite.org/VaSStra/reference/state_choices.md)/[`trajectory_choices()`](https://pak.dynasite.org/VaSStra/reference/trajectory_choices.md)
  comparisons keep their permissive defaults.

- Labels imply the count: with `n_states`/`n_trajectories` left on
  `"auto"`, supplying three labels fits three groups.

- `n_states` and `n_trajectories` also accept a candidate vector (for
  example `2:4`) to compare exactly those counts and fit the recommended
  one.

- [`fit_state_choice()`](https://pak.dynasite.org/VaSStra/reference/fit_state_choice.md)/[`fit_trajectory_choice()`](https://pak.dynasite.org/VaSStra/reference/fit_trajectory_choice.md)
  fit the recommended candidate when called with only the choices
  object, and select candidates by `n_states`/`method`/`lpa_model` (or
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

- [`step1_states()`](https://pak.dynasite.org/VaSStra/reference/step1_states.md),
  [`step2_sequences()`](https://pak.dynasite.org/VaSStra/reference/step2_sequences.md),
  and
  [`step3_trajectories()`](https://pak.dynasite.org/VaSStra/reference/step3_trajectories.md)
  gain the same automation, so each step also runs alone with minimal
  arguments;
  [`step2_sequences()`](https://pak.dynasite.org/VaSStra/reference/step2_sequences.md)
  detects a single categorical state column in plain data frames.

- Added the
  [`evaluate()`](https://pak.dynasite.org/VaSStra/reference/evaluate.md)
  verb for states, trajectories, and complete fits: one tidy row per
  compared cluster count with `best` and `fitted` markers, plus a
  per-cluster quality table with mean silhouette widths.

- Added evaluation plots (`plot(evaluate(fit))`): selection curve,
  per-cluster silhouette widths, and group sizes in one layout.

- Added state plot types `"bars"` (grouped indicator means) and `"all"`
  (profile + bars + heatmap + sizes overview).

- Unified all base-graphics plots on one colorblind-safe palette and a
  lighter shared style; sequence plots remain delegated to Nestimate.

- Automatic selection surfaces the underlying error when every candidate
  fails (for example missing indicator values with `missing = "error"`).

- [`vasstra()`](https://pak.dynasite.org/VaSStra/reference/vasstra.md)
  now errors only when both `variables` and `state` are supplied; when
  neither is given, indicator columns are resolved automatically.

## VaSStra 0.1.0

- Added a four-step, pipe-friendly VaSStra workflow.
- Added a complete one-call
  [`vasstra()`](https://pak.dynasite.org/VaSStra/reference/vasstra.md)
  interface.
- Added tidy S3 results and summaries.
- Added the ready-to-load `engagement` chapter data with clear raw and
  course-standardized indicator names plus attached VaSStra data roles.
- Added metadata-aware one-call workflows and tidy complete-fit tables
  at the subject, observation, state-profile, and trajectory units.
- Added
  [`state_choices()`](https://pak.dynasite.org/VaSStra/reference/state_choices.md)
  and
  [`fit_state_choice()`](https://pak.dynasite.org/VaSStra/reference/fit_state_choice.md)
  for explicit comparison and fitting of k-means, PAM, hierarchical, and
  optional mclust LPA states.
- Added
  [`trajectory_choices()`](https://pak.dynasite.org/VaSStra/reference/trajectory_choices.md)
  and
  [`fit_trajectory_choice()`](https://pak.dynasite.org/VaSStra/reference/fit_trajectory_choice.md)
  for tidy Nestimate comparisons across sequence distances, clustering
  methods, and trajectory counts.
- Added lightweight state profile, heatmap, size, and metric-aware
  choice plots; sequence-based visualizations remain delegated to
  Nestimate.
- Added sequence clustering and all sequence plots through `Nestimate`.
- Added a base-R state-profile plot; no TraMineR dependency is required.
