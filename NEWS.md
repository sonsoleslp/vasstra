# VaSStra 0.2.0

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

# VaSStra 0.1.0

- Added a four-step, pipe-friendly VaSStra workflow.
- Added a complete one-call `vasstra()` interface.
- Added tidy S3 results and summaries.
- Added the ready-to-load `engagement` chapter data with clear raw and
  course-standardized indicator names plus attached VaSStra data roles.
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
