# Package index

## Complete workflow

Run the whole variables-to-trajectories analysis in one call.

- [`vasstra()`](https://sonsoles.me/vasstra/reference/vasstra.md) : Run
  the Complete VaSSTra Workflow

## Explicit steps

The four steps
[`vasstra()`](https://sonsoles.me/vasstra/reference/vasstra.md) runs,
for full control over each stage.

- [`step1_states()`](https://sonsoles.me/vasstra/reference/step1_states.md)
  : Step 1: Turn Variables into States
- [`step2_sequences()`](https://sonsoles.me/vasstra/reference/step2_sequences.md)
  : Step 2: Turn States into Sequences
- [`step3_trajectories()`](https://sonsoles.me/vasstra/reference/step3_trajectories.md)
  : Step 3: Turn Sequences into Trajectories
- [`step4_describe()`](https://sonsoles.me/vasstra/reference/step4_describe.md)
  : Step 4: Describe the Trajectories

## Choosing and evaluating a model

Compare cluster counts and methods, and read fit statistics.

- [`evaluate()`](https://sonsoles.me/vasstra/reference/evaluate.md) :
  Evaluate a Fitted Clustering
- [`fit_indices()`](https://sonsoles.me/vasstra/reference/fit_indices.md)
  : Tidy Fit Indices for a Fitted Clustering
- [`state_choices()`](https://sonsoles.me/vasstra/reference/state_choices.md)
  : Compare Choices for the Number and Method of States
- [`trajectory_choices()`](https://sonsoles.me/vasstra/reference/trajectory_choices.md)
  : Compare Choices for the Number and Method of Trajectories
- [`fit_state_choice()`](https://sonsoles.me/vasstra/reference/fit_state_choice.md)
  : Fit One State Choice
- [`fit_trajectory_choice()`](https://sonsoles.me/vasstra/reference/fit_trajectory_choice.md)
  : Fit One Trajectory Choice

## Plots

Sequence, trajectory, state, and evaluation views, plus transition
networks and state-flow diagrams.

- [`plot(`*`<vasstra>`*`)`](https://sonsoles.me/vasstra/reference/plot.vasstra.md)
  : Plot a Complete VaSSTra Analysis
- [`plot(`*`<vasstra_states>`*`)`](https://sonsoles.me/vasstra/reference/plot.vasstra_states.md)
  : Plot Estimated VaSSTra States
- [`plot(`*`<vasstra_sequences>`*`)`](https://sonsoles.me/vasstra/reference/plot.vasstra_sequences.md)
  : Plot VaSSTra State Sequences with Nestimate
- [`plot(`*`<vasstra_trajectories>`*`)`](https://sonsoles.me/vasstra/reference/plot.vasstra_trajectories.md)
  : Plot VaSSTra Trajectories with Nestimate
- [`plot(`*`<vasstra_evaluation>`*`)`](https://sonsoles.me/vasstra/reference/plot.vasstra_evaluation.md)
  : Plot a Clustering Evaluation
- [`plot(`*`<vasstra_evaluations>`*`)`](https://sonsoles.me/vasstra/reference/plot.vasstra_evaluations.md)
  : Plot the Evaluations of a Complete Fit
- [`plot(`*`<vasstra_state_choices>`*`)`](https://sonsoles.me/vasstra/reference/plot.vasstra_state_choices.md)
  : Plot State-Clustering Choices
- [`plot(`*`<vasstra_trajectory_choices>`*`)`](https://sonsoles.me/vasstra/reference/plot.vasstra_trajectory_choices.md)
  : Plot Trajectory-Clustering Choices
- [`transition_plot()`](https://sonsoles.me/vasstra/reference/transition_plot.md)
  : Plot the State Transition Network
- [`transition_centrality()`](https://sonsoles.me/vasstra/reference/transition_centrality.md)
  : State Transition Network Centralities
- [`flow_plot()`](https://sonsoles.me/vasstra/reference/flow_plot.md) :
  Plot State Flows Between Consecutive Time Points

## Labels and tidy output

Rename groups after inspection and extract tidy tables.

- [`set_labels()`](https://sonsoles.me/vasstra/reference/set_labels.md)
  : Relabel Fitted States or Trajectories
- [`as.data.frame(`*`<vasstra>`*`)`](https://sonsoles.me/vasstra/reference/as.data.frame.vasstra.md)
  : Convert a VaSSTra Analysis to a Tidy Table

## Interactive app

- [`launch_app()`](https://sonsoles.me/vasstra/reference/launch_app.md)
  : Launch the Interactive VaSSTra App

## Data

- [`engagement`](https://sonsoles.me/vasstra/reference/engagement.md) :
  Longitudinal Student Engagement
