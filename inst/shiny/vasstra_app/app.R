# VaSStra interactive app: variables -> states -> sequences -> trajectories.
# Launched through VaSStra::launch_app(); shiny and DT are suggested packages.

library(shiny)
library(VaSStra)

options(shiny.maxRequestSize = 100 * 1024^2)

data("engagement", package = "VaSStra", envir = environment())

.parse_labels <- function(text) {
  values <- trimws(strsplit(text, ",", fixed = TRUE)[[1L]])
  values <- values[nzchar(values)]
  if (length(values) == 0L) NULL else values
}

.count_choice <- function(value) {
  if (identical(value, "auto")) "auto" else as.integer(value)
}

app_css <- "
  body { font-family: 'Segoe UI', Helvetica, Arial, sans-serif; }
  .app-title {
    background: linear-gradient(90deg, #1a56db, #2c7be5, #4facfe);
    color: white; padding: 18px 24px; border-radius: 8px;
    margin-bottom: 16px;
  }
  .app-title h2 { margin: 0; font-weight: 600; }
  .app-title .subtitle { margin: 4px 0 0; opacity: 0.92; font-size: 14.5px; }
  .sidebar-panel { background: #f8fafc; border: 1px solid #e2e8f0;
    border-radius: 8px; }
  .section-header {
    color: #1a56db; font-weight: 600; font-size: 13px;
    text-transform: uppercase; letter-spacing: 0.4px;
    border-bottom: 1px solid #dbeafe; margin: 16px 0 8px; padding-bottom: 3px;
  }
  .btn-primary, .btn-primary:focus {
    background: #2c7be5; border-color: #2c7be5;
  }
  .help-section {
    background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px;
    padding: 14px 18px; margin-bottom: 14px;
  }
  .help-section h4 { color: #1a56db; margin-top: 4px; }
  .measure-table { width: 100%; border-collapse: collapse; margin: 8px 0; }
  .measure-table th {
    text-align: left; color: #1a56db; border-bottom: 2px solid #dbeafe;
    padding: 5px 10px 5px 0; font-size: 13.5px;
  }
  .measure-table td {
    border-bottom: 1px solid #eef2f7; padding: 5px 10px 5px 0;
    font-size: 13.5px; vertical-align: top;
  }
  .decisions-box {
    background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 8px;
    padding: 10px 14px; margin-bottom: 12px;
    font-family: monospace; font-size: 12.5px; white-space: pre-wrap;
  }
  .app-footer {
    margin-top: 24px; padding: 14px; color: #64748b; font-size: 12.5px;
    border-top: 1px solid #e2e8f0;
  }
  .app-footer a { color: #2c7be5; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  div(
    class = "app-title",
    h2("VaSStra"),
    p(
      class = "subtitle",
      "Variables to States to Sequences to Trajectories — ",
      "person-centered analysis of longitudinal multivariate data"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "sidebar-panel",
      div(class = "section-header", "Data"),
      radioButtons(
        "data_source",
        label = NULL,
        choices = c(
          "Built-in: engagement (chapter data)" = "engagement",
          "Upload CSV" = "upload"
        ),
        selected = "engagement"
      ),
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput(
          "file",
          NULL,
          accept = c(".csv", "text/csv"),
          placeholder = "Choose CSV file"
        )
      ),
      div(class = "section-header", "Roles"),
      uiOutput("ui_id"),
      uiOutput("ui_time"),
      uiOutput("ui_variables"),
      div(class = "section-header", "States"),
      selectInput(
        "n_states",
        "Number of states",
        choices = c("auto", as.character(2:8)),
        selected = "auto"
      ),
      selectInput(
        "state_method",
        "State method",
        choices = c(
          "lpa", "kmeans", "pam", "ward.D2", "ward.D", "complete",
          "average", "single", "mcquitty", "median", "centroid"
        ),
        selected = "lpa"
      ),
      conditionalPanel(
        condition = "input.state_method == 'lpa'",
        selectInput(
          "lpa_model",
          "LPA covariance model",
          choices = c(
            "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE",
            "VEE", "EVE", "VVE", "EEV", "VEV", "EVV", "VVV"
          ),
          selected = "EEI"
        )
      ),
      textInput(
        "state_labels",
        "State labels (comma-separated, imply the count)",
        placeholder = "e.g. Disengaged, Average, Active"
      ),
      div(class = "section-header", "Trajectories"),
      selectInput(
        "n_trajectories",
        "Number of trajectories",
        choices = c("auto", as.character(2:8)),
        selected = "auto"
      ),
      selectInput(
        "dissimilarity",
        "Sequence distance",
        choices = c(
          "hamming", "osa", "lv", "dl", "lcs",
          "qgram", "cosine", "jaccard", "jw"
        ),
        selected = "lcs"
      ),
      selectInput(
        "cluster_method",
        "Clustering method",
        choices = c(
          "ward.D2", "pam", "ward.D", "complete", "average",
          "single", "mcquitty", "median", "centroid"
        ),
        selected = "ward.D2"
      ),
      textInput(
        "trajectory_labels",
        "Trajectory labels (comma-separated, imply the count)",
        placeholder = "e.g. Mostly active, Mixed, Mostly disengaged"
      ),
      div(class = "section-header", "Description"),
      selectInput(
        "positive_states",
        "Positive states",
        choices = character(0),
        multiple = TRUE
      ),
      selectInput(
        "negative_states",
        "Negative states",
        choices = character(0),
        multiple = TRUE
      ),
      numericInput("seed", "Seed", value = 123, min = 1, step = 1),
      actionButton(
        "run",
        "Run VaSStra",
        class = "btn-primary btn-block",
        icon = icon("play")
      ),
      div(class = "section-header", "Rename fitted groups"),
      textInput("rename_states", "New state labels", placeholder = "optional"),
      textInput(
        "rename_trajectories",
        "New trajectory labels",
        placeholder = "optional"
      ),
      actionButton("rename", "Apply labels", class = "btn-block")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Quick Start",
          div(
            class = "help-section",
            h4("Get started in three steps"),
            tags$ol(
              tags$li(
                "Pick the built-in engagement data or upload a CSV with ",
                "one row per subject and time point; the subject, time, ",
                "and indicator roles are pre-filled when they can be ",
                "detected."
              ),
              tags$li(
                strong("Run VaSStra."),
                " Counts left on “auto” are selected by comparing ",
                "2–6 candidates; every automated decision is reported ",
                "in the Decisions box and recorded in the fit. Supplying ",
                "labels fixes the count directly — three labels mean ",
                "three groups."
              ),
              tags$li(
                "Read the Summary, walk the States → Sequences → ",
                "Trajectories tabs, check Evaluation and Fit indices, ",
                "rename groups once you can interpret them, and export ",
                "the tidy tables."
              )
            )
          ),
          div(
            class = "help-section",
            h4("Key options"),
            tags$table(
              class = "measure-table",
              tags$tr(tags$th("Option"), tags$th("What it decides")),
              tags$tr(
                tags$td("Number of states / trajectories"),
                tags$td(
                  "“auto” compares 2–6 candidates and fits ",
                  "the recommendation (never a group under 5% of ",
                  "observations); a number fits exactly that count; ",
                  "labels imply the count."
                )
              ),
              tags$tr(
                tags$td("State method"),
                tags$td(
                  "Latent profile analysis (default; recommended by BIC) ",
                  "or k-means / PAM / hierarchical (recommended by ",
                  "silhouette)."
                )
              ),
              tags$tr(
                tags$td("Sequence distance + clustering"),
                tags$td(
                  "How sequences are compared and grouped; LCS + Ward ",
                  "follows the VaSSTra chapter, Hamming + PAM the ",
                  "Nestimate default."
                )
              ),
              tags$tr(
                tags$td("Positive / negative states"),
                tags$td(
                  "Enable the time-weighted integrative-potential and ",
                  "negative-exposure indices in the description; choices ",
                  "appear after states have labels."
                )
              )
            )
          )
        ),
        tabPanel(
          "Help",
          div(
            class = "help-section",
            h4("The four steps"),
            tags$table(
              class = "measure-table",
              tags$tr(tags$th("Step"), tags$th("Question it answers")),
              tags$tr(
                tags$td("1. States"),
                tags$td(
                  "Which few profiles summarize the multivariate ",
                  "observations at each time point?"
                )
              ),
              tags$tr(
                tags$td("2. Sequences"),
                tags$td("How does each subject move through the states?")
              ),
              tags$tr(
                tags$td("3. Trajectories"),
                tags$td("Which subjects share a similar journey?")
              ),
              tags$tr(
                tags$td("4. Description"),
                tags$td(
                  "What characterizes each journey — stability, ",
                  "complexity, and time-weighted exposure to the states ",
                  "declared positive or negative?"
                )
              )
            )
          ),
          div(
            class = "help-section",
            h4("Fit indices glossary"),
            tags$table(
              class = "measure-table",
              tags$tr(tags$th("Index"), tags$th("Reading")),
              tags$tr(
                tags$td("AIC, BIC, SABIC, CAIC, KIC"),
                tags$td(
                  "Likelihood penalized for complexity; lower is better; ",
                  "compare only among LPA candidates on the same data."
                )
              ),
              tags$tr(
                tags$td("AWE, CLC, ICL"),
                tags$td(
                  "Criteria that additionally punish fuzzy ",
                  "classification; lower is better; ICL ≈ BIC + ",
                  "classification entropy."
                )
              ),
              tags$tr(
                tags$td("Entropy"),
                tags$td(
                  "Normalized classification certainty (0–1, higher ",
                  "is better); summarizes the whole posterior matrix."
                )
              ),
              tags$tr(
                tags$td("prob_min / prob_max"),
                tags$td(
                  "Smallest and largest average posterior probability of ",
                  "assigned members; prob_min guards the weakest class, ",
                  "values ≥ 0.8 are conventionally acceptable."
                )
              ),
              tags$tr(
                tags$td("Silhouette"),
                tags$td(
                  "Separation vs. cohesion (−1 to 1, higher is ",
                  "better); the one diagnostic comparable across all ",
                  "methods and both clustering steps."
                )
              ),
              tags$tr(
                tags$td("Mean within-distance"),
                tags$td(
                  "Average pairwise distance inside a trajectory; ",
                  "comparable only within one sequence distance."
                )
              )
            )
          ),
          div(
            class = "help-section",
            h4("Sequence distances"),
            tags$table(
              class = "measure-table",
              tags$tr(tags$th("Distance"), tags$th("Best for")),
              tags$tr(
                tags$td("hamming"),
                tags$td(
                  "Position-wise disagreement; timing matters, ",
                  "sequences share one time axis."
                )
              ),
              tags$tr(
                tags$td("lcs"),
                tags$td(
                  "Longest common subsequence; order matters more than ",
                  "exact timing (the chapter's choice)."
                )
              ),
              tags$tr(
                tags$td("osa / lv / dl"),
                tags$td("Edit distances allowing shifts and swaps.")
              ),
              tags$tr(
                tags$td("qgram / cosine / jaccard / jw"),
                tags$td(
                  "Composition-oriented distances; use when state mix ",
                  "matters more than order."
                )
              )
            )
          ),
          div(
            class = "app-footer",
            HTML(paste0(
              "Method: Saqr &amp; López-Pernas, ",
              "<a href='https://lamethods.org/book1/chapters/",
              "ch11-vasstra/ch11-vasstra.html' target='_blank'>",
              "VaSSTra chapter</a>, <i>Learning Analytics Methods and ",
              "Tutorials</i>. Sequence engine: Nestimate. ",
              "<a href='https://saqr.me' target='_blank'>saqr.me</a> · ",
              "<a href='https://sonsoles.me' target='_blank'>sonsoles.me</a>"
            ))
          )
        ),
        tabPanel(
          "Summary",
          br(),
          uiOutput("decisions_ui"),
          verbatimTextOutput("fit_print"),
          h4("Trajectory summary"),
          DT::DTOutput("trajectory_summary")
        ),
        tabPanel(
          "States",
          br(),
          selectInput(
            "state_plot_type",
            "View",
            choices = c("all", "profile", "bars", "heatmap", "sizes"),
            selected = "all",
            width = "220px"
          ),
          plotOutput("state_plot", height = "620px"),
          h4("State profiles"),
          DT::DTOutput("state_profiles")
        ),
        tabPanel(
          "Sequences",
          br(),
          selectInput(
            "sequence_plot_type",
            "View",
            choices = c("distribution", "index", "heatmap"),
            selected = "distribution",
            width = "220px"
          ),
          plotOutput("sequence_plot", height = "560px")
        ),
        tabPanel(
          "Trajectories",
          br(),
          selectInput(
            "trajectory_plot_type",
            "View",
            choices = c("index", "distribution", "heatmap"),
            selected = "index",
            width = "220px"
          ),
          plotOutput("trajectory_plot", height = "560px"),
          h4("Per-subject indices"),
          DT::DTOutput("subject_indices")
        ),
        tabPanel(
          "Evaluation",
          br(),
          plotOutput("evaluation_plot", height = "680px"),
          verbatimTextOutput("evaluation_print")
        ),
        tabPanel(
          "Fit indices",
          br(),
          fluidRow(
            column(
              width = 4,
              radioButtons(
                "indices_step",
                "Clustering",
                choices = c("states", "trajectories"),
                inline = TRUE
              )
            ),
            column(
              width = 4,
              checkboxInput(
                "indices_compare",
                "Compare all candidate counts",
                value = TRUE
              )
            )
          ),
          DT::DTOutput("fit_indices_table")
        ),
        tabPanel(
          "Export",
          br(),
          div(
            class = "help-section",
            h4("Tidy tables"),
            p(
              "Each download is one tidy CSV at a fixed analysis unit, ",
              "ready for modeling or reporting."
            ),
            downloadButton("dl_subject", "Subject indices"),
            downloadButton("dl_observation", "Observations"),
            downloadButton("dl_state_profiles", "State profiles"),
            downloadButton("dl_trajectory", "Trajectory summary"),
            downloadButton("dl_indices", "Fit index comparison")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  fit_rv <- reactiveVal(NULL)
  decisions_rv <- reactiveVal(character(0))

  data_loaded <- reactive({
    if (identical(input$data_source, "engagement")) {
      return(engagement)
    }
    req(input$file)
    tryCatch(
      utils::read.csv(input$file$datapath, stringsAsFactors = FALSE),
      error = function(error) {
        showNotification(conditionMessage(error), type = "error",
                         duration = 8)
        NULL
      }
    )
  })

  detected_roles <- reactive({
    data <- data_loaded()
    req(data)
    tryCatch(
      suppressMessages(
        VaSStra:::.vasstra_auto_roles(data, NULL, NULL, NULL)
      ),
      error = function(error) {
        list(
          id = names(data)[[1L]],
          time = names(data)[[min(2L, ncol(data))]],
          variables = names(data)[vapply(data, is.numeric, logical(1L))]
        )
      }
    )
  })

  output$ui_id <- renderUI({
    data <- data_loaded()
    req(data)
    selectInput("id", "Subject id", choices = names(data),
                selected = detected_roles()$id)
  })
  output$ui_time <- renderUI({
    data <- data_loaded()
    req(data)
    selectInput("time", "Time", choices = names(data),
                selected = detected_roles()$time)
  })
  output$ui_variables <- renderUI({
    data <- data_loaded()
    req(data)
    numeric_columns <- names(data)[vapply(data, is.numeric, logical(1L))]
    selectInput("variables", "Indicators", choices = numeric_columns,
                selected = detected_roles()$variables, multiple = TRUE)
  })

  observeEvent(input$state_labels, {
    labels <- .parse_labels(input$state_labels)
    if (!is.null(labels)) {
      updateSelectInput(session, "positive_states", choices = labels,
                        selected = intersect(input$positive_states, labels))
      updateSelectInput(session, "negative_states", choices = labels,
                        selected = intersect(input$negative_states, labels))
    }
  })

  fitted <- eventReactive(input$run, {
    data <- data_loaded()
    req(data, input$id, input$time, input$variables)
    if (length(input$variables) < 2L) {
      showNotification("Select at least two indicators.", type = "error")
      return(NULL)
    }
    messages <- character(0)
    result <- withProgress(message = "Running VaSStra…", value = 0.4, {
      tryCatch(
        withCallingHandlers(
          vasstra(
            data,
            id = input$id,
            time = input$time,
            variables = input$variables,
            n_states = .count_choice(input$n_states),
            n_trajectories = .count_choice(input$n_trajectories),
            state_labels = .parse_labels(input$state_labels),
            trajectory_labels = .parse_labels(input$trajectory_labels),
            state_method = input$state_method,
            lpa_model = input$lpa_model,
            dissimilarity = input$dissimilarity,
            cluster_method = input$cluster_method,
            positive_states = if (length(input$positive_states) > 0L) {
              input$positive_states
            } else {
              NULL
            },
            negative_states = if (length(input$negative_states) > 0L) {
              input$negative_states
            } else {
              NULL
            },
            seed = input$seed
          ),
          message = function(condition) {
            messages <<- c(messages, conditionMessage(condition))
            invokeRestart("muffleMessage")
          }
        ),
        error = function(error) {
          showNotification(conditionMessage(error), type = "error",
                           duration = 10)
          NULL
        }
      )
    })
    decisions_rv(trimws(messages))
    result
  })

  observeEvent(fitted(), {
    fit <- fitted()
    req(fit)
    fit_rv(fit)
    labels <- fit$sequences$states
    updateSelectInput(session, "positive_states", choices = labels,
                      selected = fit$description$settings$positive_states)
    updateSelectInput(session, "negative_states", choices = labels,
                      selected = fit$description$settings$negative_states)
    updateTabsetPanel(session, "tabs", selected = "Summary")
  })

  observeEvent(input$rename, {
    fit <- fit_rv()
    req(fit)
    state_labels <- .parse_labels(input$rename_states)
    trajectory_labels <- .parse_labels(input$rename_trajectories)
    if (is.null(state_labels) && is.null(trajectory_labels)) {
      showNotification("Supply new state or trajectory labels.",
                       type = "warning")
      return(invisible(NULL))
    }
    renamed <- tryCatch(
      set_labels(fit, states = state_labels,
                 trajectories = trajectory_labels),
      error = function(error) {
        showNotification(conditionMessage(error), type = "error",
                         duration = 8)
        NULL
      }
    )
    if (!is.null(renamed)) {
      fit_rv(renamed)
      updateSelectInput(
        session, "positive_states",
        choices = renamed$sequences$states,
        selected = renamed$description$settings$positive_states
      )
      updateSelectInput(
        session, "negative_states",
        choices = renamed$sequences$states,
        selected = renamed$description$settings$negative_states
      )
      showNotification("Labels applied to every table and plot.",
                       type = "message")
    }
  })

  evaluation <- reactive({
    fit <- fit_rv()
    req(fit)
    evaluate(fit)
  })

  output$decisions_ui <- renderUI({
    decisions <- decisions_rv()
    if (length(decisions) == 0L) {
      return(NULL)
    }
    div(
      class = "decisions-box",
      strong("Decisions: "),
      paste(decisions, collapse = "\n")
    )
  })

  output$fit_print <- renderPrint({
    fit <- fit_rv()
    req(fit)
    print(fit)
  })

  output$trajectory_summary <- DT::renderDT({
    fit <- fit_rv()
    req(fit)
    summary_table <- as.data.frame(fit, unit = "trajectory")
    numeric_columns <- vapply(summary_table, is.numeric, logical(1L))
    summary_table[numeric_columns] <- lapply(
      summary_table[numeric_columns],
      round,
      digits = 3L
    )
    DT::datatable(summary_table, rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE))
  })

  output$state_plot <- renderPlot({
    fit <- fit_rv()
    req(fit, !is.null(fit$states))
    plot(fit, which = "states", type = input$state_plot_type)
  })

  output$state_profiles <- DT::renderDT({
    fit <- fit_rv()
    req(fit, !is.null(fit$states))
    profiles <- as.data.frame(fit, unit = "state_profile")
    numeric_columns <- vapply(profiles, is.numeric, logical(1L))
    profiles[numeric_columns] <- lapply(profiles[numeric_columns], round,
                                        digits = 3L)
    DT::datatable(profiles, rownames = FALSE, filter = "top",
                  options = list(pageLength = 24, scrollX = TRUE))
  })

  output$sequence_plot <- renderPlot({
    fit <- fit_rv()
    req(fit)
    plot(fit, which = "sequences", type = input$sequence_plot_type)
  })

  output$trajectory_plot <- renderPlot({
    fit <- fit_rv()
    req(fit)
    plot(fit, type = input$trajectory_plot_type)
  })

  output$subject_indices <- DT::renderDT({
    fit <- fit_rv()
    req(fit)
    indices <- as.data.frame(fit)
    numeric_columns <- vapply(indices, is.numeric, logical(1L))
    indices[numeric_columns] <- lapply(indices[numeric_columns], round,
                                       digits = 3L)
    DT::datatable(indices, rownames = FALSE, filter = "top",
                  options = list(pageLength = 15, scrollX = TRUE))
  })

  output$evaluation_plot <- renderPlot({
    plot(evaluation())
  })

  output$evaluation_print <- renderPrint({
    print(evaluation())
  })

  fit_indices_table <- reactive({
    fit <- fit_rv()
    req(fit)
    tryCatch(
      as.data.frame(fit_indices(
        fit,
        step = input$indices_step,
        compare = input$indices_compare
      )),
      error = function(error) {
        showNotification(conditionMessage(error), type = "error")
        NULL
      }
    )
  })

  output$fit_indices_table <- DT::renderDT({
    table <- fit_indices_table()
    req(table)
    numeric_columns <- vapply(table, is.numeric, logical(1L))
    table[numeric_columns] <- lapply(table[numeric_columns], round,
                                     digits = 3L)
    DT::datatable(table, rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE))
  })

  .download_csv <- function(name, builder) {
    downloadHandler(
      filename = function() paste0("vasstra_", name, "_", Sys.Date(), ".csv"),
      content = function(file) {
        fit <- fit_rv()
        req(fit)
        utils::write.csv(builder(fit), file, row.names = FALSE)
      }
    )
  }

  output$dl_subject <- .download_csv("subjects", function(fit) {
    as.data.frame(fit)
  })
  output$dl_observation <- .download_csv("observations", function(fit) {
    as.data.frame(fit, unit = "observation")
  })
  output$dl_state_profiles <- .download_csv("state_profiles", function(fit) {
    as.data.frame(fit, unit = "state_profile")
  })
  output$dl_trajectory <- .download_csv("trajectories", function(fit) {
    as.data.frame(fit, unit = "trajectory")
  })
  output$dl_indices <- .download_csv("fit_indices", function(fit) {
    as.data.frame(fit_indices(fit, step = "states", compare = TRUE))
  })
}

shinyApp(ui, server)
