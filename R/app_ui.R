#' App UI
#'
#' @noRd

# File size limit 50 MB
options(shiny.maxRequestSize=50*1024^2)


## All ui components
ui_upload_file <- sidebarLayout(
  sidebarPanel(
    fileInput("file_GLM", "Upload Your Data",
              accept = c("csv", ".csv", "xlsx", ".xlsx"))
  ),
  mainPanel(
    uiOutput("wizard_ui"),
    uiOutput("summary_ui"),
    uiOutput("preview_ui")
  )
)

ui_data_vis <- navset_pill_list(
  nav_panel("Heatmap",
            radioButtons("data_for_heatmap",
                         tags$span("Choose the data for the plot",
                                   shinyBS::bsButton("info_data_for_heatmap", label = "", icon = icon("circle-info"), style = "default", size = "extra-small")),
                         choices = c("Original Data" = "orig_data",
                                     "Normalized Data" = "norm_data")),
            shinyBS::bsPopover(id = "info_data_for_heatmap",
                      title = "",
                      content = "Choose data for plot as desired. Sometimes it makes more sense to visualize the raw values between conditions. Note that if the raw data is highly skewed, the heatmap will be less informative, in that case, log transformation is automatically applied for the purpose of visualization",
                      placement = "right",
                      trigger = "hover",
                      options = list(container = "body")),
            radioButtons("show_rownames_heatmap",
                         "Show Rownames",
                         choices = c("Yes" = "yes",
                                     "No" = "no"),
                         selected = "no"),
            radioButtons("cluster_row_heatmap",
                         "Cluster Row (features)",
                         choices = c("Yes" = "yes",
                                     "No" = "no"),
                         selected = "no"),
            radioButtons("cluster_col_heatmap",
                         "Cluster Column (samples)",
                         choices = c("Yes" = "yes",
                                     "No" = "no"),
                         selected = "no"),
            actionButton("submit_heatmap", "Submit"),
            fluidRow(
              column(8, withSpinner(plotOutput("heatmap", height = "1000px")))
            )
  ),
  nav_panel("Barplot",
            uiOutput("select_data_barplot"),
            fluidRow(
              column(6, withSpinner(plotOutput("barplot")))
            )
  ),
  nav_panel("Boxplot",
            uiOutput("select_data_boxplot"),
            fluidRow(
              column(6, withSpinner(plotOutput("boxplot")))
            )
  ),
  nav_panel("Linegraph",
            uiOutput("select_data_linegraph"),
            fluidRow(
              column(6, withSpinner(plotOutput("linegraph")))
            )
  )
)

ui_fit_GLM <- sidebarLayout(
  sidebarPanel(
    radioButtons("center_time",
                 tags$span("Center Time for the model?",
                           shinyBS::bsButton("info_center_time", label = "", icon = icon("circle-info"), style = "default", size = "extra-small")),
                 choices = c("Use centered Time" = "center",
                             "Use original Time" = "no_center")),
    # https://stackoverflow.com/questions/70679604/info-icon-next-to-label-of-a-selectinput-in-shiny
    shinyBS::bsPopover(id = "info_center_time",
              title = "",
              content = "It is recommanded to use centered time to reduce multicollinearity",
              placement = "right",
              trigger = "hover",
              options = list(container = "body")),
    uiOutput("select_ref_cat"),
    numericInput("n_perm",
                 tags$span("Number of permutations",
                           shinyBS::bsButton("info_nperm", label = "", icon = icon("circle-info"), style = "default", size = "extra-small")),
                 value = 100, min = 100, max = 1000, step = 100),
    shinyBS::bsPopover(id = "info_nperm",
              title = "",
              content = "More permutation takes more time",
              placement = "right",
              trigger = "hover",
              options = list(container = "body")),
    actionButton("submit_fit_GLM", "Submit")
  ),
  mainPanel(
    withSpinner(tableOutput("GLM_results")),
    uiOutput("download_full_table_ui")
  )
)

ui_GLM_vis <- navset_pill_list(
  nav_panel("3D Plot",
            withSpinner(plotlyOutput("3D_plot", height = "1000px"))
  ),
  nav_panel("Volcano Plot & Pie Chart",
            h3("Piechart"),
            fluidRow(
              column(6, withSpinner(plotOutput("piechart")))
            ),
            h3("Volcano Plot"),
            uiOutput("select_cutoff_volcano"),
            fluidRow(
              column(6, withSpinner(plotOutput("volcano", height = "800px")))
            )
  )
)


# Define UI for application
app_ui <- function() {
  fluidPage(
  useShinyjs(),
  #theme = bslib::bs_theme(bootswatch="lumen"),
  tags$head(
    tags$style(HTML("
    .modal-dialog {
      position: fixed;
      top: 50% !important;
      left: 50% !important;
      transform: translate(-50%, -50%) !important;
      margin: 0 !important;
    }

    .shiny-input-container label {
      white-space: nowrap;
    }
  "))
  ),

  navbarPage(
    title = "ShinyGLM",
    tabPanel("Get Started",
             navlistPanel(
               tabPanel("About ShinyGLM",
                        withMathJax(),
                        h2("What does ShinyGLM do?"),
                        p("ShinyGLM is a user interface built with Shiny to run generalized linear model (GLM) on temporal data. This app has the following components:"),
                        p(" - data visualization"),
                        p(" - model fitting, feature ranking and trajectory classification"),
                        p(" - result visualization"),
                        h3("Model Formulation"),
                        p("Currently, a quadratic model is assumed for data trend over time: $$y=\\beta_0+\\beta_1I+\\beta_2t+\\beta_3It+\\beta_4t^2+\\beta_5It^2+\\epsilon$$"),
                        p("where \\(I\\) is the condition indicator (0 for the reference condition, 1 for the other condition). The main effect of \\(I\\) allows the condition of interest differs from the reference condition in intercept. Interaction terms \\(It, It^2\\) allow the two trajectories deviate in slope and curvature."),
                        br(),
                        h3("Quantify Differences"),
                        p("Fitted models will be used to generate predicted expression values across a dense grid of time points for both conditions. For each feature, a root mean squared difference (RMSD) is calculated:"),
                        p("$$RMSD=\\sqrt{\\frac{1}{n}\\sum_i^n{(\\frac{\\Delta_i}{SE(\\Delta_i)})^2}}$$"),
                        p("where \\(\\Delta_i\\) is the predicted difference between the two conditions at the \\(i\\)-th time point"),
                        p("To prioritize features whose trajectories are well explained by the model, RMSD values are weighted by the model's adjusted \\(R^2: weighted\\;RMSD=RMSD\\times adjusted\\;R^2\\)"),
                        br(),
                        h3("Feature Ranking and Significance"),
                        p("Features are ranked by weighted RMSD, with higher values indicating greater divergence in trajectory between conditions. To assess statistical significance, permutation testing is employed. This will generate empirical p-values for each feature."),
                        br(),
                        h3("Trajectory Classification"),
                        p("Temporal dynamics can manifest in diverse forms - for instance, linear increases, early peaks followed by decline, or transient dips. To interpret these patterns systematically, features are classfied into five trajectory categories based on the structure and alignment of fitted models."),
                        p(" 1. Linear Concordance: Both conditions exhibit significant linear trends and non-significant curvature, with slopes in the same direction."),
                        p(" 2. Linear Discordance: As above, but with slopes in opposing directions."),
                        p(" 3. Polynomial Concordance: Both conditions show significant curvature, and the sign of curvature is the same (e.g., both convex or both concave)."),
                        p(" 4. Polynomial Discordance: Both conditions show significant curvature, but in opposite directions."),
                        p(" 5. Cross-Model Discordance: All other cases, including mismatched model structures (e.g., one condition is linear and the other polynomial) or ambiguous patterns."),
                        br(),
                        p("For details of this tool: ",
                          a(href="",
                            "Link to the paper, if available",
                            target="_blank"))
               ),
               tabPanel("How to use this app",
                        h2("Data"),
                        p("Temporal omics data can be used to explore trajectory change over time."),
                        p("The upload data should either be a Excel Worksheet (.xlsx) or a Comma-Separate File (.csv) and
                          contains the following columns:"),
                        p("ID column: ID that can uniquely identify a feature. Column name for this should be \"ID\"."),
                        p("Measurements: name of measurement columns should follow \"Condition_Time_Replicate\" (e.g. WT_10_R1)
                          where"),
                        p(" - Condition: treatment/genotype"),
                        p(" - Time: time point of the samples. Required to be numbers in the same unit"),
                        p(" - Replicate: replicate of each sample"),
                        p("If additional columns are present in the data, they will be removed."),
                        p("To fit GLM to the data, make sure there are 2 conditions and at least 3 time points. More time points is recommmended."),
                        p("Below is an example of the required data format:"),
                        img(src="www/data_example.PNG", width='100%'),
                        br(),br(),
                        h2("Data Upload and Preparation"),
                        p("Upload data in the \"Import Data\" tab, follow the wizard to complete data check, filter, and normalization steps.
                          After submitting for desired normalization, several plots will be available to see the effect on all the data preparation steps.
                          Make sure to hit \"Finalize\" button to pass the normalized data to the next step. You will see a Data Preview table.
                          The processed data can be downloaded."),
                        img(src="www/data_prepare1.PNG", width='50%'),
                        br(),
                        h2("Data Visualization"),
                        p("Before model fitting, users can choose to visualize the data in multiple ways: heatmap, barplot, boxplot, and linegraph"),
                        br(),
                        h2("Model Fitting"),
                        p("Fit GLM model with desired time transformation and number of permutation to get significance on RMSD estimates."),
                        br(),
                        h2("Results Visualization"),
                        p("Available outputs:"),
                        p(" 1. 3D plot on model coefficients"),
                        p(" 2. Piechart on trajectory classification"),
                        p(" 3. Volcano plot on weighted RMSD vs adjusted p-value")
               ),
               widths = c(3, 9)
             )
    ),
    tabPanel("Modeling",
             tabsetPanel(
               id = "GLM_tabs",
               tabPanel("Import Data",
                        ui_upload_file),
               tabPanel("Visualize Data",
                        ui_data_vis,
                        value = "tab_data_vis"),
               tabPanel("Fit a GLM Model",
                        ui_fit_GLM,
                        value = "tab_fit_GLM"),
               tabPanel("Visualize GLM Results",
                        ui_GLM_vis,
                        value = "tab_GLM_vis")
             )
    )
  )# navbarpage
) # fluidPage
}
