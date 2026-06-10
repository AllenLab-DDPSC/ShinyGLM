#' App server
#'
#' @noRd

app_server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })

  # disable other tabs initially
  disable(selector = "a[data-value=tab_data_vis]")
  disable(selector = "a[data-value=tab_fit_GLM]")
  disable(selector = "a[data-value=tab_GLM_vis]")

  rv <- reactiveValues()
  qc_finalized <- reactiveVal(FALSE)

  # --- Data Upload ---
  observeEvent(input$file_GLM, {
    ext <- tools::file_ext(input$file_GLM$name)

    if (ext == "csv") {
      rv$data <- read.csv(input$file_GLM$datapath)
      # remove extra whitespace in column names
      colnames(rv$data) <- trimws(colnames(rv$data))
    } else if (ext == "xlsx") {
      rv$data <- readxl::read_excel(input$file_GLM$datapath)
      colnames(rv$data) <- trimws(colnames(rv$data))
    } else {
      rv$data <- NULL
      showModal(modalDialog(
        title = "File Format Error",
        "Data cannot be recognized. Please upload a xlsx or csv file.",
        footer = modalButton("OK"),
        easyClose = FALSE
      ))
    }
  })


  # --- Wizard only after a valid file is present ---
  output$wizard_ui <- renderUI({
    req(input$file_GLM)
    req(tools::file_ext(input$file_GLM$datapath) %in% c("csv", "xlsx"))
    tagList(h3("Data Check and Prepration"),
    shinyBS::bsCollapse(id = "wizard", open = "step1",
               shinyBS::bsCollapsePanel(
                 title = "Step 1: Data Format Check",
                 uiOutput("qc_step1"),
                 actionButton("next1", "Next"),
                 style = "info", value = "step1"
               ),
               shinyBS::bsCollapsePanel(
                 title = "Step 2: Remove Unused Columns",
                 uiOutput("qc_step2"),
                 actionButton("next2", "Next"),
                 actionButton("back2", "Back"),
                 style = "info", value = "step2"
               ),
               shinyBS::bsCollapsePanel(
                 title = "Step 3: Data Filtering",
                 uiOutput("qc_step3"),
                 actionButton("next3", "Next"),
                 actionButton("back3", "Back"),
                 style = "info", value = "step3"
               ),
               shinyBS::bsCollapsePanel(
                 title = "Step 4: Data Imputation and Normalization",
                 uiOutput("qc_step4"),
                 actionButton("finalize", "Finalize", disabled = TRUE),
                 actionButton("back4", "Back"),
                 style = "info", value = "step4"
               )
    ))
  })

  # --- Step 1: Critical QC ---
  output$qc_step1 <- renderUI({
    req(input$file_GLM)

    messages <- list()
    stop_here <- FALSE

    if (!"ID" %in% colnames(rv$data)) { messages <- c(messages, "\u2717 ID column missing"); stop_here <- TRUE }
    else { messages <- c(messages, "\u2713 ID column present") }

    if (!stop_here && anyNA(rv$data[["ID"]])) { messages <- c(messages, "\u2717 Missingness in ID column"); stop_here <- TRUE }
    else if (!stop_here) { messages <- c(messages, "\u2713 ID column complete") }

    if (!stop_here && anyDuplicated(rv$data[["ID"]])) { messages <- c(messages, "\u2717 ID not unique"); stop_here <- TRUE }
    else if (!stop_here) { messages <- c(messages, "\u2713 ID column unique") }

    if (!stop_here && !any(setdiff(colnames(rv$data), "ID") %like% "_.+_")) { messages <- c(messages, "\u2717 Measurement columns missing"); stop_here <- TRUE }
    else if (!stop_here) { messages <- c(messages, "\u2713 Measurement columns present") }

    rv$times <- as.numeric(sapply(strsplit(colnames(rv$data)[colnames(rv$data) %like% "_.+_"], '[_]' ), `[` , 2))
    rv$conditions <- unique(sapply(strsplit(colnames(rv$data)[colnames(rv$data) %like% "_.+_"], '[_]' ), `[` , 1))

    if (!stop_here && length(rv$conditions) != 2) { messages <- c(messages, "\u2717 Need 2 conditions") ; stop_here <- TRUE }
    else if (!stop_here) { messages <- c(messages, "\u2713 2 conditions in the data")}

    if (!stop_here && anyNA(rv$times)) { messages <- c(messages, "\u2717 Not all Time are numeric") ; stop_here <- TRUE }
    else if (!stop_here) { messages <- c(messages, "\u2713 Time in numeric")}

    if (!stop_here && length(rv$times) < 3) { messages <- c(messages, "\u2717 Less than 3 time points. Unable to fit a quadratic regression") ; stop_here <- TRUE }
    else if (!stop_here && length(rv$times) == 3) { messages <- c(messages, "\u26A0 Only 3 time points in the data. Not recommanded")}
    else if (!stop_here) { messages <- c(messages, "\u2713 Enough Time points")}

    if (!stop_here) {
      rv$step1_pass <- TRUE
    } else {
      rv$step1_pass <- FALSE
      disable("next1")
    }

    # Note: when editing, keep in mind that these icons do not align with the cursor
    if (rv$step1_pass) { messages <- c(messages, "\u2705 Critical QC Passed") }
    else { messages <- c(messages, "\u274C Critical QC Failed. Please make sure data requirements are met and reupload your file.")}

    tagList(lapply(messages, function(m) p(m)))
  })

  # Navigate Step 1 --> Step 2
  observeEvent(input$next1, {
    req(rv$step1_pass)
    shinyBS::updateCollapse(session, "wizard", open = "step2")
    # Do not put manipulation of data which renderUI depends on in the renderUI
    rv$n_extra <- length(setdiff(colnames(rv$data), c("ID", colnames(rv$data)[grepl("_.+_", colnames(rv$data))])))
    rv$data <- rv$data %>%
      mutate(across(matches("_.+_"), ~ suppressWarnings(as.numeric(.)))) %>%
      select(ID, matches("_.+_"))
  })

  # --- Step 2: Column Removal / Conversion ---
  output$qc_step2 <- renderUI({
    req(rv$step1_pass, rv$data)
    if (rv$n_extra > 0) { p(paste("\u26A0", rv$n_extra, "additional column(s) detected and removed")) }
    else { p("No additional columns found. Proceed to the next step") }
  })

  observeEvent(input$next2, {
    shinyBS::updateCollapse(session, "wizard", open = "step3")
  })
  observeEvent(input$back2, {
    shinyBS::updateCollapse(session, "wizard", open = "step1")
  })

  # --- Step 3: Optional Filtering ---
  output$qc_step3 <- renderUI({
    req(rv$data)

    rv$zero_features <- rowMeans(rv$data[grepl("_.+_", colnames(rv$data))] == 0, na.rm = TRUE) > 0.5
    rv$NA_features <- rowMeans(is.na(rv$data[grepl("_.+_", colnames(rv$data))])) > 0.5
    rv$num_FilterOutFeatures <- sum(rv$zero_features | rv$NA_features)

    if (identical(rv$num_FilterOutFeatures > 0, TRUE)) {
      tagList(
      p(paste(rv$num_FilterOutFeatures, "feature(s) with >50% zeros or NAs detected")),
      radioButtons("filter_choice",
                   tags$span("What to do?",
                             shinyBS::bsButton("info_filter_choice", label = "", icon = icon("circle-info"), style = "default", size = "extra-small")),
                   choices = c("Remove problematic features" = "remove",
                               "Keep all features" = "keep")),
      shinyBS::bsPopover(id = "info_filter_choice",
                title = "",
                content = "It is recommanded to remove features with excessive missingness or zeros, as the GLM fit will not have a complete set of points to rely on",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")),
      actionButton("submit_filter", "Submit")
      )
    }else {tagList(p("No features need filtering"))}
  })

  # Apply filtering
  observeEvent(input$submit_filter, {
    req(rv$num_FilterOutFeatures, rv$data)
    if (input$filter_choice == "remove") {
      rv$data <- rv$data[!(rv$zero_features | rv$NA_features), ,drop = FALSE]
      showNotification(paste(rv$num_FilterOutFeatures, "feature(s) removed"), type = "message")
    }
  })

  observeEvent(input$next3, {
    shinyBS::updateCollapse(session, "wizard", open = "step4")
  })
  observeEvent(input$back3, {
    shinyBS::updateCollapse(session, "wizard", open = "step2")
  })

  # --- Step 4: Optional Normalization ---
  output$qc_step4 <- renderUI({
    req(rv$num_FilterOutFeatures, rv$data)

    ui_elements <- list()

    # Sample normalization selection
    ui_elements <- c(ui_elements, list(
      radioButtons("sample_norm_choice",
                   tags$span("Sample Normalization Method (Column-wise)",
                             shinyBS::bsButton("info_sample_norm_choice", label = "", icon = icon("circle-info"), style = "default", size = "extra-small")),
                   choices = c("None" = "no_sample_norm",
                               "Median normalization" = "median_norm")),
      shinyBS::bsPopover(id = "info_sample_norm_choice",
                title = "",
                content = "Sample normalization is helpful to reduce technical/systematic variation",
                placement = "right",
                trigger = "hover",
                options = list(container = "body"))
    ))

    # Log transformation
    ui_elements <- c(ui_elements, list(
      radioButtons("log_choice",
                   tags$span("Log Transformation",
                             shinyBS::bsButton("info_log_choice", label = "", icon = icon("circle-info"),
                                      style = "default", size = "extra-small")),
                   choices = c("Ln (Natural log)" = "ln",
                               "Log2" = "log2",
                               "Log10" = "log10",
                               "None" = "no_log")),
      shinyBS::bsPopover(id = "info_log_choice",
                title = "",
                content = "Log transformation is neccessary if the data is skewed, which is often the case in omics data, because GLM assumes the data is normally distributed",
                placement = "right",
                trigger = "hover",
                options = list(container = "body"))
    ))

    # Imputation selection
    if (anyNA(rv$data)) {
      ui_elements <- c(ui_elements, list(
        p("Missing value(s) detected"),
        radioButtons("impute_choice",
                     tags$span("Imputation Method",
                               shinyBS::bsButton("info_impute_choice", label = "", icon = icon("circle-info"), style = "default", size = "extra-small")),
                     choices = c("No imputation" = "no_impute",
                                 "Half nonzero min" = "halfmin_impute",
                                 "Nonzero min" = "min_impute")),

        shinyBS::bsPopover(id = "info_impute_choice",
                  title = "",
                  content = "Do you want to impute NAs? 0s will not be changed",
                  placement = "right",
                  trigger = "hover",
                  options = list(container = "body"))
      ))
    } else {
      ui_elements <- c(ui_elements, list(
        p("No missing values or zeros in the data"),
        hidden(radioButtons("impute_choice", NULL,
                            choices = c("No imputation" = "no_impute"),
                            selected = "no_impute"))
      ))
    }

    # Feature normalization
    ui_elements <- c(ui_elements, list(
      radioButtons("feature_norm_choice",
                   tags$span("Feature Normalization Method (Row-wise)",
                             shinyBS::bsButton("info_feature_norm_choice", label = "", icon = icon("circle-info"),
                                      style = "default", size = "extra-small")),
                   choices = c("None" = "no_feature_norm",
                               "Z-score transformation" = "zscore_norm",
                               "Pareto transformation" = "pareto_norm",
                               "Min-max scaling" = "minmax_norm")),
      shinyBS::bsPopover(id = "info_feature_norm_choice",
                title = "",
                content = "In general, one should not apply feature normalization, as this operation will squeeze the differences between conditions (for example, wild-type vs mutant) to make each feature comparable. However, it can be useful when difference in level between conditions does not matter",
                placement = "right",
                trigger = "hover",
                options = list(container = "body"))
    ))

    # Reference category
    ui_elements <- c(ui_elements, list(
      radioButtons("ref_cat",
                   tags$span("Select Reference Condition",
                             shinyBS::bsButton("info_ref_cat", label = "", icon = icon("circle-info"),
                                      style = "default", size = "extra-small")),
                   choices = rv$conditions),
      shinyBS::bsPopover(id = "info_ref_cat",
                title = "",
                content = "This is mainly for plotting purpose. The reference condition will be placed at left for an easy comparison",
                placement = "right",
                trigger = "hover",
                options = list(container = "body"))
    ))

    # Final submit button
    ui_elements <- c(ui_elements, list(actionButton("submit_norm", "Submit")))

    do.call(tagList, ui_elements)
  })


  # Apply selected normalization
  observeEvent(input$submit_norm, {
    req(rv$data)
    # get long format of data before normalization
    rv$data_long <- rv$data %>%
      pivot_longer(cols = -c(ID),
                   names_to = c("Condition", "Time", "Replicate"),
                   names_sep = "_",
                   values_to = "Expression") %>%
      mutate(Condition = factor(Condition, levels = c(input$ref_cat, setdiff(rv$conditions, input$ref_cat))),
             Sample = paste0(Condition, "_", Time, "_", Replicate))
    # get avg format of data before normalization (for different plots)
    rv$data_avg <- rv$data_long %>%
      group_by(ID, Condition, Time) %>%
      summarise(mean = mean(Expression, na.rm = TRUE),
                se = sd(Expression, na.rm = T) / sqrt(length(na.omit(.))), .groups = "drop")

    ## Order: sample norm - log - imputation - feature norm
    # Sample normalization
    if (input$sample_norm_choice == "no_sample_norm") {
      rv$data_after_sample_norm <- rv$data
    }else if(input$sample_norm_choice == "median_norm"){
      data_mat <- rv$data %>% select(-ID)
      max_median <- max(apply(data_mat, 2, median, na.rm = TRUE))
      med_norm_data <- sweep(data_mat, 2,
                             apply(data_mat, 2, function(x){median(x, na.rm = TRUE)}),
                             FUN = "/") * max_median
      rv$data_after_sample_norm <- data.frame(ID = rv$data[["ID"]], med_norm_data)
    }

    # log transformation
    if (input$log_choice == "ln"){
      rv$data_after_log <- rv$data_after_sample_norm %>%
        mutate(across(-ID, ~log1p(.)))
    }else if (input$log_choice == "log2"){
      rv$data_after_log <- rv$data_after_sample_norm %>%
        mutate(across(-ID, ~log2(.+1)))
    }else if (input$log_choice == "log10"){
      rv$data_after_log <- rv$data_after_sample_norm %>%
        mutate(across(-ID, ~log10(.+1)))
    }else if (input$log_choice == "no_log"){
      rv$data_after_log <- rv$data_after_sample_norm
    }

    # imputation
    if (input$impute_choice == "halfmin_impute"){
      rv$c <- "coalesce(Expression, 0) + min(Expression[Expression > 0], na.rm = TRUE) / 2"
    }else if (input$impute_choice == "min_impute"){
      rv$c <- "coalesce(Expression, 0) + min(Expression[Expression > 0], na.rm = TRUE)"
    }else if (input$impute_choice == "no_impute"){
      rv$c <- "Expression"
    }
    rv$data_after_impute <- rv$data_after_log %>%
      pivot_longer(cols = -c(ID),
                   names_to = c("Condition", "Time", "Replicate"),
                   names_sep = "_",
                   values_to = "Expression") %>%
      group_by(ID, Condition, Time) %>%
      mutate(Expression = !!parse_expr(rv$c)) %>%
      ungroup() %>%
      pivot_wider(names_from = c("Condition", "Time", "Replicate"),
                  names_sep = "_",
                  values_from = Expression)

    # Feature normalization
    if (input$feature_norm_choice == "no_feature_norm") {
      rv$data_after_feature_norm <- rv$data_after_impute %>% select(-ID)
    }else if(input$feature_norm_choice == "zscore_norm"){
      data_mat <- rv$data_after_impute %>% select(-ID)
      rv$data_after_feature_norm <- t(apply(data_mat, 1, function(x) {
        (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
      }))
    }else if(input$feature_norm_choice == "pareto_norm"){
      data_mat <- rv$data_after_impute %>% select(-ID)
      rv$data_after_feature_norm <- t(apply(data_mat, 1, function(x) {
        (x - mean(x, na.rm = TRUE)) / sqrt(sd(x, na.rm = TRUE))
      }))
    }else if(input$feature_norm_choice == "minmax_norm"){
      data_mat <- rv$data_after_impute %>% select(-ID)
      rv$data_after_feature_norm <- t(apply(data_mat, 1, function(x) {
        (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      }))
    }
    rv$data_after_norm <- data.frame(ID = rv$data_after_impute[["ID"]], rv$data_after_feature_norm)
    # long format of data after normalization
    rv$data_after_norm_long <- rv$data_after_norm %>%
      pivot_longer(cols = -c(ID),
                   names_to = c("Condition", "Time", "Replicate"),
                   names_sep = "_",
                   values_to = "Expression") %>%
      mutate(Condition = factor(Condition, levels = c(input$ref_cat, setdiff(rv$conditions, input$ref_cat))),
             Sample = paste0(Condition, "_", Time, "_", Replicate))
    # avg format of data after normalization
    rv$data_after_norm_avg <- rv$data_after_norm_long %>%
      group_by(ID, Condition, Time) %>%
      summarise(mean = mean(Expression, na.rm = TRUE),
                se = sd(Expression, na.rm = T) / sqrt(length(na.omit(.))), .groups = "drop")
  })

  observeEvent(input$submit_norm, {
    req(rv$data_after_norm)
    showNotification("Selected normalization done", type = "message")
  })


  # --- Summary after normalization to show before vs after ---
  output$summary_ui <- renderUI({
    req(input$submit_norm)
    tagList(
      h3("Data Summary"),
      navset_tab(nav_panel("Missingness Heatmap",
                           fluidRow(
                             column(6, withSpinner(plotOutput("missing_summary1"))),
                             column(6, withSpinner(plotOutput("missing_summary2")))
                           )),
                 nav_panel("Boxplots on Samples",
                           fluidRow(
                             column(6, withSpinner(plotOutput("boxplot_summary1"))),
                             column(6, withSpinner(plotOutput("boxplot_summary2")))
                           )),
                 nav_panel("PCA",
                           fluidRow(
                             column(6, withSpinner(plotOutput("pca_summary1"))),
                             column(6, withSpinner(plotOutput("pca_summary2")))
                           )),
                 nav_panel("Hierarchical Clustering",
                           fluidRow(
                             column(6, withSpinner(plotOutput("hc_summary1"))),
                             column(6, withSpinner(plotOutput("hc_summary2")))
                           )))
    )
  })

  output$missing_summary1 <- renderPlot({
    df <- rv$data_long %>%
      mutate(Expression = ifelse(Expression != 0, "value", "zero")) %>%
      select(ID, Sample, Expression)
    ggplot(df, aes(x = Sample, y = ID)) +
      geom_raster(aes(fill = Expression)) +
      scale_x_discrete(limits = setdiff(colnames(rv$data), "ID")) +
      scale_fill_manual(values = c("value" = "grey90", "zero" = "yellow", "NA" = "black")) +
      labs(x = "", y = "Rows / Observations",
           fill = "Value", title = "Missingness (Before Imputation)") +
      theme_classic() +
      theme(text = element_text(size = 16),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  })

  output$missing_summary2 <- renderPlot({
    df <- rv$data_after_norm_long %>%
      mutate(Expression = ifelse(Expression != 0, "value", "zero")) %>%
      select(ID, Sample, Expression)
    ggplot(df, aes(x = Sample, y = ID)) +
      geom_raster(aes(fill = Expression)) +
      scale_x_discrete(limits = setdiff(colnames(rv$data), "ID")) +
      scale_fill_manual(values = c("value" = "grey90", "zero" = "yellow", "NA" = "black")) +
      labs(x = "", y = "Rows / Observations",
           fill = "Value", title = "Missingness (After Imputation)") +
      theme_classic() +
      theme(text = element_text(size = 16),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  })

  output$boxplot_summary1 <- renderPlot({
    ggplot(rv$data_long, aes(x = Sample, y = Expression)) +
      geom_boxplot() +
      labs(y = "Raw Value", title = "Before Normalization") +
      scale_x_discrete(limits = setdiff(colnames(rv$data), "ID")) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text = element_text(colour="black", size = 12),
            axis.title = element_text(size = 12))
  })

  output$boxplot_summary2 <- renderPlot({
    ggplot(rv$data_after_norm_long, aes(x = Sample, y = Expression)) +
      geom_boxplot() +
      labs(y = "Transformed Value", title = "After Normalization") +
      scale_x_discrete(limits = setdiff(colnames(rv$data), "ID")) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text = element_text(colour="black", size = 12),
            axis.title = element_text(size = 12))
  })

  output$pca_summary1 <- renderPlot({
    expression_mat <- rv$data %>% column_to_rownames("ID")
    expression_mat <- expression_mat[apply(expression_mat, 1, function(feature) var(feature, na.rm = TRUE) > 0),]
    pca <- prcomp(t(na.omit(expression_mat)), center = T, scale. = T)
    pca_var <- pca$sdev^2 / sum(pca$sdev^2)
    pca_var_percent <- round(pca_var * 100, 2)
    pca_data <- as.data.frame(pca$x) %>%
      rownames_to_column("SampleID") %>%
      separate(SampleID, into = c("Condition", "Time", "Replicate"), sep = "_", convert = TRUE) %>%
      mutate(Condition = factor(Condition, levels = c(input$ref_cat, setdiff(rv$conditions, input$ref_cat))))
    ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(Time), fill = as.factor(Time), shape = Condition)) +
      geom_point(size = 4) +
      ggforce::geom_mark_ellipse(aes(group = interaction(Time, Condition)),
                                 fill = NA, linetype = "dashed") +
      # scale_color_manual(values=cbPalette) +
      # scale_fill_manual(values=cbPalette) +
      labs(title = "PCA (Before Normalization)",
           x = paste0("PC1 (", pca_var_percent[1], "%)"),
           y = paste0("PC2 (", pca_var_percent[2], "%)"),
           color = "Time") +
      guides(fill = "none") +
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.1))) +
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
      theme_minimal() +
      theme(text = element_text(size = 16),
            axis.text = element_text(colour="black"),
            panel.grid = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(-0.2, "cm"),
            legend.position = "right")
  })

  output$pca_summary2 <- renderPlot({
    expression_mat <- rv$data_after_norm %>% column_to_rownames("ID")
    expression_mat <- expression_mat[apply(expression_mat, 1, function(feature) var(feature, na.rm = TRUE) > 0),]
    pca <- prcomp(t(na.omit(expression_mat)), center = T, scale. = T)
    pca_var <- pca$sdev^2 / sum(pca$sdev^2)
    pca_var_percent <- round(pca_var * 100, 2)
    pca_data <- as.data.frame(pca$x) %>%
      rownames_to_column("SampleID") %>%
      separate(SampleID, into = c("Condition", "Time", "Replicate"), sep = "_", convert = TRUE) %>%
      mutate(Condition = factor(Condition, levels = c(input$ref_cat, setdiff(rv$conditions, input$ref_cat))))
    ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(Time), fill = as.factor(Time), shape = Condition)) +
      geom_point(size = 4) +
      ggforce::geom_mark_ellipse(aes(group = interaction(Time, Condition)),
                                 fill = NA, linetype = "dashed") +
      # scale_color_manual(values=cbPalette) +
      # scale_fill_manual(values=cbPalette) +
      labs(title = "PCA (After Normalization)",
           x = paste0("PC1 (", pca_var_percent[1], "%)"),
           y = paste0("PC2 (", pca_var_percent[2], "%)"),
           color = "Time") +
      guides(fill = "none") +
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.1))) +
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
      theme_minimal() +
      theme(text = element_text(size = 16),
            axis.text = element_text(colour="black"),
            panel.grid = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(-0.2, "cm"),
            legend.position = "right")
  })

  output$hc_summary1 <- renderPlot({
    dist_mat <- dist(t(scale(rv$data %>% select(-ID))))
    hc <- hclust(dist_mat, method = "average")
    ggdendro::ggdendrogram(hc, rotate = TRUE) +
      labs(title = "Hierarchical Clustering - Before Normalization")
  })

  output$hc_summary2 <- renderPlot({
    dist_mat <- dist(t(scale(rv$data_after_norm %>% select(-ID))))
    hc <- hclust(dist_mat, method = "average")
    ggdendro::ggdendrogram(hc, rotate = TRUE) +
      labs(title = "Hierarchical Clustering - After Normalization")
  })

  # Enable the finalize button only after submit_norm is hit
  observeEvent(input$submit_norm, {
    enable("finalize")
  })

  observeEvent(input$finalize, {
    qc_finalized(TRUE)
    showNotification("QC workflow completed!", type = "message")
    enable(selector = "a[data-value=tab_data_vis]")
    enable(selector = "a[data-value=tab_fit_GLM]")
    enable(selector = "a[data-value=tab_GLM_vis]")
    # parse the data for GLM input after finalizing
    rv$GLM_input <- rv$data_after_norm_long
  })

  # --- Preview only after Finalize ---
  output$preview_ui <- renderUI({
    req(qc_finalized())
    tagList(
      h3("Data Preview"),
      tableOutput("preview"),
      downloadButton("download_processed_data", "Download Processed Data")
    )
  })

  output$preview <- renderTable({
    req(qc_finalized(), rv$data_after_norm)
    head(rv$data_after_norm, 10)
  }, caption = "Data After Processing (only showing the first 10 rows)",
  caption.placement = getOption("xtable.caption.placement", "top"))

  output$download_processed_data <- downloadHandler(
    filename = function(){"Processed Data.csv"},
    content = function(file) {
      write.csv(rv$data_after_norm, file, row.names = F)
    }
  )


  # --- Plots ---

  # Heatmap
  # to make sure heatmap does not automatically update by changing the inputs
  # only change the parameters value when the submit button is hit
  heatmap_params <- eventReactive(input$submit_heatmap, {
    list(data_heatmap = input$data_for_heatmap,
         show_rownames = input$show_rownames_heatmap,
         cluster_row = input$cluster_row_heatmap,
         cluster_col = input$cluster_col_heatmap)
  })

  output$heatmap <- renderPlot({
    params <- heatmap_params()
    heatmap_data <- if(params$data_heatmap == "orig_data"){
      rv$data_avg
    }else{
      rv$data_after_norm_avg
    }
    heatmap_data <- heatmap_data %>%
      select(-se) %>%
      pivot_wider(names_from = c(Condition, Time), values_from = mean) %>%
      column_to_rownames("ID")
    skew <- e1071::skewness(unlist(heatmap_data))
    heatmap_data <- if(abs(skew) > 2){
      log(heatmap_data + 1)
    }else{
      heatmap_data
    }
    Heatmap(as.matrix(heatmap_data),
            name = ifelse(abs(skew) > 2, "log(value)", "value"),
            col = colorRampPalette(c("slateblue4", "cadetblue2", "black", "yellow", "red"))(100),
            show_row_names = ifelse(params$show_rownames == "yes", TRUE, FALSE),
            row_names_gp = gpar(fontsize = 8),
            cluster_rows = ifelse(params$cluster_row == "yes", TRUE, FALSE),
            cluster_columns = ifelse(params$cluster_col == "yes", TRUE, FALSE))
  })

  # Barplots
  output$select_data_barplot <- renderUI({
    tagList(
      radioButtons("data_for_barplot",
                   tags$span("Choose the data for the plot",
                             shinyBS::bsButton("info_data_for_barplot", label = "", icon = icon("circle-info"), style = "default", size = "extra-small")),
                   choices = c("Original Data" = "orig_data",
                               "Normalized Data" = "norm_data")),
      shinyBS::bsPopover(id = "info_data_for_barplot",
                title = "",
                content = "Choose data for plot as desired. Sometimes it makes more sense to visualize the raw values between conditions",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")),
      selectInput("feature_barplot", "Select feature for plotting",
                  choices = rv$data[["ID"]]),
      actionButton("submit_barplot", "Submit")
    )
  })

  barplot_params <- eventReactive(input$submit_barplot, {
    list(data_barplot = input$data_for_barplot,
         feature_barplot = input$feature_barplot)
  })

  output$barplot <- renderPlot({
    # do not use ifelse as it's vectorized and cannot pass a whole df
    avg_data <- if(barplot_params()$data_barplot == "orig_data"){
      rv$data_avg
    }else{
      rv$data_after_norm_avg
    }
    long_data <- if(barplot_params()$data_barplot == "orig_data"){
      rv$data_long
    }else{
      rv$data_after_norm_long
    }
    barplot_data <- avg_data %>%
      filter(ID == barplot_params()$feature_barplot)
    test_res <- long_data %>%
      filter(ID == barplot_params()$feature_barplot) %>%
      group_by(Time) %>%
      summarise(p.value = tryCatch({t.test(Expression ~ Condition)$p.value}, error = function(e) NA)) %>%
      mutate(label = case_when(
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**",
        p.value <= 0.05 ~ "*",
        TRUE ~ NA
      ))
    add <- barplot_data %>%
      mutate(y = (mean + se) * 0.02) %>%
      pull(y) %>%
      max(., na.rm = TRUE)
    text_pos <- barplot_data %>%
      group_by(Time) %>%
      summarise(y = max(mean + se, na.rm = TRUE) + add, .groups = "drop")
    sig_labels <- left_join(test_res, text_pos, by = "Time")
    ggplot(barplot_data, aes(x = factor(Time), y = mean, fill = Condition)) +
      geom_bar(position="dodge", stat="identity") +
      geom_errorbar(aes(ymin = pmax(0, mean - se),
                        ymax = mean + se), width = 0.5,
                    position=position_dodge(0.9), linewidth = 0.7, alpha = 1) +
      geom_text(data = sig_labels,
                aes(x = factor(Time), y = y, label = label),
                inherit.aes = FALSE, na.rm = TRUE,
                vjust = 0,
                size = 10) +
      scale_y_continuous(expand = c(0, 0),
                         limits = c(0, max(barplot_data$mean + barplot_data$se, na.rm = TRUE) * 1.1)) +
      labs(title = barplot_params()$feature_barplot, x = "Time", y = "Value") +
      theme_classic() +
      theme(axis.text = element_text(colour="black", size = 12),
            axis.title = element_text(size = 16),
            axis.ticks.x = element_blank(),
            axis.ticks.length.y = unit(-0.15, "cm"),
            axis.ticks.y = element_line(linewidth = 1),
            axis.line = element_line(linewidth = 1),
            plot.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.title = element_blank())
  })

  # Boxplots
  output$select_data_boxplot <- renderUI({
    tagList(
      radioButtons("data_for_boxplot",
                   tags$span("Choose the data for the plot",
                             shinyBS::bsButton("info_data_for_boxplot", label = "", icon = icon("circle-info"), style = "default", size = "extra-small")),
                   choices = c("Original Data" = "orig_data",
                               "Normalized Data" = "norm_data")),
      shinyBS::bsPopover(id = "info_data_for_boxplot",
                title = "",
                content = "Choose data for plot as desired. Sometimes it makes more sense to visualize the raw values between conditions",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")),
      selectInput("feature_boxplot", "Select feature for plotting",
                  choices = rv$data[["ID"]]),
      actionButton("submit_boxplot", "Submit")
    )
  })

  boxplot_params <- eventReactive(input$submit_boxplot, {
    list(data_boxplot = input$data_for_boxplot,
         feature_boxplot = input$feature_boxplot)
  })

  output$boxplot <- renderPlot({
    long_data <- if(boxplot_params()$data_boxplot == "orig_data"){
      rv$data_long
    }else{
      rv$data_after_norm_long
    }
    boxplot_data <- long_data %>%
      filter(ID == boxplot_params()$feature_boxplot)
    test_res <- boxplot_data %>%
      group_by(Time) %>%
      summarise(p.value = tryCatch({t.test(Expression ~ Condition)$p.value}, error = function(e) NA)) %>%
      mutate(label = case_when(
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**",
        p.value <= 0.05 ~ "*",
        TRUE ~ NA
      ))
    add <- boxplot_data %>%
      group_by(Time) %>%
      mutate(y = max(Expression, na.rm = TRUE) * 0.02) %>%
      pull(y) %>%
      max(., na.rm = TRUE)
    text_pos <- boxplot_data %>%
      group_by(Time) %>%
      summarise(y = max(Expression, na.rm = TRUE) + add, .groups = "drop")
    sig_labels <- left_join(test_res, text_pos, by = "Time")
    ggplot(boxplot_data, aes(x = factor(Time), y = Expression, fill = Condition)) +
      geom_boxplot(size = 1, staplewidth = 0.5, outliers = FALSE, position = position_dodge(width = 0.8)) +
      geom_point(aes(group = Condition), color = "darkgrey", alpha = 0.5, size = 1.5, position = position_dodge(width = 0.8)) +
      geom_text(data = sig_labels,
                aes(x = factor(Time), y = y, label = label),
                inherit.aes = FALSE, na.rm = TRUE,
                vjust = 0,
                size = 10) +
      scale_y_continuous(expand = c(0, 0),
                         limits = c(0, max(boxplot_data$Expression, na.rm = TRUE) * 1.1)) +
      labs(title = boxplot_params()$feature_boxplot, x = "Time", y = "Value") +
      theme_classic() +
      theme(axis.text = element_text(colour="black", size = 12),
            axis.title = element_text(size = 16),
            axis.ticks.x = element_blank(),
            axis.ticks.length.y = unit(-0.15, "cm"),
            axis.ticks.y = element_line(linewidth = 1),
            axis.line = element_line(linewidth = 1),
            plot.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.title = element_blank())
  })

  # Linegraphs
  output$select_data_linegraph <- renderUI({
    tagList(
      radioButtons("data_for_linegraph",
                   tags$span("Choose the data for the plot",
                             shinyBS::bsButton("info_data_for_linegraph", label = "", icon = icon("circle-info"), style = "default", size = "extra-small")),
                   choices = c("Original Data" = "orig_data",
                               "Normalized Data" = "norm_data")),
      shinyBS::bsPopover(id = "info_data_for_linegraph",
                title = "",
                content = "Choose data for plot as desired. Sometimes it makes more sense to visualize the raw values between conditions",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")),
      selectInput("feature_linegraph", "Select feature for plotting",
                  choices = rv$data[["ID"]]),
      radioButtons("smooth_linegraph", "Smoothed line",
                   choices = c("Yes" = "yes", "No" = "no"), selected = "no"),
      actionButton("submit_linegraph", "Submit")
    )
  })

  linegraph_params <- eventReactive(input$submit_linegraph, {
    list(data_linegraph = input$data_for_linegraph,
         feature_linegraph = input$feature_linegraph,
         smooth = input$smooth_linegraph)
  })

  output$linegraph <- renderPlot({
    avg_data <- if(linegraph_params()$data_linegraph == "orig_data"){
      rv$data_avg
    }else{
      rv$data_after_norm_avg
    }
    long_data <- if(linegraph_params()$data_linegraph == "orig_data"){
      rv$data_long
    }else{
      rv$data_after_norm_long
    }
    linegraph_data <- avg_data %>%
      filter(ID == linegraph_params()$feature_linegraph)
    test_res <- long_data %>%
      filter(ID == linegraph_params()$feature_linegraph) %>%
      group_by(Time) %>%
      summarise(p.value = tryCatch({t.test(Expression ~ Condition)$p.value}, error = function(e) NA)) %>%
      mutate(label = case_when(
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**",
        p.value <= 0.05 ~ "*",
        TRUE ~ NA
      ))
    add <- linegraph_data %>%
      mutate(y = (mean + se) * 0.03) %>%
      pull(y) %>%
      max(., na.rm = TRUE)
    text_pos <- linegraph_data %>%
      group_by(Time) %>%
      summarise(y = max(mean + se, na.rm = TRUE) + add, .groups = "drop")
    sig_labels <- left_join(test_res, text_pos, by = "Time")
    p <- ggplot(linegraph_data, aes(x = factor(Time), y = mean,
                               group = Condition, shape = Condition, color = Condition)) +
      geom_point(size = 4) +
      geom_errorbar(aes(ymin = pmax(0, mean - se), ymax = mean + se),
                    width = 0.2, linewidth = 0.7, alpha = 1) +
      geom_text(data = sig_labels,
                aes(x = factor(Time), y = y, label = label),
                inherit.aes = FALSE, na.rm = TRUE,
                vjust = 0.75, hjust = 0,
                size = 10, angle = 90) +
      scale_y_continuous(expand = c(0, 0),
                         limits = c(0, max(linegraph_data$mean + linegraph_data$se, na.rm = TRUE) * 1.1)) +
      labs(title = linegraph_params()$feature_linegraph, x = "Time", y = "Value") +
      theme_classic() +
      theme(axis.text = element_text(colour="black", size = 12),
            axis.title = element_text(size = 16),
            axis.ticks.x = element_blank(),
            axis.ticks.length.y = unit(-0.15, "cm"),
            axis.ticks.y = element_line(linewidth = 1),
            axis.line = element_line(linewidth = 1),
            plot.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.title = element_blank())
    if(linegraph_params()$smooth == "yes"){
      p +
        ggalt::geom_xspline(spline_shape = -0.5) +
        aes(lwd = 1.8) +
        scale_linewidth_identity()
    }else{
      p +
        geom_line(linewidth = 1.8)
    }
  })
  # adding this to avoid display issue where smoothed line plot does not update until resizing window
  graphics.off()


  # --- GLM Fitting ---
  GLM_res <- eventReactive(input$submit_fit_GLM,{
    center_time_operation <- ifelse(input$center_time == "center", "Time - mean(Time)", "Time = Time")
    # mutations based on selections
    rv$GLM_input <- rv$GLM_input %>%
      filter(!is.na(Expression)) %>%
      mutate(Time = as.numeric(Time),
             Condition = factor(Condition, levels = c(input$ref_cat, setdiff(rv$conditions, input$ref_cat))),
             Indicator = ifelse(Condition == input$ref_cat, 0, 1)) %>%
      group_by(ID) %>%
      mutate(Time = !!parse_expr(center_time_operation)) %>%
      ungroup() %>%
      as.data.frame()
    # disable the submit button immediately when it's hit
    shinyjs::disable("submit_fit_GLM")
    # fit and trajectory classification
    fit_res <- GLM_Fit_and_Classification(rv$GLM_input)
    # Permutation
    perm_res <- PermuteWrapperFast(rv$GLM_input, input$n_perm)
    perm_res <- perm_res %>%
      left_join(fit_res[,c("ID", "weighted_rmsd")], by = "ID") %>%
      group_by(ID) %>%
      summarise(p_value = (sum(perm_value > weighted_rmsd) + 1) / (input$n_perm + 1), .groups = "drop")
    # Compute empirical p-values and put results together
    final_res <- fit_res %>%
      left_join(perm_res, by = "ID") %>%
      # rowwise() %>%
      # mutate(p_value = (sum(c_across(starts_with("perm_")) >= weighted_rmsd) + 1) / (input$n_perm + 1)) %>%
      # ungroup() %>%
      # select(-contains("perm_")) %>%
      mutate(FDR = p.adjust(p_value, method = "fdr")) %>%
      arrange(desc(weighted_rmsd))
    # Rearrange the p value columns
    final_res <- final_res[,c(1:5, 12, 13, 6:11)]
    # enable the button once the run is finished
    shinyjs::enable("submit_fit_GLM")
    return(final_res)
  })

  output$GLM_results <- renderTable({head(GLM_res(), n = 50)},
                                    caption = "Results Table (only showing the first 50 rows)",
                                    caption.placement = getOption("xtable.caption.placement", "top"),
                                    digits = 4)

  # show download button after the calculation is done
  output$download_full_table_ui <- renderUI({
    req(GLM_res())
    downloadButton("download_full_table", "Download Full Table")
  })

  output$download_full_table <- downloadHandler(
    filename = function(){"GLM Results.xlsx"},
    content = function(file) {
      params_df <- data.frame(parameter = c("Filter Out Problematic Features", "Imputation", "Sample Normalization",
                                            "Log Transformation", "Feature Normalization",
                                            "Center Time", "# Permutation"),
                              choice = c(ifelse(!is.null(input$filter_choice), input$filter_choice, "No filtering needed"),
                                         ifelse(!is.null(input$impute_choice), gsub("_impute", "", input$impute_choice), "No imputation needed"),
                                         ifelse(input$sample_norm_choice == "median_norm", "median normalization", "no normalization"),
                                         ifelse(input$log_choice != "no_log", input$log_choice, "no log transformation"),
                                         ifelse(input$feature_norm_choice != "no_feature_norm", gsub("_norm", "", input$feature_norm_choice), "no normalization"),
                                         ifelse(input$center_time == "center", "yes", "no"),
                                         input$n_perm))
      writexl::write_xlsx(list(GLM_res(), params_df), file)
    }
  )


  # --- GLM Plots ---
  # 3D plot
  output$`3D_plot` <- renderPlotly({
    cat1 <- input$ref_cat
    cat2 <- setdiff(rv$conditions, input$ref_cat)
    plotdata_3d <- GLM_res() %>%
      select(ID, contains(cat1), contains(cat2), weighted_rmsd, category) %>%
      pivot_longer(cols = -c(ID, weighted_rmsd, category),
                   names_to = c("variable", "condition"),
                   names_sep = "_",
                   values_to = "value") %>%
      pivot_wider(names_from = variable,
                  values_from = value) %>%
      mutate(category = factor(category, levels = c("Linear Concordance",
                                                    "Linear Discordance",
                                                    "Polynomial Concordance",
                                                    "Polynomial Discordance",
                                                    "Cross-Model Discordance"))) %>%
      group_by(ID) %>%
      mutate(hover_info = paste0("Feature: ", ID,
                                 "<br>Condition: ", condition,
                                 "<br>Intercept: ", round(intercept, 2),
                                 " Slope: ", round(slope, 3),
                                 " Curvature: ", round(curvature, 4)
      ),
      paired_hover_text = paste0(
        ifelse(condition == cat1,
               paste0("Paired Gene (", cat2, "):", round(intercept[condition == cat2], 2), " ", round(slope[condition == cat2], 3), " ", round(curvature[condition == cat2], 4)),
               paste0("Paired Gene (", cat1, "):", round(intercept[condition == cat1], 2), " ", round(slope[condition == cat1], 3), " ", round(curvature[condition == cat1], 4))
        ),
        "<br>Weighted RMSD: ", round(weighted_rmsd, 2))
      )
      highlight_key_data <- highlight_key(plotdata_3d, key = ~ID)
      plot <- plot_ly(data = highlight_key_data,
                      x = ~intercept, y = ~slope, z = ~curvature,
                      type = "scatter3d",
                      mode = "markers",
                      color = ~category,
                      colors = c("#66CCEE", "#EE6677", "#4477AA", "#AA3377", "lightgoldenrod3"),
                      symbol = ~condition,
                      symbols = c("square", "cross"),
                      text = ~paste(hover_info, paired_hover_text, sep = "<br>"),
                      hoverinfo = "text",
                      marker = list(size = 5)) %>%
        layout(
          scene = list(
            xaxis = list(title = "Intercept", color = "black", gridcolor = "dimgray",
                         titlefont = list(size = 24), tickfont = list(size = 16)),
                         #range = c(-6, 6), tickvals = c(-4, -2, 0, 2, 4)),
                         #range = c(-10, 10), tickvals = c(-8, -4, 0, 4, 8)),
            yaxis = list(title = "Slope", color = "black", gridcolor = "dimgray",
                         titlefont = list(size = 24), tickfont = list(size = 16)),
                         #range = c(-0.3, 0.3), tickvals = c(-0.2, -0.1, 0, 0.1, 0.2)),
                         #range = c(-0.6, 0.6), tickvals = c(-0.4, -0.2, 0, 0.2, 0.4)),
            zaxis = list(title = "Curvature", color = "black", gridcolor = "dimgray",
                         titlefont = list(size = 24), tickfont = list(size = 16))
                         #range = c(-0.015, 0.015), tickvals = c(-0.01, -0.005, 0, 0.005, 0.01))
                         #range = c(-0.03, 0.03), tickvals = c(-0.02, -0.01, 0, 0.01, 0.02))
          ),
          title = "GLM Coefficients",
          legend = list(x = 0.8, y = 0.9, font = list(size = 20))
        ) %>%
        highlight(
          on = "plotly_click",
          off = "plotly_doubleclick",
          dynamic = TRUE,
          selectize = TRUE,
          opacityDim = 0.3
        ) %>%
        htmlwidgets::onRender("
          function(el, x) {
            el.on('plotly_hover', function(d) {
              var hoverLayer = document.querySelector('.hoverlayer');
              if (hoverLayer) {
                hoverLayer.style.textAlign = 'left';  // Left-align text
              }
            });
          }
        ")
  })

  # Volcano plot
  output$select_cutoff_volcano <- renderUI({
    tagList(
      numericInput("rmsd_cutoff_volcano",
                   tags$span("Choose the RMSD cutoff for the volcano plot",
                             shinyBS::bsButton("info_rmsd_cutoff_volcano", label = "", icon = icon("circle-info"), style = "default", size = "extra-small")),
                   value = 100, min = 1, max = nrow(GLM_res()), step = 100),
      shinyBS::bsPopover(id = "info_rmsd_cutoff_volcano",
                title = "",
                content = "Cutoff for weighted RMSD. Cutoff is in terms of ranking. 100 means to make a cutoff to show the top 100 features",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")),
      numericInput("pval_cutoff_volcano",
                   tags$span("P-value cutoff",
                             shinyBS::bsButton("info_pval_cutoff_volcano", label = "", icon = icon("circle-info"), style = "default", size = "extra-small")),
                   value = 0.05, min = 0, max = 1, step = 0.1),
      shinyBS::bsPopover(id = "info_pval_cutoff_volcano",
                title = "",
                content = "Choose cutoff for p-value. Adjusted p-value (FDR) is used in plot",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")),
      actionButton("submit_volcano", "Submit")
    )
  })

  # observeEvent(input$submit_volcano, {
  #   if (input$rmsd_cutoff_volcano < 1 || input$rmsd_cutoff_volcano > nrow(GLM_res())) {
  #     showNotification("RMSD cutoff value must be between 1 and number of features in the data", type = "error")
  #   }
  #   if (input$pval_cutoff_volcano < 0 || input$pval_cutoff_volcano > 1) {
  #     showNotification("P-value must be between 0 and 1", type = "error")
  #   }
  # })

  volcano_params <- eventReactive(input$submit_volcano, {
    list(rmsd_cutoff_volcano = input$rmsd_cutoff_volcano,
         pval_cutoff_volcano = input$pval_cutoff_volcano)
  })

  output$volcano <- renderPlot({
    # if the values are not valid, disable the submit button
    shinyjs::disable("submit_volcano")
    # make sure input values are in range
    validate(
      need(input$rmsd_cutoff_volcano >= 1 && input$rmsd_cutoff_volcano <= nrow(GLM_res()),
           "RMSD cutoff must be within valid range"),
      need(input$pval_cutoff_volcano >= 0 && input$pval_cutoff_volcano <= 1,
           "P-value must be between 0 and 1"),
      need(input$pval_cutoff_volcano >= min(GLM_res()$FDR),
           "0 feature with the p-value cutoff"))
    # after validation, enable the submit button
    shinyjs::enable("submit_volcano")
    # create custom colors for points
    colors <- data.frame(category = GLM_res()$category) %>%
      mutate(
        color = case_when(
          category == "Linear Concordance" ~ "#66CCEE",
          category == "Polynomial Concordance" ~ "#EE6677",
          category == "Linear Discordance" ~ "#4477AA",
          category == "Polynomial Discordance" ~ "#AA3377",
          category == "Cross-Model Discordance" ~ "lightgoldenrod3")
      )
    keyvals <- unlist(colors$color)
    names(keyvals) <- colors$category

    sig_by_pval <- GLM_res() %>%
      filter(FDR < volcano_params()$pval_cutoff_volcano)

    rmsd_cutoff <- ifelse(volcano_params()$rmsd_cutoff_volcano >= nrow(sig_by_pval),
                          0, # show all features in user ask for top x where x is larger than number of features
                          mean(sig_by_pval$weighted_rmsd[volcano_params()$rmsd_cutoff_volcano], sig_by_pval$weighted_rmsd[volcano_params()$rmsd_cutoff_volcano + 1]))

    EnhancedVolcano(GLM_res(), lab = GLM_res()$ID,
                    x = "weighted_rmsd",
                    y = "FDR",
                    title = "",
                    subtitle = "",
                    xlab = "RMSD",
                    pCutoff = volcano_params()$pval_cutoff_volcano, FCcutoff = rmsd_cutoff,
                    xlim = c(-0.5, ceiling(max(GLM_res()$weighted_rmsd)) + 1),
                    ylim = c(0, 3),
                    pointSize = 3, labSize = 5,
                    colAlpha = 1,
                    colCustom = keyvals,
                    max.overlaps = 60,
                    boxedLabels = TRUE,
                    drawConnectors = TRUE,
                    widthConnectors = 0.5,
                    colConnectors = 'black',
                    legendPosition = 'none')
  })

  output$piechart <- renderPlot({
    # Pie chart
    piechart_df <- as.data.frame(table(GLM_res() %>% pull(category))) %>%
      rename(category = Var1, count = Freq) %>%
      mutate(percentage = count / sum(count),
             label = ifelse(percentage > 0.01,  # Hide labels under 1%
                            paste0(round(percentage * 100), "%"),
                            ""))
    # if any category is not present, add them back
    piechart_df <- data.frame(category = c("Linear Concordance", "Linear Discordance",
                                           "Polynomial Concordance", "Polynomial Discordance",
                                           "Cross-Model Discordance")) %>%
      left_join(piechart_df) %>%
      mutate(category = factor(category,
                               levels = c("Linear Concordance", "Linear Discordance",
                                          "Polynomial Concordance", "Polynomial Discordance",
                                          "Cross-Model Discordance")))
    piechart_df$count[is.na(piechart_df$count)] <- 0
    piechart_df$percentage[is.na(piechart_df$percentage)] <- 0
    piechart_df$label[is.na(piechart_df$label)] <- ""

    ggplot(piechart_df, aes(x = "", y = count, fill = category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = label),
                position = position_stack(vjust = 0.5),
                size = 6, color = "black") +
      scale_fill_manual(values = c(
        "#66CCEE",  # Linear Concordance
        "#4477AA",  # Linear Divergence
        "#EE6677",  # Polynomial Concordance
        "#AA3377",  # Polynomial Divergence
        "lightgoldenrod3"  # Cross-Model Divergence
      ))+
      theme_void() +
      labs(title = NULL) +
      theme(text = element_text(size = 20),
            legend.title = element_blank())
  })

}
