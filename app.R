# extrinsic-dq-app
# Extrinsic Data Quality Assessment Tool
# Version: 1.0
# Authors: Vanessa Lux, Siyu Zhang
# This work utilized GPT-4.1 for coding support; all code generated was thoroughly reviewed and validated by the authors.

# Full R code for the Extrinsic Data Quality Assessment ShinyApp with indicator and dataset specific plots.
# Input weights are scaled to 0-1 - keeping within and between relation of weights.

library(shiny)
library(tidyverse)
library(bslib)
library(zip)


# ==============================================================================
# 1. Define indicators and weights
# ==============================================================================

# List of quality indicators mapped to the five dimensions
indicators <- list(
  Findability = c("doi_available", "keywords_assigned", "categories_assigned", "temporal_info", "spatial_info"),
  Accessibility = c("access_url_accessible", "download_url_given", "download_url_accessible", "downloadable_without_registration"),
  Interoperability = c("file_format_documented", "media_type_documented", "controlled_vocabularies_usage", "non_proprietary", "machine_readability"),
  Reusability = c("license_documented", "use_restrictions", "contact_point", "dataset_author", "data_description", "read_me_file", "variable_documentation", "raw_data", "preprocessed_data"),
  Contextuality = c("file_size", "data_publication_date", "data_modification_date", "associated_publication")
)

# Labels for indicator variables
indicator_labels <- c(
  doi_available                = "DOI available",
  keywords_assigned            = "Keywords assigned",
  categories_assigned          = "Categories assigned",
  temporal_info                = "Temporal information documented",
  spatial_info                 = "Geographical information documented",
  access_url_accessible        = "Access URL accessible",
  download_url_given           = "Download URL provided",
  download_url_accessible      = "Download URL accessible",
  downloadable_without_registration = "Download without registration",
  file_format_documented       = "File format documented",
  media_type_documented        = "Media type documented",
  controlled_vocabularies_usage = "Controlled vocabularies used",
  non_proprietary              = "Non‑proprietary formats used",
  machine_readability          = "Machine‑readable format provided",
  license_documented           = "Usage license documented",
  use_restrictions             = "Use restrictions documented",
  contact_point                = "Contact information provided",
  dataset_author               = "Dataset author documented",
  data_description             = "Dataset description available",
  read_me_file                 = "README or documentation file available",
  variable_documentation       = "Variables documented (codebook, etc.)",
  raw_data                     = "Raw data available",
  preprocessed_data            = "Preprocessed data available",
  file_size                    = "File size documented",
  data_publication_date        = "Data publication date documented",
  data_modification_date       = "Data modification date documented",
  associated_publication       = "Associated publication available"
)

# Default weight values for each indicator
weights_default <- list(
  Findability = c(50, 30, 20, 20, 20),
  Accessibility = c(40, 30, 40, 20),
  Interoperability = c(20, 10, 10, 30, 30),
  Reusability = c(20, 20, 20, 10, 20, 20, 20, 20, 20),
  Contextuality = c(5, 5, 5, 20)
)

indicator_colors <- c(
  "Findability_score" = "#f0f921",
  "Accessibility_score" = "#fca636",
  "Interoperability_score" = "#e16462",
  "Reusability_score" = "#b12a90",
  "Contextuality_score" = "#6a00a8",
  "Overall_score" = "#0d0887"
)



# Only for dot plot (dimensions = indicators + overall score)
dimension_order <- c(
  "Overall_score",        
  "Findability_score",
  "Accessibility_score",
  "Interoperability_score",
  "Reusability_score",
  "Contextuality_score"
)

# For bar plots
indicator_order <- c(
  "Findability_score",
  "Accessibility_score",
  "Interoperability_score",
  "Reusability_score",
  "Contextuality_score"
)

# ==============================================================================
# 2. Extrinsic DQ Score Function
# ==============================================================================

# Metadata quality assessment
assess_metadata_quality <- function(metadata_df, weights = weights_default) {
  
  # Normalize weights within each indicator
  weights_normalized <- lapply(weights, function(w) w / sum(w))
  
  # Normalize indicator-level weights to sum to 1
  indicator_totals <- sapply(weights, sum)
  indicator_weights <- indicator_totals / sum(indicator_totals)
  
  # Initialize vector to track score column names
  score_cols <- c()
  
  for (dimension in names(indicators)) {
    cols <- indicators[[dimension]]
    existing_cols <- cols[cols %in% colnames(metadata_df)]
    
    if (length(existing_cols) > 0) {
      # Match weights to existing columns
      weight_vec <- weights_normalized[[dimension]][match(existing_cols, cols)]
      
      # Compute weighted scores per row
      weighted_scores <- sweep(metadata_df[, existing_cols, drop = FALSE], 2, weight_vec, `*`)
      metadata_df[[paste0(dimension, "_score")]] <- rowSums(weighted_scores, na.rm = TRUE)
    } else {
      metadata_df[[paste0(dimension, "_score")]] <- NA
    }
    
    score_cols <- c(score_cols, paste0(dimension, "_score"))
  }
  
  # Compute overall score as weighted average of indicator scores
  indicator_weights_vector <- indicator_weights[names(indicators)]
  metadata_df$Overall_score <- rowSums(
    sweep(metadata_df[, score_cols, drop = FALSE], 2, indicator_weights_vector, `*`),
    na.rm = TRUE
  )
  
  return(metadata_df)
}

# ==============================================================================
# 3. UI
# ==============================================================================

# Function to generate numeric inputs for weights dynamically
create_weight_inputs <- function(dim_name, prefix) {
  fields <- indicators[[dim_name]]
  defaults <- weights_default[[dim_name]]
  lapply(seq_along(fields), function(i) {
    numericInput(
      inputId = paste0(prefix, "_", i),
      label   = indicator_labels[ fields[i] ],  
      value   = defaults[i],
      min     = 0
    )
  })
}

# Read footer file
footer_html <- includeHTML("footer.html")

ui <- tagList(
    tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(HTML("document.addEventListener('change', function(e) {
      if (e.target && e.target.id === 'file') {
        var files = e.target.files;
        var nameSpan = document.getElementById('file-name-display');
        if (!nameSpan) return;
        if (files.length === 0) {
          nameSpan.textContent = 'No file selected';
        } else if (files.length === 1) {
          nameSpan.textContent = files[0].name;
        } else {
          nameSpan.textContent = files.length + ' files selected';
        }
      }
    });
  ")),
    HTML(sprintf("
      <!-- etracker code 6.0 -->
      <script type='text/javascript'>
         var et_pagename = '%s';
         var et_areas    = '%s';
      </script>
      <script id='_etLoader'
              type='text/javascript'
              charset='UTF-8'
              data-block-cookies='true'
              data-secure-code='%s'
              src='//code.etracker.com/code/e.js'
              async>
      </script>
      <!-- etracker code 6.0 end -->
      ",
                 Sys.getenv('ET_PAGENAME'),
                 Sys.getenv('ET_AREAS'),
                 Sys.getenv('ET_SECURE_CODE')
    ))
    ),

    ## -------- GLOBAL HEADER with GESIS logo --------
    tags$div(
      class = "global-header",
      tags$img(
        src = "logo_gesis_en.svg",
        alt = "GESIS Logo",
        class = "global-header-logo"
      )
    ),
    ## -----------------------------------------------------
    
    
    page_navbar(title = "Extrinsic Data Quality Assessment",
   
  # --------- Introduction Panel ---------
  nav_panel("Introduction",
            div(class = "container", style = "max-width: 1100px; padding-top: 2.5rem; padding-bottom: 3rem;",
                tags$header(
                  h1("Extrinsic Data Quality Assessment Tool"),
                  hr(),
                  
                # 1. About the Tool
                div(class = "row mt-4",
                    div(class = "col-md-12",
                        h2("About the Tool", class = "mb-4"),
                        
                        p("This tool provides a quantitative method to assess the Extrinsic Data Quality of a dataset based on its metadata and contextual information."),
                        p("As outlined by ", 
                          tags$a("Birkenmaier et al. (2024)",
                                 href = "https://doi.org/10.21241/ssoar.96764",
                                 target = "_blank"), 
                          ", data quality depends on the intended purpose of use and relies on two main pillars:"),
                        
                        tags$ul(
                          tags$li("Intrinsic requirements:", "Is the data ", strong("accurate "), "for the intended purpose?"),
                          tags$li("Extrinsic requirements:", "Is the data ", strong("usable "), "for the intended purpose?")
                        ),
                        
                        p("This tool specifically targets the extrinsic aspect, determining whether a dataset is sufficiently prepared for (re-)use. This comprises the way a dataset is documented, archived, and made accessible while respecting legal and privacy requirements."),
                        
                        h3("Who is this for?"),
                        tags$ul(
                          tags$li("Social scientists wishing to assess and document the extrinsic data quality of their (re)used data,"),
                          tags$li("Scholars interested in studying data accessibility, usability, and related biases,"),
                          tags$li("Data archivists aiming to compare parts of their collections to assess curation needs.")
                        ),
                        
                        h3("How is extrinsic data quality measured?"),
                        p("To measure extrinsic data quality, the tool assesses datasets across five key dimensions. We expanded the four ", 
                        strong("FAIR criteria"), " – ", strong("findability, accessibility, interoperability, and reusability"), " – by incorporating ", strong("contextual information,"),
                        " such as related publications and changes to the dataset, as an additional dimension in the assessment. Contextual information contributes to extrinsic data quality by improving the interpretability of a dataset. 
                        The assessment can be tailored to the specific purpose of use by ", strong("adjusting the weighting"), "of these dimensions.
                        The tool also allows ",strong("comparison of multiple datasets"), " across these extrinsic data quality dimensions."),                        
                        p("The methodology is adapted from the ", 
                          tags$a("Metadata Quality Assessment (MQA)", 
                                 href = "https://data.europa.eu/mqa/methodology?locale=en", 
                                 target = "_blank"), 
                          " tool by data.europa.eu, broadening its application from large-scale data portals to the evaluation of individual datasets.")
                    )
                
                ),
                
                # 2. How to use the tool
                h2("How to use this tool?", class = "mt-5 mb-4"),
                layout_column_wrap(
                  width = 1/3,
                  gap = "1.5rem",
                  card(
                    card_header("1. Upload Metadata"),
                    card_body("Upload a CSV file where each row represents a dataset. Please ensure columns match the required naming convention (see coding scheme).")
                  ),
                  card(
                    card_header("2. Define Weights"),
                    card_body("Not every indicator is equally important for every project. You can adjust the weights in the dashboard to reflect your specific research use case or data type.")
                  ),
                  card(
                    card_header("3. Evaluate Scores"),
                    card_body("The tool calculates weighted dimension-level scores and an overall score, allowing you to benchmark and compare datasets effectively.")
                  )
                ),
                br(),
                hr(),
                # How to cite the tool
                        p(strong("Citation")),
                        p("Lux, Vanessa, & Zhang, Siyu (2026). The Extrinsic Data Quality Assessment Tool (ShinyApp),", 
                          tags$a("https://shiny.gesis.org/extrinsic-dq/", 
                                 href = "https://shiny.gesis.org/extrinsic-dq/", 
                                 target = "_blank") 
                          ),
                        br(),
                        p("Last updated: 2026-03-20", style = "font-size: small;")
                
                
            )
            )           
),
  

  
  # --------- Coding Scheme Panel ---------
  nav_panel("Coding Scheme",
            div(class = "container", style = "max-width: 1100px; padding-top: 2.5rem; padding-bottom: 3rem;",
                tags$header(
                  h1("Extrinsic Data Quality Assessment Tool"),
                  hr(),
                  
                # Coding scheme
                h2("Coding Scheme", class = "mb-4"), 
                card(
                  card_body(
                    p("Each quality indicator is operationalized as a metadata feature and evaluated using binary coding:"),
                    tags$ul(
                      tags$li(strong("1"), " = The criterion/feature is present or met."),
                      tags$li(strong("0"), " or ", strong("NA"), " = The criterion is absent or not documented.")
                    ),
                    
                    div(class = "mt-4",
                        h3("Sample CSV Structure"),
                        div(style = "overflow-x: auto; border: 1px solid #dee2e6; border-radius: 4px;",
                            tableOutput("sample_data_table")
                        )
                    ),
                    
                    
                  # Full list of indicators
                      accordion(
                        open = FALSE,
                        class = "mt-3 coding-accordion",
                        accordion_panel(
                          "Full List of Indicators",
                          div(
                            class = "small",
                            
                            # Findability
                            h6(strong("Findability")),
                            tags$ul(
                              tags$li(
                                tags$code("doi_available"),
                                " – DOI is available for the dataset."
                              ),
                              tags$li(
                                tags$code("keywords_assigned"),
                                " – Keywords are assigned to the dataset; if at least one keyword is assigned, code as 1."
                              ),
                              tags$li(
                                tags$code("categories_assigned"),
                                " – Categories specifying the dataset are given (e.g., sensor data, big data, experimental data). Any category counts; if at least one category is assigned, code as 1."
                              ),
                              tags$li(
                                tags$code("temporal_info"),
                                " – Information about when the dataset was uploaded or released is documented."
                              ),
                              tags$li(
                                tags$code("spatial_info"),
                                " – Geographic information about the origin of the dataset is documented. This can be the researchers’ location, institution, or the place where the data was collected."
                              )
                            ),
                            
                            # Accessibility
                            h6(class = "mt-3", strong("Accessibility")),
                            tags$ul(
                              tags$li(
                                tags$code("access_url_accessible"),
                                " – URL to the dataset page is provided and accessible."
                              ),
                              tags$li(
                                tags$code("download_url_given"),
                                " – URL of a download link is provided on the dataset page."
                              ),
                              tags$li(
                                tags$code("download_url_accessible"),
                                " – The download link actually gives access to the data (not a dead/broken link)."
                              ),
                              tags$li(
                                tags$code("downloadable_without_registration"),
                                " – Data can be downloaded without prior registration or login."
                              )
                            ),
                            
                            # Interoperability
                            h6(class = "mt-3", strong("Interoperability")),
                            tags$ul(
                              tags$li(
                                tags$code("file_format_documented"),
                                " – File format of the dataset is documented."
                              ),
                              tags$li(
                                tags$code("media_type_documented"),
                                " – Media type (e.g., text/csv, application/zip) is documented, ideally using a predefined list (controlled vocabulary)."
                              ),
                              tags$li(
                                tags$code("controlled_vocabularies_usage"),
                                " – Controlled vocabularies or standardized lists are used (e.g., for formats, keywords, subject categories)."
                              ),
                              tags$li(
                                tags$code("non_proprietary"),
                                " – File format is non‑proprietary (e.g., CSV instead of XLSX)."
                              ),
                              tags$li(
                                tags$code("machine_readability"),
                                " – File format is machine‑readable (e.g., CSV, JSON, XML)."
                              )
                            ),
                            
                            # Reusability
                            h6(class = "mt-3", strong("Reusability")),
                            tags$ul(
                              tags$li(
                                tags$code("license_documented"),
                                " – A usage license for the dataset is documented."
                              ),
                              tags$li(
                                tags$code("use_restrictions"),
                                " – Any reuse restrictions according to the license are documented (e.g., non‑commercial only, restricted access)."
                              ),
                              tags$li(
                                tags$code("contact_point"),
                                " – Information about whom to contact in case of questions about the dataset is provided."
                              ),
                              tags$li(
                                tags$code("dataset_author"),
                                " – Information about who published or created the dataset is documented."
                              ),
                              tags$li(
                                tags$code("data_description"),
                                " – A textual description of the dataset (scope, content, purpose, etc.) is provided."
                              ),
                              tags$li(
                                tags$code("read_me_file"),
                                " – A README or similar documentation file is available."
                              ),
                              tags$li(
                                tags$code("variable_documentation"),
                                " – Variables are documented (e.g., codebook, variable labels, value definitions)."
                              ),
                              tags$li(
                                tags$code("raw_data"),
                                " – Raw (original) data are available."
                              ),
                              tags$li(
                                tags$code("preprocessed_data"),
                                " – Preprocessed or cleaned data are available."
                              )
                            ),
                            
                            # Contextuality
                            h6(class = "mt-3", strong("Contextuality")),
                            tags$ul(
                              tags$li(
                                tags$code("file_size"),
                                " – File size of the dataset is documented."
                              ),
                              tags$li(
                                tags$code("data_publication_date"),
                                " – Date of publication or release of the dataset is documented (can be identical to the temporal information if only one date is given)."
                              ),
                              tags$li(
                                tags$code("data_modification_date"),
                                " – Dates at which the data have been modified or updated are documented."
                              ),
                              tags$li(
                                tags$code("associated_publication"),
                                " – An associated publication (e.g., article, report) is documented and linked to the dataset."
                              )
                            )
                          )
                        )
                      ),
                  
                  # Coding Template download 
                  
                  p("A template CSV file aligned with the coding scheme can be downloaded here:"),
                  downloadButton(
                    outputId = "download_coding_template",
                    label = "Download Coding Template (CSV)"
                  )
                      
                    )
                  )
                )
             
            )
  
  ),
  
  
  # --------- Analysis Dashboard Panel---------
  nav_panel("Analysis Dashboard",
            layout_sidebar(
              sidebar = sidebar(
                width = 360,
                class = "analysis-sidebar",
                div(class = "small sidebar-downloads",
                    h5("Data Input"),
                    p("Example data are used to illustrate the score table and plots. To evaluate your own dataset, simply upload your assessment file here. Please refer to the", strong("Coding Scheme"),"for correct variable names and formatting."),
                  #  h6("Upload CSV File"),
                  div(
                    class = "custom-file-input-wrapper",
                    fileInput("file", "Upload CSV File")
                  ),
                  div(
                    class = "custom-file-display",
                    tags$label(
                      `for` = "file",
                      class = "btn custom-browse-btn",
                      tags$i(class = "fa-solid fa-file-upload upload-icon"),
                      #tags$span(class = "upload-icon", "🖴↑"),
                      tags$span(class = "upload-text", "Upload CSV file")
                    ),
                    span(id = "file-name-display", "No file selected")
                  ),
                  
                    radioButtons("sep", "CSV Delimiter", choices = c("Comma" = ",", "Semicolon" = ";"), selected = ","),
                    
                    hr(),
                    h5("Weight Settings"),
                    accordion(
                      open = FALSE,
                      class="weight-accordion",
                      accordion_panel("Findability", create_weight_inputs("Findability", "w_find")),
                      accordion_panel("Accessibility", create_weight_inputs("Accessibility", "w_acc")),
                      accordion_panel("Interoperability", create_weight_inputs("Interoperability", "w_inter")),
                      accordion_panel("Reusability", create_weight_inputs("Reusability", "w_reu")),
                      accordion_panel("Contextuality", create_weight_inputs("Contextuality", "w_cont"))
                    ),
                    
                    hr(),
                    h5("Downloads"),
                    downloadButton("download_plot_overall", "Overall Score Barplot (PNG)", 
                                   class = "w-100 mb-2"),
                    downloadButton("download_stacked_plot_png", "Stacked Bar Plot (PNG)",
                                   class = "w-100 mb-2"),
                    downloadButton("download_plot_dot", "Dot Plot (PNG)",
                                   class = "w-100 mb-2"),
                    downloadButton("download_plot_indicators", "Indicator Bar Plots (PNG)",
                                   class = "w-100 mb-2"),
                    downloadButton("download_plot_datasets", "Dataset Bar Plots (PNG)",
                                   class = "w-100 mb-2"),
                    downloadButton("download_score_table_csv", "Score Table (CSV)",
                                   class = "w-100 mb-2"),
                    downloadButton("download_all_zip", "Score Table and Plots (ZIP)",
                                   class = "w-100 special-zip-button")
                )),
              
              navset_card_underline(
                nav_panel("Score Table", card_body(tableOutput("score_table"))),
                nav_panel("Overall Score", card_body(plotOutput("barplot_overall",height = "600px"))),
                nav_panel("Stacked Bar Plot", card_body(plotOutput("stacked_bar_plot", height = "600px"))),
                nav_panel("Dot Plot", card_body(plotOutput("dotplot", height = "600px"))),
                nav_panel("Indicator Bar Plots", card_body(plotOutput("indicator_bar_plots", height = "600px"))),
                nav_panel("Dataset Bar Plots", card_body(plotOutput("dataset_bar_plots", height = "600px")))
              )
            )
  )
),

footer_html
)

# ==============================================================================
# 4. Server
# ==============================================================================

server <- function(input, output, session) {
  
 
  # Reactive value to hold the path to the current data file (example or uploaded)
  current_file_path <- reactiveVal(NULL)
  
  # Load example dataset from the app directory
  observe({
    example_path <- file.path(getwd(), "example_data.csv")
    if (file.exists(example_path)) {
      current_file_path(example_path)
      # Ensure delimiter defaults to comma for the example
      updateRadioButtons(session, "sep", selected = ",")
    }
  })
  
  # Switch to the uploaded file
  observeEvent(input$file, {
    req(input$file)
    current_file_path(input$file$datapath)
  })
  
  
  # Render sample table for 'Coding Scheme' panel
    output$sample_data_table <- renderTable({
    data.frame(
      id = c("1", "2", "3"),
      doi_available = c("0", "1", "1"),
      keywords_assigned = c("1", "0", "1"),
      categories_assigned = c("1", "0", "1"),
      temporal_info = c("1", "0", "1"),
      spatial_info = c("1", "1", "1"),
      access_url_accessible = c("1", "1", "0"),
      download_url_given = c("1", "1", "0"),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")
  
  
  
  # Coding template CSV download ('Coding Scheme' panel)
  output$download_coding_template <- downloadHandler(
    filename = function() {
      "coding_template.csv"
    },
    content = function(file) {
      # Path to the template file in the same directory as the app
      template_path <- file.path(getwd(), "coding_template.csv")
      if (!file.exists(template_path)) {
        stop("The coding_template.csv file was not found in the application directory.")
      }
      file.copy(template_path, file)
    }
  )
  
  
  # Read and process CSV data (example_data or uploaded user file)
  metadata <- reactive({
    req(current_file_path())
    df <- read_delim(current_file_path(), delim = input$sep, show_col_types = FALSE)
    if ("id" %in% colnames(df)) {
      # Use existing "id" column and preserve original order
      ids <- as.character(df$id)
      df$dataset <- factor(ids, levels = unique(ids))
    } else {
      # Generate ids if "id" column is missing
      labels <- paste("Dataset", seq_len(nrow(df)))
      df$dataset <- factor(labels, levels = labels)
    }
    df
  })
  
  # Calculate scores based on user-defined weights
  scored_data <- reactive({
    req(metadata())
    user_weights <- list(
      Findability = sapply(1:length(indicators$Findability), function(i) input[[paste0("w_find_", i)]]),
      Accessibility = sapply(1:length(indicators$Accessibility), function(i) input[[paste0("w_acc_", i)]]),
      Interoperability = sapply(1:length(indicators$Interoperability), function(i) input[[paste0("w_inter_", i)]]),
      Reusability = sapply(1:length(indicators$Reusability), function(i) input[[paste0("w_reu_", i)]]),
      Contextuality = sapply(1:length(indicators$Contextuality), function(i) input[[paste0("w_cont_", i)]])
    )
    assess_metadata_quality(metadata(), weights = user_weights)
  })
  
  # --- Output: Table and Visualizations ---
  
  # Render 'Score Table' 
  output$score_table <- renderTable({
    req(scored_data())
    scored_data() %>% select(dataset, Overall_score, ends_with("_score"))
  })
  
  # Render 'Overall Score' bar plot
  output$barplot_overall <- renderPlot({
    req(scored_data())
    df <- scored_data()
    ggplot(df, aes(x = dataset, y = Overall_score)) +
      geom_bar(stat = "identity", fill = "#0d0887") +
      theme_minimal() +
      labs(title = "Overall Extrinsic DQ Score per Dataset", x = "Dataset", y = "Overall Score") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render 'Stacked Bar Plot' 
  output$stacked_bar_plot <- renderPlot({
    req(scored_data())
    df <- scored_data() %>%
      select(dataset, ends_with("_score")) %>%
      pivot_longer(-dataset, names_to = "Dimension", values_to = "Score") %>%
      filter(Dimension != "Overall_score")
    df$Dimension <- factor(df$Dimension, levels = indicator_order)
    ggplot(df, aes(x = dataset, y = Score, fill = Dimension)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = indicator_colors) +
      theme_minimal() +
      labs(title = "Stacked Bar Plot: Indicator Scores per Dataset", x = "Dataset", y = "Score") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render 'Dot Plot' 
  output$dotplot <- renderPlot({
    req(scored_data())
    df <- scored_data() %>%
      select(dataset, ends_with("_score")) %>%
      pivot_longer(-dataset, names_to = "Dimension", values_to = "Score")
    df$Dimension <- factor(df$Dimension, levels = dimension_order)
    ggplot(df, aes(x = Dimension, y = dataset, color = Dimension)) +
      geom_point(aes(size = Score)) +
      scale_color_manual(values = indicator_colors) +
      theme_minimal() +
      labs(title = "Dot Plot: Indicator Scores per Dataset", x = "Dimension", y = "Dataset") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render 'Indicator Bar Plots
  output$indicator_bar_plots <- renderPlot({
    req(scored_data())
    df <- scored_data() %>%
      select(dataset, ends_with("_score")) %>%
      pivot_longer(-dataset, names_to = "Dimension", values_to = "Score") %>%
      filter(Dimension != "Overall_score")
    df$Dimension <- factor(df$Dimension, levels = indicator_order)
    ggplot(df, aes(x = dataset, y = Score, fill = Dimension)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Dimension, scales = "free_y") +
      scale_fill_manual(values = indicator_colors, guide = "none") +
      theme_minimal() +
      labs(title = "Indicator Scores Across Datasets", x = "Dataset", y = "Score") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render 'Dataset Bar Plots'
  output$dataset_bar_plots <- renderPlot({
    req(scored_data())
    df <- scored_data() %>%
      select(dataset, ends_with("_score")) %>%
      pivot_longer(-dataset, names_to = "Dimension", values_to = "Score") 
    df$Dimension <- factor(df$Dimension, levels = dimension_order)
    ggplot(df, aes(x = Dimension, y = Score, fill = Dimension)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ dataset, scales = "free_y") +
      scale_fill_manual(values = indicator_colors, guide = "none") +
      theme_minimal() +
      labs(title = "Indicator Scores per Dataset", x = "Dimension", y = "Score") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
##Download handlers
  
  # 1. Overall score plot (PNG)
  output$download_plot_overall <- downloadHandler(
    filename = function() {
      paste0("overall_score_barplot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(scored_data())
      df <- scored_data()
      p <- ggplot(df, aes(x = dataset, y = Overall_score)) +
        geom_bar(stat = "identity", fill = "#0d0887") +
        theme_minimal() +
        labs(
          title = "Overall Extrinsic DQ Score per Dataset",
          x = "Dataset", y = "Overall Score"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      png(file, width = 2000, height = 1200, res = 200)
      print(p)
      dev.off()
    }
  )
  
  # 2. Stacked bar plot (PNG)
  output$download_stacked_plot_png <- downloadHandler(
    filename = function() {
      paste0("stacked_bar_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(scored_data())
      df <- scored_data() %>%
        select(dataset, ends_with("_score")) %>%
        pivot_longer(-dataset, names_to = "Dimension", values_to = "Score") %>%
        filter(Dimension != "Overall_score")
      df$Dimension <- factor(df$Dimension, levels = indicator_order)
      p <- ggplot(df, aes(x = dataset, y = Score, fill = Dimension)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = indicator_colors) +
        theme_minimal() +
        labs(
          title = "Stacked Bar Plot: Indicator Scores per Dataset",
          x = "Dataset", y = "Score"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      png(file, width = 2000, height = 1200, res = 200)
      print(p)
      dev.off()
    }
  )
  
  # 3. Dot plot (PNG)
  output$download_plot_dot <- downloadHandler(
    filename = function() {
      paste0("dot_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(scored_data())
      df <- scored_data() %>%
        select(dataset, ends_with("_score")) %>%
        pivot_longer(-dataset, names_to = "Dimension", values_to = "Score")
      df$Dimension <- factor(df$Dimension, levels = dimension_order)
      p <- ggplot(df, aes(x = Dimension, y = dataset, color = Dimension)) +
        geom_point(aes(size = Score)) +
        scale_color_manual(values = indicator_colors) +
        theme_minimal() +
        labs(
          title = "Dot Plot: Indicator Scores per Dataset",
          x = "Dimension", y = "Dataset"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      png(file, width = 2000, height = 1200, res = 200)
      print(p)
      dev.off()
    }
  )
  
  # 4. Indicator bar plots (PNG)
  output$download_plot_indicators <- downloadHandler(
    filename = function() {
      paste0("indicator_bar_plots_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(scored_data())
      df <- scored_data() %>%
        select(dataset, ends_with("_score")) %>%
        pivot_longer(-dataset, names_to = "Dimension", values_to = "Score") %>%
        filter(Dimension != "Overall_score")
      df$Dimension <- factor(df$Dimension, levels = indicator_order)
      p <- ggplot(df, aes(x = dataset, y = Score, fill = Dimension)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ Dimension, scales = "free_y") +
        scale_fill_manual(values = indicator_colors, guide = "none") +
        theme_minimal() +
        labs(
          title = "Indicator Scores Across Datasets",
          x = "Dataset", y = "Score"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      png(file, width = 2400, height = 1600, res = 200)
      print(p)
      dev.off()
    }
  )
  
  # 5. Dataset bar plots (PNG)
  output$download_plot_datasets <- downloadHandler(
    filename = function() {
      paste0("dataset_bar_plots_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(scored_data())
      df <- scored_data() %>%
        select(dataset, ends_with("_score")) %>%
        pivot_longer(-dataset, names_to = "Dimension", values_to = "Score")
      df$Dimension <- factor(df$Dimension, levels = dimension_order)
      p <- ggplot(df, aes(x = Dimension, y = Score, fill = Dimension)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ dataset, scales = "free_y") +
        scale_fill_manual(values = indicator_colors, guide = "none") +
        theme_minimal() +
        labs(
          title = "Indicator Scores per Dataset",
          x = "Dimension", y = "Score"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      png(file, width = 2400, height = 1600, res = 200)
      print(p)
      dev.off()
    }
  )
  
  # 6. Score table (CSV)
  output$download_score_table_csv <- downloadHandler(
    filename = function() {
      paste0("score_table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(scored_data())
      table_df <- scored_data() %>%
        select(dataset, Overall_score, ends_with("_score"))
      write.csv(table_df, file, row.names = FALSE)
    }
  )
  
  # 7. Score table + plots (ZIP)
  output$download_all_zip <- downloadHandler(
    filename = function() {
      paste0("dq_scores_and_plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(scored_data())
      # Create a temporary directory
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)
      
      files_to_zip <- c()
      
      # ---- CSV: score table ----
      table_df <- scored_data() %>%
        select(dataset, Overall_score, ends_with("_score"))
      score_csv <- "score_table.csv"
      write.csv(table_df, score_csv, row.names = FALSE)
      files_to_zip <- c(files_to_zip, score_csv)
      
      # Save a plot object as PNG in tempdir
      save_plot_png <- function(p, fname, width = 2000, height = 1200, res = 200) {
        png(fname, width = width, height = height, res = res)
        print(p)
        dev.off()
        files_to_zip <<- c(files_to_zip, fname)
      }
      
      # ---- Overall score bar plot ----
      df_overall <- scored_data()
      p_overall <- ggplot(df_overall, aes(x = dataset, y = Overall_score)) +
        geom_bar(stat = "identity", fill = "#0d0887") +
        theme_minimal() +
        labs(
          title = "Overall Extrinsic DQ Score per Dataset",
          x = "Dataset", y = "Overall Score"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      save_plot_png(p_overall, "overall_score_barplot.png")
      
      # ---- Stacked bar plot ----
      df_stack <- scored_data() %>%
        select(dataset, ends_with("_score")) %>%
        pivot_longer(-dataset, names_to = "Dimension", values_to = "Score") %>%
        filter(Dimension != "Overall_score")
      df_stack$Dimension <- factor(df_stack$Dimension, levels = indicator_order)
      p_stack <- ggplot(df_stack, aes(x = dataset, y = Score, fill = Dimension)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = indicator_colors) +
        theme_minimal() +
        labs(
          title = "Stacked Bar Plot: Indicator Scores per Dataset",
          x = "Dataset", y = "Score"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      save_plot_png(p_stack, "stacked_bar_plot.png")
      
      # ---- Dot plot ----
      df_dot <- scored_data() %>%
        select(dataset, ends_with("_score")) %>%
        pivot_longer(-dataset, names_to = "Dimension", values_to = "Score")
      df_dot$Dimension <- factor(df_dot$Dimension, levels = dimension_order)
      p_dot <- ggplot(df_dot, aes(x = Dimension, y = dataset, color = Dimension)) +
        geom_point(aes(size = Score)) +
        scale_color_manual(values = indicator_colors) +
        theme_minimal() +
        labs(
          title = "Dot Plot: Indicator Scores per Dataset",
          x = "Dimension", y = "Dataset"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      save_plot_png(p_dot, "dot_plot.png")
      
      # ---- Indicator bar plots ----
      df_ind <- scored_data() %>%
        select(dataset, ends_with("_score")) %>%
        pivot_longer(-dataset, names_to = "Dimension", values_to = "Score") %>%
        filter(Dimension != "Overall_score")
      df_ind$Dimension <- factor(df_ind$Dimension, levels = indicator_order)
      p_ind <- ggplot(df_ind, aes(x = dataset, y = Score, fill = Dimension)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ Dimension, scales = "free_y") +
        scale_fill_manual(values = indicator_colors, guide = "none") +
        theme_minimal() +
        labs(
          title = "Indicator Scores Across Datasets",
          x = "Dataset", y = "Score"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      save_plot_png(p_ind, "indicator_bar_plots.png", width = 2400, height = 1600)
      
      # ---- Dataset bar plots ----
      df_ds <- scored_data() %>%
        select(dataset, ends_with("_score")) %>%
        pivot_longer(-dataset, names_to = "Dimension", values_to = "Score")
      df_ds$Dimension <- factor(df_ds$Dimension, levels = dimension_order)
      p_ds <- ggplot(df_ds, aes(x = Dimension, y = Score, fill = Dimension)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ dataset, scales = "free_y") +
        scale_fill_manual(values = indicator_colors, guide = "none") +
        theme_minimal() +
        labs(
          title = "Indicator Scores per Dataset",
          x = "Dimension", y = "Score"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      save_plot_png(p_ds, "dataset_bar_plots.png", width = 2400, height = 1600)
      
      # ---- Create ZIP ----
      zip::zip(zipfile = file, files = files_to_zip)
    }
  )
  
  
  
  
}


shinyApp(ui = ui, server = server)