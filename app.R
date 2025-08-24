# app.R

# ---- Repos seguro + auto-instalación de paquetes ----
options(repos = c(CRAN = "https://cran.r-project.org"))

pkgs <- c(
  "shiny","shinydashboard","shinydashboardPlus","shinyjs","shinycssloaders",
  "shinyWidgets","readxl","EGAnet","ggplot2","dplyr","tibble","openxlsx",
  "parallel","dashboardthemes","ggstats","psych","jsonlite","httr","officer",
  "digest","commonmark"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

# ---- Helpers utilizados en todo el script (definir ANTES) ----
`%||%` <- function(a, b) if (!is.null(a) && length(a)) a else b
compact <- function(x) {
  nulls <- vapply(x, is.null, logical(1))
  x[!nulls]
}

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(readxl)
library(EGAnet)
library(ggplot2)
library(dplyr)
library(tibble)
library(openxlsx)
library(parallel)
library(dashboardthemes)
library(ggstats)
library(psych)
library(jsonlite)
library(httr)
library(officer)  # For Word document generation
library(digest)   # For caching

# Function to generate R code for each analysis
generate_r_code <- function(analysis_type, params, data_info) {
  code <- paste0(
    "# ExGraf Analysis - R Code\n",
    "# Generated on: ", Sys.Date(), "\n\n",
    "# Load required libraries\n",
    "library(EGAnet)\n",
    "library(dplyr)\n",
    "library(ggplot2)\n\n",
    "# Load your data\n",
    "# Replace 'your_data.csv' with your actual file path\n",
    "data <- read.csv('your_data.csv')\n\n"
  )

  if (!is.null(data_info$group_col)) {
    code <- paste0(code,
                   "# Separate group variable\n",
                   "group_var <- data$", data_info$group_col, "\n",
                   "data <- data[, !names(data) %in% '", data_info$group_col, "']\n\n"
    )
  }

  if (length(data_info$removed_items) > 0) {
    code <- paste0(code,
                   "# Remove selected items\n",
                   "data <- data[, !names(data) %in% c(",
                   paste0("'", data_info$removed_items, "'", collapse = ", "), ")]\n\n"
    )
  }

  code <- paste0(code, "# Ensure all data is numeric\n",
                 "data <- data %>% mutate(across(everything(), as.numeric))\n\n")

  switch(analysis_type,
         "ega" = {
           code <- paste0(code,
                          "# Run EGA Analysis\n",
                          "ega_result <- EGA(\n",
                          "  data = data,\n",
                          "  corr = '", params$corr, "',\n",
                          "  model = '", params$model, "',\n",
                          "  algorithm = '", params$algorithm, "',\n",
                          "  plot.EGA = TRUE"
           )

           if (params$algorithm == "leiden") {
             code <- paste0(code,
                            ",\n  resolution_parameter = ", params$resolution_parameter,
                            ",\n  objective_function = '", params$objective_function, "'"
             )
           }

           code <- paste0(code, "\n)\n\n",
                          "# View results\n",
                          "print(ega_result)\n",
                          "plot(ega_result)\n\n",
                          "# Get network loadings\n",
                          "network_loadings <- net.loads(ega_result)\n",
                          "print(network_loadings$std)\n"
           )
         },

         "bootega" = {
           code <- paste0(code,
                          "# Run Bootstrap EGA (Reliability Analysis)\n",
                          "set.seed(", params$seed, ")\n",
                          "boot_result <- bootEGA(\n",
                          "  data = data,\n",
                          "  iter = ", params$iter, ",\n",
                          "  model = '", params$model, "',\n",
                          "  corr = '", params$corr, "',\n",
                          "  algorithm = '", params$algorithm, "',\n",
                          "  type = '", params$type, "',\n",
                          "  seed = ", params$seed
           )

           if (params$algorithm == "leiden") {
             code <- paste0(code,
                            ",\n  resolution_parameter = ", params$resolution_parameter,
                            ",\n  objective_function = '", params$objective_function, "'"
             )
           }

           code <- paste0(code, "\n)\n\n",
                          "# Get dimension stability\n",
                          "stability <- dimensionStability(boot_result)\n",
                          "print(stability$dimension.stability$structural.consistency)\n\n",
                          "# Plot item stability\n",
                          "plot(boot_result$stability$item.stability$plot)\n"
           )
         },

         "invariance" = {
           code <- paste0(code,
                          "# IMPORTANT: Ensure group order matches app\n",
                          "# Sort groups alphabetically to ensure consistency\n",
                          "unique_groups <- sort(unique(group_var))\n",
                          "group_var <- factor(group_var, levels = unique_groups)\n",
                          "group_var <- as.character(group_var)\n\n",
                          "# Run Measurement Invariance Analysis\n",
                          "set.seed(", params$seed, ")\n",
                          "RNGkind(sample.kind = 'Rounding')\n\n",
                          "invariance_result <- invariance(\n",
                          "  data = data,\n",
                          "  group = group_var,\n",
                          "  corr = '", params$corr, "',\n",
                          "  model = '", params$model, "',\n",
                          "  algorithm = '", params$algorithm, "',\n",
                          "  iter = ", params$iter, ",\n",
                          "  seed = ", params$seed, ",\n",
                          "  uni.method = 'LE',\n",
                          "  configural.type = 'resampling',\n",
                          "  configural.threshold = ", params$configural_threshold, ",\n",
                          "  ncores = 1,\n",
                          "  verbose = FALSE,\n",
                          "  loading.method = 'revised'"
           )

           if (params$algorithm == "leiden") {
             code <- paste0(code,
                            ",\n  resolution_parameter = ", params$resolution_parameter,
                            ",\n  objective_function = '", params$objective_function, "'"
             )
           }

           code <- paste0(code, "\n)\n\n",
                          "# View results\n",
                          "print(invariance_result$results)\n\n",
                          "# Plot invariance\n",
                          "plot(invariance_result, p_type = '", params$p_type,
                          "', p_value = ", params$p_value, ")\n\n",
                          "# Save results for comparison if needed\n",
                          "# saveRDS(invariance_result, 'invariance_results_r.rds')\n"
           )
         },

         "hierega" = {
           code <- paste0(code,
                          "# Run Hierarchical EGA\n",
                          "hier_result <- hierEGA(\n",
                          "  data = data,\n",
                          "  scores = 'network',\n",
                          "  plot.EGA = TRUE\n",
                          ")\n\n",
                          "# View results\n",
                          "print(hier_result)\n",
                          "plot(hier_result)\n"
           )
         },

         "wording" = {
           code <- paste0(code,
                          "# Run Wording Effects Analysis\n",
                          "set.seed(", params$seed, ")\n",
                          "wording_result <- riEGA(\n",
                          "  data = data,\n",
                          "  corr = '", params$corr, "',\n",
                          "  model = '", params$model, "',\n",
                          "  algorithm = '", params$algorithm, "',\n",
                          "  plot.EGA = TRUE,\n",
                          "  seed = ", params$seed, "\n",
                          ")\n\n",
                          "# View results\n",
                          "print(wording_result)\n",
                          "plot(wording_result)\n"
           )
         },

         "uva" = {
           code <- paste0(code,
                          "# Run Redundancy Analysis (UVA)\n",
                          "uva_result <- UVA(data)\n\n",
                          "# Console summary (wTO cutoffs and pairs):\n",
                          "summary(uva_result)\n\n",
                          "# Suggested removals by UVA (if any):\n",
                          "if (!is.null(uva_result$keep_remove$remove)) {\n",
                          "  cat('Remove redundant items:', paste(uva_result$keep_remove$remove, collapse = ', '), '\\n')\n",
                          "}\n"
           )
         }
  )

  return(code)
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$div("ExGraf", style = "font-weight:bold;")
  ),
  dashboardSidebar(
    shinyDashboardThemes(theme = "onenote"),
    useShinyjs(),
    fileInput(
      "file",
      "Upload Excel (.xlsx) or CSV (.csv)",
      accept = c(".xlsx", ".csv")
    ),
    uiOutput("groupColumnUI"),
    pickerInput(
      "removeItemsManual", "Select items to remove",
      choices = NULL, multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
    ),
    sidebarMenu(id = "tabs",
                menuItem("Introducción",          tabName = "intro",       icon = icon("info-circle")),
                menuItem("Item Analysis",         tabName = "item_analysis", icon = icon("chart-bar")),
                menuItem("Redundancy Analysis",   tabName = "uva",         icon = icon("redo")),        # ← MOVIDO
                menuItem("EGA Validation",        tabName = "ega",         icon = icon("project-diagram")),
                menuItem("Reliability",           tabName = "bootega",     icon = icon("chart-line")),
                menuItem("Measurement Invariance",tabName = "invariance",  icon = icon("balance-scale")),
                menuItem("Hierarchical Model",    tabName = "hier",        icon = icon("layer-group")),
                menuItem("Wording Effects",       tabName = "wording",     icon = icon("font")),
                menuItem("Generate Report",       tabName = "report",      icon = icon("file-alt")),
                menuItem("References",            tabName = "references",  icon = icon("book-open"))
    )

  ),
  dashboardBody(
    tabItems(
      # 0) Introducción
      tabItem(tabName = "intro",
              fluidRow(
                box(
                  title = "¡Welcome to ExGraf!", status = "primary", solidHeader = TRUE, width = 12,
                  HTML('
              <p><strong>ExGraf</strong> is a application designed to perform <strong>Exploratory Graph Analysis (EGA)</strong> to identify the dimensional structure of psychometric data using network-based methods.</p>

              <h4>What can you do with ExGraf?</h4>
              <ul>
                <li><strong>Data Import:</strong> Upload files in <code>.csv</code> or <code>.xlsx</code> format.</li>
                <li><strong>Item Analysis:</strong> Explore descriptive statistics and visualize response distributions using Likert plots.</li>
                <li><strong>EGA Validation:</strong> Analyze and visualize dimensions using networks.</li>
                <li><strong>Reliability:</strong> Assess internal consistency of the identified dimensions.</li>
                <li><strong>Measurement Invariance:</strong> Compare structures across groups.</li>
                <li><strong>Hierarchical Model:</strong> Explore relationships between first- and second-order dimensions.</li>
                <li><strong>Wording Effects:</strong> Detect reverse-worded items that may distort dimensionality.</li>
                <li><strong>Redundancy Analysis:</strong> Identify redundant items within your scale.</li>
                <li><strong>Export R Code:</strong> Generate reproducible R scripts for all analyses.</li>
              </ul>

              <p style="color:#a94442; font-weight:bold;">
              ⚠️ To properly run the Measurement Invariance, your dataset must include at least one <strong>categorical variable</strong> (e.g., <em>sex</em>).
              </p>

              <p>Don\'t have a dataset to test? Download a sample dataset here:</p>
            '),
                  downloadButton("descargar_ejemplo",
                                 label = HTML("<i class='fa fa-download'></i> Download sample dataset"),
                                 class = "btn btn-primary")
                )
              )
      ),

      # 00) Item Analysis
      tabItem(tabName = "item_analysis",
              fluidRow(
                box(title = "Likert Response Plot", width = 12, status = "primary", solidHeader = TRUE,
                    withSpinner(plotOutput("grafico_likert"), type = 4),
                    br(),
                    h5("Download Settings"),
                    numericInput("widthLikert",  "Width (inches):",  8, min = 1),
                    numericInput("heightLikert", "Height (inches):", 6, min = 1),
                    numericInput("dpiLikert",    "Resolution (dpi):", 300, min = 50)
                )
              ),
              fluidRow(
                downloadBttn("download_likertplot",   "Download Likert Plot",       style = "jelly", color = "success"),
                downloadBttn("download_item_r_code",  "Download R Code",            style = "jelly", color = "royal")
              )
      ),

      # 1) EGA Validation
      tabItem(tabName = "ega",
              fluidRow(
                column(width = 4,
                       box(
                         title = "EGA Settings", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, width = NULL,
                         pickerInput("corr", "Correlation type",
                                     choices = c("auto"="cor_auto","Spearman"="spearman","Pearson"="pearson"),
                                     selected = "spearman"   # <- Spearman por defecto
                         ),
                         pickerInput("model", "Model", choices = c("glasso","TMFG")),
                         pickerInput("algorithm", "Algorithm",
                                     choices = c("louvain","walktrap","leiden")),
                         conditionalPanel("input.algorithm == 'leiden'",
                                          sliderInput("resolution_parameter","Resolution Parameter",
                                                      value = 0.05, min = 0, max = 1, step = 0.01),
                                          pickerInput("objective_function","Objective Function",
                                                      choices = c("CPM","modularity"))
                         ),
                         actionBttn("runEGA","Run EGA", style="stretch", color="success", icon=icon("play")),
                         hr(),
                         h5("Download Settings"),
                         numericInput("widthEGA", "Width (inches):", 8, min = 1),
                         numericInput("heightEGA","Height (inches):",6, min = 1),
                         numericInput("dpiEGA",   "Resolution (dpi):",300, min = 50)
                       ),
                       box(
                         title = "Informative Table", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, width = NULL,
                         withSpinner(tableOutput("informativeTable"), type = 3, color.background = "#FFFFFF")
                       )
                ),
                column(width = 8,
                       withSpinner(plotOutput("plotEGA", height = "600px"), type = 6),
                       br(),
                       withSpinner(tableOutput("networkLoads"), type = 3, color.background = "#FFFFFF")
                )
              ),
              fluidRow(
                downloadBttn("downloadEGAPlot",           "Download EGA Plot",           style="jelly", color="primary"),
                downloadBttn("downloadNetworkLoads",     "Download Network Loads",      style="jelly", color="royal"),
                downloadBttn("downloadInformativeTable", "Download Informative Table",  style="jelly", color="success"),
                downloadBttn("downloadEGARCode",         "Download R Code",             style="jelly", color="danger")
              )
      ),

      # 2) Reliability
      tabItem(tabName = "bootega",
              fluidRow(
                column(width = 4,
                       box(
                         title="BootEGA Settings", status="warning", solidHeader=TRUE,
                         collapsible=TRUE, width=NULL,
                         numericInput("iter","Iterations",value=100,min=1),
                         numericInput("seed","Seed",value=2025,min=1),
                         pickerInput("type","Bootstrapping type",choices=c("resampling","parametric")),
                         actionBttn("runBootEGA","Run Reliability",style="stretch", color="warning", icon=icon("play")),
                         hr(),
                         h5("Download Settings"),
                         numericInput("widthBoot","Width (inches):",8, min=1),
                         numericInput("heightBoot","Height (inches):",6, min=1),
                         numericInput("dpiBoot","Resolution (dpi):",300, min=50)
                       )
                ),
                column(width = 8,
                       withSpinner(plotOutput("plotBootEGA", height = "600px"), type = 6),
                       br(),
                       withSpinner(tableOutput("structuralConsistency"), type = 3, color.background = "#FFFFFF")
                )
              ),
              fluidRow(
                downloadBttn("downloadBootEGAPlot",          "Download Item Stability Plot",     style="jelly", color="warning"),
                downloadBttn("downloadStructuralConsistency","Download Structural Consistency",  style="jelly", color="success"),
                downloadBttn("downloadBootEGARCode",         "Download R Code",                  style="jelly", color="danger")
              )
      ),

      # 3) Measurement Invariance
      tabItem(tabName = "invariance",
              fluidRow(
                column(width = 4,
                       box(
                         title = "Invariance Settings",
                         status = "danger",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = NULL,
                         pickerInput("p_type", "p adjustment",
                                     choices = c("BH" = "p_BH", "none" = "p")),
                         numericInput("p_value", "p-value",
                                      value = 0.05, step = 0.01),
                         numericInput("configural_threshold", "Configural threshold",
                                      value = 0.70, step = 0.01),
                         numericInput("iterInv", "Iterations:",
                                      value = 1000, min = 1),
                         numericInput("seedInv", "Seed:",
                                      value = 2025, min = 1),
                         actionBttn("runInvariance", "Run Invariance",
                                    style = "stretch", color = "danger", icon = icon("play")),
                         hr(),
                         h5("Download Settings"),
                         numericInput("widthInv", "Width (inches):", 8, min = 1),
                         numericInput("heightInv", "Height (inches):", 6, min = 1),
                         numericInput("dpiInv", "Resolution (dpi):", 300, min = 50)
                       )
                ),
                column(width = 8,
                       withSpinner(plotOutput("invariancePlot", height = "600px"), type = 6),
                       br(),
                       withSpinner(tableOutput("invarianceTable"), type = 3, color.background = "#FFFFFF")
                )
              ),
              fluidRow(
                downloadBttn("downloadInvariancePlot", "Download Invariance Plot",
                             style = "jelly", color = "danger"),
                downloadBttn("downloadInvarianceTable", "Download Invariance Table",
                             style = "jelly", color = "success"),
                downloadBttn("downloadInvarianceRCode", "Download R Code",
                             style = "jelly", color = "primary")
              )
      ),

      # 4) Hierarchical Model
      tabItem(tabName = "hier",
              fluidRow(
                column(width = 4,
                       box(
                         title="Hierarchical EGA", status="info", solidHeader=TRUE,
                         collapsible=TRUE, width=NULL,
                         helpText("Calculates hierEGA automatically."),
                         hr(),
                         h5("Download Settings"),
                         numericInput("widthHier","Width (inches):",8, min=1),
                         numericInput("heightHier","Height (inches):",6, min=1),
                         numericInput("dpiHier","Resolution (dpi):",300, min=50)
                       )
                ),
                column(width = 8,
                       withSpinner(plotOutput("plotHierEGA", height = "600px"), type = 6),
                       textOutput("hierEGAError")
                )
              ),
              fluidRow(
                downloadBttn("downloadHierEGAPlot","Download Hierarchical Plot", style="jelly", color="primary"),
                downloadBttn("downloadHierRCode",  "Download R Code",           style="jelly", color="danger")
              )
      ),

      # 5) Wording Effects
      tabItem(tabName = "wording",
              fluidRow(
                column(width = 4,
                       box(
                         title="Wording Effects", status="success", solidHeader=TRUE,
                         collapsible=TRUE, width=NULL,
                         actionBttn("runWordingEffects","Run Wording Effects",style="stretch", color="success", icon=icon("play")),
                         hr(),
                         h5("Download Settings"),
                         numericInput("widthWord","Width (inches):",8, min=1),
                         numericInput("heightWord","Height (inches):",6, min=1),
                         numericInput("dpiWord","Resolution (dpi):",300, min=50)
                       )
                ),
                column(width = 8,
                       uiOutput("wordingPlotUI"),
                       textOutput("wordingEffectsError")
                )
              ),
              fluidRow(
                uiOutput("downloadWordingUI"),
                downloadBttn("downloadWordingRCode", "Download R Code", style="jelly", color="danger")
              )
      ),

      # 6) Redundancy Analysis (UVA)
      tabItem(tabName = "uva",
              # 1) Primero: live summary (equivalente a summary(UVA(...)))
              fluidRow(
                column(width = 12,
                       box(
                         title = "UVA – Variable pairs by wTO cutoffs",
                         status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                         helpText("Salida equivalente a summary(UVA(datos_actuales)). Se actualiza al retirar ítems."),
                         withSpinner(verbatimTextOutput("uvaPairsVerbose"), type = 4)
                       )
                )
              ),

              fluidRow(
                column(width = 12,
                       box(
                         title = "UVA Summary",
                         status = "purple", solidHeader = TRUE, collapsible = FALSE, width = 12,
                         withSpinner(textOutput("uvaSuggest"), type = 4),
                         br(),
                         withSpinner(textOutput("uvaSummary"), type = 4)
                       )
                )
              ),

              fluidRow(
                downloadBttn("downloadUVARCode", "Download R Code", style = "jelly", color = "danger")
              )
      ),

      # 7) Generate Report
      tabItem(tabName = "report",
              fluidRow(
                box(
                  title = "Generate Analysis Report with ChatGPT", width = 12, status = "success", solidHeader = TRUE,
                  HTML('
              <p>This section allows you to automatically generate academic methods and results sections using ChatGPT API.</p>
              <p><strong>Steps:</strong></p>
              <ol>
                <li>Complete your analyses in the previous tabs</li>
                <li>Enter your OpenAI API key (get one at <a href="https://platform.openai.com/api-keys" target="_blank">OpenAI Platform</a>)</li>
                <li>Choose the language for your report</li>
                <li>Click "Generate Report" and wait for the results</li>
              </ol>
              <p><em>Note:</em> The app uses the fixed model <strong>GPT-4.1</strong>.</p>
            '),
                  br(),
                  fluidRow(
                    column(width = 6,
                           passwordInput("apiKey", "OpenAI API Key:",
                                         placeholder = "sk-...", width = "100%"),
                           helpText("Your API key is not stored and is only used for this session")
                    ),
                    column(width = 6,
                           selectInput("reportLanguage", "Report Language:",
                                       choices = c("English" = "en", "Spanish" = "es"),
                                       selected = "en", width = "100%")
                    )
                  ),
                  fluidRow(
                    column(width = 12,
                           actionBttn("generateReport", "Generate Report",
                                      style = "jelly", color = "success",
                                      icon = icon("magic"), size = "md"),
                           actionBttn("generateJSON", "Export JSON Only",
                                      style = "jelly", color = "primary",
                                      icon = icon("download"), size = "sm")
                    )
                  ),
                  br(),
                  # Debug section - always visible
                  box(
                    title = "Debug Info", width = NULL, status = "warning",
                    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    verbatimTextOutput("debugInfo")
                  ),
                  conditionalPanel(
                    condition = "output.reportGenerated == true",
                    box(
                      title = "Generated Report", width = NULL, status = "success", solidHeader = TRUE,
                      h4("Data Analysis Section"),
                      uiOutput("dataAnalysisSection"),
                      hr(),
                      h4("Results Section"),
                      uiOutput("resultsSection"),
                      br(),
                      downloadBttn("downloadReport", "Download Report (Text)",
                                   style = "jelly", color = "success"),
                      downloadBttn("downloadReportWord", "Download Report (Word)",
                                   style = "jelly", color = "royal")
                    )
                  ),
                  conditionalPanel(
                    condition = "output.jsonGenerated == true",
                    box(
                      title = "JSON Data Export", width = NULL, status = "info",
                      solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                      verbatimTextOutput("jsonPreview"),
                      br(),
                      downloadBttn("downloadJSON", "Download JSON", style = "jelly", color = "primary"),
                      br(), br(),
                      h5("R Code for API Integration"),
                      verbatimTextOutput("apiRCode")
                    )
                  )
                )
              )
      ),

      # 8) Referencias
      tabItem(tabName = "references",
              fluidRow(
                box(
                  title = "How to Cite ExGraf", width = 12, status = "primary", solidHeader = TRUE,
                  HTML('
              <p>If you use <strong>ExGraf</strong> in your research or teaching, please cite it as follows:</p>
              <p>Ventura-León, J., Lino-Cruz, C., Tocto-Muñoz, S., & Sánchez-Villena, A. R. (in development). ExGraf: A interface for accessible Exploratory Graph Analysis in psychological and behavioral research. GitHub Repository. <a href="https://github.com/jventural/ExGraf_app" target="_blank">https://github.com/jventural/ExGraf_app</a></p>
              <hr>
              <h4>Recommended References</h4>
              <ul>
                <li>Christensen, A. P., Garrido, L. E., & Guerra-Peña, K. (2024). Comparing community detection algorithms in psychometric networks: A Monte Carlo simulation. <em>Behavior Research Methods, 56</em>, 1485–1505. <a href="https://doi.org/10.3758/s13428-023-02106-4" target="_blank">https://doi.org/10.3758/s13428-023-02106-4</a></li>
                <li>Christensen, A. P., & Golino, H. (2021). Estimating the stability of psychological dimensions via Bootstrap Exploratory Graph Analysis: A Monte Carlo simulation and tutorial. <em>Psych, 3</em>(3), 479–500. <a href="https://doi.org/10.3390/psych3030032" target="_blank">https://doi.org/10.3390/psych3030032</a></li>
                <li>Golino, H. F., & Epskamp, S. (2017). Exploratory graph analysis: A new approach for estimating the number of dimensions in psychological research. <em>PLoS ONE, 12</em>(6), e0174035. <a href="https://doi.org/10.1371/journal.pone.0174035" target="_blank">https://doi.org/10.1371/journal.pone.0174035</a></li>
                <li>Jamison, L., Christensen, A. P., & Golino, H. F. (2024). Metric invariance in Exploratory Graph Analysis via permutation testing. <em>Methodology, 20</em>(2), 144–186. <a href="https://doi.org/10.5964/meth.12877" target="_blank">https://doi.org/10.5964/meth.12877</a></li>
                <li>Jiménez, M., Abad, F. J., García-Garzón, E., Golino, H., Christensen, A. P., & Garrido, L. E. (2023). Dimensionality assessment in bifactor structures with multiple general factors: A network psychometrics approach. <em>Psychological Methods</em>. Advance online publication. <a href="https://doi.org/10.1037/met0000590" target="_blank">https://doi.org/10.1037/met0000590</a></li>
              </ul>
            ')
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {

  # ===== Config =====
  DEFAULT_GPT_MODEL <- "gpt-4.1"  # modelo fijo

  # ---------- Utilidades de CPU (local / Posit Cloud seguro) ----------
  detect_cores_for_cloud <- function(fallback = 2L) {
    cores <- try(parallel::detectCores(logical = FALSE), silent = TRUE)
    if (inherits(cores, "try-error") || is.na(cores)) cores <- fallback
    is_cloud <- grepl("posit", tolower(Sys.getenv("RSTUDIO_PRODUCT"))) ||
      nzchar(Sys.getenv("RSC_PLATFORM")) || nzchar(Sys.getenv("CLOUD_PROJECT"))
    if (is_cloud) cores <- min(cores, 2L)
    slurm <- suppressWarnings(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = NA)))
    if (!is.na(slurm) && slurm > 0) cores <- min(cores, slurm)
    cores <- max(1L, as.integer(cores))
    return(cores)
  }

  # --- Helpers robustos para la API de OpenAI --------------------------------
  ping_openai <- function(api_key) {
    resp <- try(
      httr::RETRY(
        "GET",
        url = "https://api.openai.com/v1/models",
        httr::add_headers(Authorization = paste("Bearer", api_key)),
        httr::user_agent("ExGrafShiny/1.0 (httr)"),
        times = 3, pause_min = 1, pause_cap = 4,
        terminate_on = c(400, 401, 403, 404),
        httr::timeout(20)
      ),
      silent = TRUE
    )
    if (inherits(resp, "try-error")) return(FALSE)
    httr::status_code(resp) %in% c(200, 401)
  }

  openai_chat_retry <- function(model, prompt, api_key, max_tokens = 1500, temperature = 0.3, timeout_sec = 120) {
    do_call <- function(body_list) {
      httr::RETRY(
        "POST",
        url = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        body   = jsonlite::toJSON(body_list, auto_unbox = TRUE),
        encode = "json",
        httr::user_agent("ExGrafShiny/1.0 (httr)"),
        times = 4, pause_min = 1, pause_cap = 8,
        terminate_on = c(400, 401, 403, 404),
        httr::timeout(timeout_sec)
      )
    }

    body_list <- list(
      model = model,
      messages = list(
        list(role = "system", content = "You are an expert in psychometric analysis and academic writing."),
        list(role = "user",   content = prompt)
      ),
      temperature = temperature,
      max_tokens  = max_tokens
    )

    resp <- do_call(body_list)
    if (httr::status_code(resp) != 200) {
      cont <- try(httr::content(resp), silent = TRUE)
      msg  <- if (!inherits(cont, "try-error") && !is.null(cont$error$message)) cont$error$message else paste("HTTP", httr::status_code(resp))
      # Reintento sin 'temperature' si el modelo no lo soporta
      if (grepl("temperature.*does not support", tolower(msg))) {
        body_list$temperature <- NULL
        resp <- do_call(body_list)
      }
    }

    if (httr::status_code(resp) != 200) {
      cont <- try(httr::content(resp), silent = TRUE)
      msg  <- if (!inherits(cont, "try-error") && !is.null(cont$error$message)) cont$error$message else paste("HTTP", httr::status_code(resp))
      stop("OpenAI API error: ", msg)
    }

    httr::content(resp)$choices[[1]]$message$content
  }

  shrink_json_for_prompt <- function(json_text, target_chars = 2800) {
    jt <- try(jsonlite::fromJSON(json_text, simplifyVector = FALSE), silent = TRUE)
    if (inherits(jt, "try-error")) return(substr(json_text, 1, target_chars))
    if (!is.null(jt$reliability_results$item_stability)) jt$reliability_results$item_stability <- NULL
    if (!is.null(jt$ega_results$membership)) jt$ega_results$membership <- NULL
    if (!is.null(jt$likert_summary)) {
      nm <- names(jt$likert_summary)
      if (length(nm) > 6) jt$likert_summary <- jt$likert_summary[nm[1:6]]
    }
    out <- jsonlite::toJSON(jt, auto_unbox = TRUE, pretty = FALSE, digits = 3)
    if (nchar(out) <= target_chars) return(out)
    if (!is.null(jt$ega_results$network_loadings)) jt$ega_results$network_loadings <- NULL
    out2 <- jsonlite::toJSON(jt, auto_unbox = TRUE, pretty = FALSE, digits = 3)
    if (nchar(out2) <= target_chars) return(out2)
    substr(out2, 1, target_chars)
  }

  # === PASO 1: Mapa Idea ↔ Cita (queda en alcance del server y antes del prompt) ===
  citation_map <- list(
    rstudio_env   = list(
      idea = "El análisis se realizó en R dentro de RStudio.",
      cite = "(RStudio Team, 2023)"
    ),
    eganet_pkg    = list(
      idea = "EGAnet se usó para EGA y bootEGA.",
      cite = "(Golino & Christensen, 2021)"
    ),
    psychonetrics = list(
      idea = "psychonetrics se usó para modelos de red latente / SEM.",
      cite = "(Epskamp, 2024)"
    ),
    lct           = list(
      idea = "Se aplicó el Loading Comparison Test (LCT) para decidir entre marco latente vs marco de red.",
      cite = "(Christensen & Golino, 2021)"
    ),
    tidyverse     = list(
      idea = "Manipulación y limpieza de datos con tidyverse.",
      cite = "(Wickham et al., 2019)"
    ),
    gridextra     = list(
      idea = "Composición/maquetación de figuras con gridExtra.",
      cite = "(Auguie, 2017)"
    ),
    walktrap      = list(
      idea = "Detección de comunidades con el algoritmo walktrap (caminatas aleatorias).",
      cite = "(Garcia-Pardina et al., 2022)"
    ),
    spearman_ebic = list(
      idea = "Matriz de partida Spearman por robustez y mejor edge recovery en EBICglasso bajo asimetría, comparable/superior a policórica.",
      cite = "(Isvoranu & Epskamp, 2021)"
    ),
    netload_thr   = list(
      idea = "Umbrales de cargas de red .15/.25/.35 para pequeña/moderada/grande.",
      cite = "(Christensen & Golino, 2021)"
    ),
    bootega       = list(
      idea = "bootEGA con 1000 réplicas y algoritmo LE para consistencia estructural y estabilidad de ítems.",
      cite = "(Golino & Christensen, 2021)"
    ),
    crit_75       = list(
      idea = "Criterio de ≥ 75% de replicación como satisfactorio.",
      cite = "(Golino et al., 2021)"
    ),
    struct_cons   = list(
      idea = "La consistencia estructural se diferencia de la consistencia interna clásica.",
      cite = "(Christensen et al., 2020)"
    ),
    lnm           = list(
      idea = "Latent Network Modeling para explorar interacciones entre variables latentes.",
      cite = "(Epskamp et al., 2017)"
    ),
    cohen_cut     = list(
      idea = "Interpretación de correlaciones: .10/.30/.50 como pequeño/mediano/grande.",
      cite = "(Cohen, 1988)"
    ),
    bootnet_centr = list(
      idea = "Métricas de centralidad están disponibles en bootnet cuando corresponde.",
      cite = "(Epskamp, Borsboom, & Fried, 2018)"
    )
  )

  # ---- Helpers para HECHOS del reporte --------------------------------------
  compact_loadings_for_prompt <- function(ega_obj, top_per_dim = 5, max_rows = 30) {
    out <- list(present = FALSE, n_dims = NA, tefi = NA, top = list())
    if (is.null(ega_obj)) return(out)
    nl <- tryCatch(EGAnet::net.loads(ega_obj)$std, error = function(e) NULL)
    if (is.null(nl)) return(out)
    out$present <- TRUE
    out$n_dims  <- ega_obj$n.dim %||% NA
    out$tefi    <- ega_obj$TEFI %||% NA
    primary_dim <- apply(abs(nl), 1, which.max)
    primary_val <- mapply(function(i, d) nl[i, d], seq_len(nrow(nl)), primary_dim)
    df <- data.frame(item = rownames(nl), dimension = primary_dim, loading = round(primary_val, 3), stringsAsFactors = FALSE)
    split_list <- split(df, df$dimension)
    top_list <- lapply(split_list, function(dd) {
      dd <- dd[order(-abs(dd$loading)), , drop = FALSE]
      dd <- head(dd, top_per_dim)
      apply(dd, 1, as.list)
    })
    flat <- unlist(top_list, recursive = FALSE)
    if (length(flat) > max_rows) flat <- flat[seq_len(max_rows)]
    out$top <- flat
    out
  }

  # === UVA: datos antes y después de remoción manual (versión única) =========
  # (a) UVA con datos previos a la remoción manual (para sugerencias)
  data_before_manual <- reactive({
    req(data_bfi())
    df <- data_bfi()
    if (!is.null(input$groupColumn)) {
      df <- df %>% dplyr::select(-dplyr::all_of(input$groupColumn))
    }
    df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  })

  uva_pre_result <- reactive({
    req(data_before_manual())
    tryCatch(EGAnet::UVA(data_before_manual()), error = function(e) NULL)
  })

  fmt_list <- function(x) if (length(x)) paste(x, collapse = ", ") else "—"
  plural   <- function(n) ifelse(n == 1, "ítem", "ítems")

  # Mensaje de sugerencia (antes de retirar)
  output$uvaSuggest <- renderText({
    sug <- character(0)
    if (!is.null(uva_pre_result()) && !is.null(uva_pre_result()$keep_remove$remove)) {
      sug <- as.character(uva_pre_result()$keep_remove$remove)
    }
    if (length(sug) == 0) {
      "UVA no sugiere retirar ítems."
    } else {
      sprintf(
        "Sugerencia UVA: revisar posibles redundancias en %d %s: %s. Usa “Select items to remove” para retirarlos del análisis.",
        length(sug), plural(length(sug)), fmt_list(sug)
      )
    }
  })

  # (b) UVA con datos actuales (post-remoción) para mostrar summary(uva)
  uva_now_result <- reactive({
    req(filtered_data())
    df <- filtered_data() %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
    tryCatch(EGAnet::UVA(df), error = function(e) NULL)
  })

  # Bloque que imprime EXACTAMENTE la salida tipo summary(UVA(...))
  output$uvaPairsVerbose <- renderPrint({
    ur <- uva_now_result(); req(ur)
    summary(ur)
  })

  # Resumen de progreso vs sugerencias
  output$uvaSummary <- renderText({
    req(filtered_data())

    sug <- character(0)
    if (!is.null(uva_pre_result()) && !is.null(uva_pre_result()$keep_remove$remove)) {
      sug <- as.character(uva_pre_result()$keep_remove$remove)
    }
    man <- as.character(data_info$removed_items %||% character(0))

    if (length(sug) == 0 && length(man) == 0) {
      return("No se detectaron ítems redundantes ni se registraron remociones manuales.")
    }

    inter      <- intersect(man, sug)      # sugeridos y retirados
    pendientes <- setdiff(sug, man)        # sugeridos que siguen presentes
    extras     <- setdiff(man, sug)        # retirados que no fueron sugeridos

    if (length(sug) > 0 && length(pendientes) == 0 && length(extras) == 0) {
      return(sprintf(
        "Se retiraron %d %s y todos coincidieron con las sugerencias de UVA: %s.",
        length(inter), plural(length(inter)), fmt_list(inter)
      ))
    }

    parts <- c()
    if (length(sug)) {
      parts <- c(parts, sprintf("Progreso: retiraste %d/%d sugeridos por UVA%s.",
                                length(inter), length(sug),
                                if (length(pendientes)) paste0(" (pendientes: ", fmt_list(pendientes), ")") else ""))
    }
    if (length(extras)) {
      parts <- c(parts, sprintf("Adicionalmente retiraste %d %s no sugeridos por UVA: %s.",
                                length(extras), plural(length(extras)), fmt_list(extras)))
    }
    if (length(man) == 0) {
      parts <- c(parts, "Aún no has retirado ítems. Selecciona en “Select items to remove”.")
    } else {
      parts <- c(parts, sprintf("Estado actual: %d %s retirados: %s.",
                                length(man), plural(length(man)), fmt_list(man)))
    }

    paste(parts, collapse = "\n")
  })

  # ---- Notificaciones cuando cambian los ítems retirados (mantener) ----------
  prev_manual <- reactiveVal(character(0))
  observeEvent(input$removeItemsManual, ignoreInit = TRUE, {
    old <- prev_manual()
    new <- sort(as.character(input$removeItemsManual %||% character(0)))
    added   <- setdiff(new, old)   # ítems recién retirados
    restored <- setdiff(old, new)  # ítems reincorporados

    if (length(added)) {
      showNotification(
        paste0("Retiro confirmado (", length(added), "): ", fmt_list(added)),
        type = "message", duration = 6
      )
    }
    if (length(restored)) {
      showNotification(
        paste0("Reincorporaste (", length(restored), "): ", fmt_list(restored)),
        type = "warning", duration = 6
      )
    }
    prev_manual(new)
  })

  # --- Normalización y post-procesado de secciones ---------------------------
  norm_md <- function(x) {
    if (is.null(x) || !nzchar(x)) return("")
    x <- gsub("\r", "", x)
    x <- gsub("(?i)<br\\s*/?>", "\n", x, perl = TRUE)
    x <- gsub("(?i)</?p>", "\n\n", x, perl = TRUE)
    x <- gsub("(?i)</?strong>", "**", x, perl = TRUE)
    x <- gsub("(?i)</?em>", "*", x, perl = TRUE)
    x <- gsub("\u2014", "-", x, fixed = TRUE)  # em dash —
    x <- gsub("\u2013", "-", x, fixed = TRUE)  # en dash –
    trimws(x)
  }

  # NUEVO: forzar párrafo en “Parámetros técnicos y criterios”
  force_params_paragraph <- function(txt) {
    if (!nzchar(txt)) return(txt)
    rx_head <- "(?im)^\\s*#{0,3}\\s*Par[aá]metros\\s+t[ée]cnicos\\s+y\\s+criterios\\s*$"
    m <- regexpr(rx_head, txt, perl = TRUE)
    if (m[1] == -1) return(txt)
    start <- m[1] + attr(m, "match.length")
    rest  <- substr(txt, start, nchar(txt))
    next_head <- regexpr("(?m)^\\s*#{1,6}\\s+|^\\s*[A-Z].*:\\s*$", rest, perl = TRUE)
    end_pos <- if (next_head[1] == -1) nchar(rest) else next_head[1] - 1
    block <- substr(rest, 1, end_pos)
    bullets <- gregexpr("(?m)^[\\t ]*([\\-\\*•])\\s+(.+)$", block, perl = TRUE)
    if (bullets[[1]][1] == -1) return(txt)
    lines <- regmatches(block, bullets)[[1]]
    lines <- gsub("(?m)^[\\t ]*([\\-\\*•])\\s+", "", lines, perl = TRUE)
    lines <- trimws(lines)
    if (!length(lines)) return(txt)
    connectors <- c("Asimismo, ", "Además, ", "Por último, ")
    parts <- lines
    if (length(parts) >= 2) parts[2] <- paste0(connectors[1], parts[2])
    if (length(parts) >= 3) parts[3] <- paste0(connectors[2], parts[3])
    if (length(parts) >= 4) parts[length(parts)] <- paste0(connectors[3], parts[length(parts)])
    paragraph <- paste(parts, collapse = " ")
    paragraph <- paste0("\n\n", paragraph, "\n\n")
    new_rest <- paste0(paragraph, substr(rest, end_pos + 1, nchar(rest)))
    paste0(substr(txt, 1, start), new_rest)
  }

  # Inserción de marcadores e intro de “Cargas de red”
  inject_markers <- function(txt) {
    if (!nzchar(txt)) return(txt)
    s <- gsub("\r", "", txt)
    s <- gsub(
      "(?is)\\n?\\s*(?:#{1,6}\\s*|\\d+\\.\\s*)?(?:An[aá]lisis\\s+de\\s+)?Red\\s+latente\\s*\\n.*?(?=\\n\\s*(?:#{1,6}|\\d+\\.|Invarianza|An[aá]lisis\\s+adicionales|Conclusiones|Referencias)\\b|\\Z)",
      "\n", s, perl = TRUE
    )
    if (!grepl("(?im)^\\s*#*\\s*Resultados\\b", s, perl = TRUE)) {
      s <- paste0("# Resultados\n\n", s)
    }
    s
  }

  md_to_html <- function(x) {
    if (!nzchar(x)) return("")
    if (requireNamespace("commonmark", quietly = TRUE)) {
      conv <- try(commonmark::markdown_html(x, extensions = "table", smart = FALSE), silent = TRUE)
      if (!inherits(conv, "try-error")) return(conv)
      return(commonmark::markdown_html(x, smart = FALSE))
    } else {
      x <- gsub("&", "&amp;", x, fixed = TRUE)
      x <- gsub("<", "&lt;",  x, fixed = TRUE)
      x <- gsub(">", "&gt;",  x, fixed = TRUE)
      gsub("\n", "<br>", x)
    }
  }

  # Secciones finales (UI y Word usan lo mismo)
  report_output <- reactiveValues(data_analysis = NULL, results = NULL, error = NULL)
  final_sections <- reactive({
    analysis_md <- norm_md(report_output$data_analysis %||% "")
    analysis_md <- force_params_paragraph(analysis_md)
    list(
      analysis = analysis_md,
      results  = inject_markers(norm_md(report_output$results %||% ""))
    )
  })

  # Store data info for R code generation
  data_info <- reactiveValues(group_col = NULL, removed_items = character(0))

  # Report cache
  report_cache <- reactiveValues(json_hash = NULL, cached_analysis = NULL, cached_results = NULL)

  # ----- Ejemplo descarga -----------------------------------------------------
  output$descargar_ejemplo <- downloadHandler(
    filename = function() "Data_Rosenberg.xlsx",
    content  = function(file) {
      datos <- data.frame(
        sexo = rep(c("M", "F"), each = 5),
        item1 = rnorm(10),
        item2 = rnorm(10),
        item3 = rnorm(10),
        item4 = rnorm(10),
        item5 = rnorm(10)
      )
      openxlsx::write.xlsx(datos, file)
    }
  )

  # ----- Carga de datos -------------------------------------------------------
  data_bfi <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (tolower(ext) == "csv") {
      hdr <- readLines(input$file$datapath, n = 1)
      sep <- if (grepl(";", hdr)) ";" else ","
      read.csv(input$file$datapath, sep = sep, header = TRUE,
               stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      readxl::read_excel(input$file$datapath)
    }
  })

  output$groupColumnUI <- renderUI({
    req(data_bfi())
    selectInput("groupColumn","Select group column",choices = names(data_bfi()))
  })

  observeEvent(data_bfi(), {
    updatePickerInput(session,"removeItemsManual",
                      choices  = setdiff(names(data_bfi()), input$groupColumn),
                      selected = character(0))
  })

  observeEvent(input$groupColumn,      { data_info$group_col    <- input$groupColumn })
  observeEvent(input$removeItemsManual,{ data_info$removed_items <- input$removeItemsManual })

  # ----- Datos filtrados (post-remoción manual) ------------------------------
  filtered_data <- reactive({
    df <- data_bfi()
    if (!is.null(input$groupColumn))
      df <- df %>% dplyr::select(-dplyr::all_of(input$groupColumn))
    if (!is.null(input$removeItemsManual))
      df <- df %>% dplyr::select(-dplyr::all_of(input$removeItemsManual))
    df
  })

  # ----- Item Analysis --------------------------------------------------------
  output$grafico_likert <- renderPlot({
    req(filtered_data())
    ggstats::gglikert(filtered_data()) +
      labs(title = "Likert Response Distribution",
           x = "Percentage of responses",
           y = "Items") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$download_likertplot <- downloadHandler(
    filename = "likert_plot.jpg",
    content = function(file) {
      g <- ggstats::gglikert(filtered_data()) +
        theme_minimal(base_size = 12) +
        labs(title = "", x = "Percentage", y = "Items") +
        theme(legend.position = "bottom")
      ggsave(file, plot = g, width = input$widthLikert, height = input$heightLikert, dpi = input$dpiLikert, bg = "white")
    }
  )

  output$download_item_r_code <- downloadHandler(
    filename = "item_analysis_code.R",
    content = function(file) {
      code <- paste0(
        "# Item Analysis - R Code\n",
        "# Generated on: ", Sys.Date(), "\n\n",
        "library(ggstats)\nlibrary(ggplot2)\nlibrary(dplyr)\n\n",
        "data <- read.csv('your_data.csv')\n\n"
      )
      if (!is.null(data_info$group_col)) {
        code <- paste0(code, "data <- data[, !names(data) %in% '", data_info$group_col, "']\n\n")
      }
      if (length(data_info$removed_items) > 0) {
        code <- paste0(code, "data <- data[, !names(data) %in% c(",
                       paste0("'", data_info$removed_items, "'", collapse = ", "), ")]\n\n")
      }
      code <- paste0(code,
                     "likert_plot <- ggstats::gglikert(data) +\n",
                     "  labs(title='Likert Response Distribution', x='Percentage of responses', y='Items') +\n",
                     "  theme_minimal(base_size=12) + theme(legend.position='bottom')\n\n",
                     "print(likert_plot)\n"
      )
      writeLines(code, file)
    }
  )

  # ----- EGA ------------------------------------------------------------------
  ega_result <- eventReactive(input$runEGA, {
    req(filtered_data())
    ncores <- detect_cores_for_cloud()

    # seed es opcional en la UI de EGA; si no existe, no lo pasamos
    seed_val <- NULL
    if (!is.null(input$seed)) {
      tmp <- suppressWarnings(as.integer(input$seed))
      if (length(tmp) == 1 && !is.na(tmp)) seed_val <- tmp
    }

    args <- list(
      data      = filtered_data(),
      corr      = input$corr,
      model     = input$model,
      algorithm = input$algorithm,
      plot.EGA  = TRUE,
      seed      = seed_val,
      ncores    = ncores
    )
    if (identical(input$algorithm, "leiden")) {
      args$resolution_parameter <- input$resolution_parameter %||% NULL
      args$objective_function   <- input$objective_function   %||% NULL
    }
    tryCatch(
      do.call(EGAnet::EGA, compact(args)),
      error=function(e){ showNotification(e$message,type="error"); NULL }
    )
  })

  output$plotEGA <- renderPlot({
    res <- ega_result(); req(res)
    print(
      res$plot.EGA +
        theme_void() +
        annotate("text", x=Inf, y=-Inf,
                 label=paste0("TEFI: ", round(res$TEFI,3)),
                 hjust=1, vjust=-1)
    )
  })

  output$downloadEGAPlot <- downloadHandler(
    filename="EGA_plot.jpg",
    content=function(file){
      res <- ega_result(); req(res)
      ggsave(
        file,
        plot   = res$plot.EGA + theme_void(),
        width  = input$widthEGA,
        height = input$heightEGA,
        dpi    = input$dpiEGA,
        bg     = "white"
      )
    }
  )

  output$networkLoads <- renderTable({
    res <- ega_result(); req(res)
    EGAnet::net.loads(res)$std %>% as.data.frame() %>% tibble::rownames_to_column("Item")
  })

  convert_EGA_to_df <- function(res) {
    m    <- res$network
    meth <- attr(m,"methods")
    metrics <- list(
      Model                   = toupper(meth$model %||% NA),
      Correlations            = meth$corr %||% NA,
      Lambda                  = if(!is.null(meth$lambda)) formatC(meth$lambda,format="f",digits=3) else NA,
      `Number of nodes`       = nrow(m),
      `Number of edges`       = sum(m!=0)/2,
      `Edge density`          = formatC(mean(m!=0),format="f",digits=3),
      M                       = formatC(mean(m[m!=0]),format="f",digits=3),
      SD                      = formatC(sd(m[m!=0]),format="f",digits=3),
      Min                     = formatC(min(m[m!=0]),format="f",digits=3),
      Max                     = formatC(max(m[m!=0]),format="f",digits=3),
      `Number of communities` = res$n.dim,
      TEFI                    = formatC(res$TEFI,format="f",digits=3)
    )
    tibble::tibble(Index=names(metrics),Value=unlist(metrics))
  }

  output$informativeTable <- renderTable({
    res <- ega_result(); req(res)
    convert_EGA_to_df(res)
  })

  output$downloadNetworkLoads <- downloadHandler(
    filename="network_loads.xlsx",
    content=function(file){
      openxlsx::write.xlsx(
        EGAnet::net.loads(ega_result())$std %>% as.data.frame() %>% tibble::rownames_to_column("Item"),
        file
      )
    }
  )

  output$downloadInformativeTable <- downloadHandler(
    filename="informative_table.xlsx",
    content=function(file){
      openxlsx::write.xlsx(convert_EGA_to_df(ega_result()), file)
    }
  )

  output$downloadEGARCode <- downloadHandler(
    filename = "ega_analysis_code.R",
    content = function(file) {
      params <- list(
        corr = input$corr,
        model = input$model,
        algorithm = input$algorithm,
        resolution_parameter = input$resolution_parameter,
        objective_function = input$objective_function
      )
      code <- generate_r_code("ega", params, data_info)
      writeLines(code, file)
    }
  )

  # ----- Reliability (BootEGA) -----------------------------------------------
  bootEGA_res <- eventReactive(input$runBootEGA, {
    req(filtered_data())
    withProgress(message="Running reliability analysis...", value=0, {
      incProgress(0.1)
      ncores <- detect_cores_for_cloud()

      # seed está definido en UI de bootEGA
      seed_val <- NULL
      if (!is.null(input$seed)) {
        tmp <- suppressWarnings(as.integer(input$seed))
        if (length(tmp) == 1 && !is.na(tmp)) seed_val <- tmp
      }

      args <- list(
        data      = filtered_data(),
        iter      = as.numeric(input$iter),
        model     = input$model,
        corr      = input$corr,
        algorithm = input$algorithm,
        seed      = seed_val,
        type      = input$type,
        ncores    = ncores
      )
      if (identical(input$algorithm,"leiden")) {
        args$objective_function   <- input$objective_function   %||% NULL
        args$resolution_parameter <- input$resolution_parameter %||% NULL
      }
      incProgress(0.2)
      res <- tryCatch(
        do.call(EGAnet::bootEGA, compact(args)),
        error=function(e){
          showNotification(paste("Error in bootEGA:", e$message), type="error", duration=10)
          NULL
        }
      )
      incProgress(0.6)

      if(is.null(res)) return(NULL)

      sc <- tryCatch(
        EGAnet::dimensionStability(res),
        error = function(e) {
          showNotification(paste("Error in dimensionStability:", e$message), type="error", duration=10)
          NULL
        }
      )
      incProgress(0.1)

      if(!is.null(sc)) list(boot=res, sc=sc) else list(boot=res, sc=NULL)
    })
  })

  output$plotBootEGA <- renderPlot({
    br <- bootEGA_res()
    req(br, br$boot, br$boot$stability, br$boot$stability$item.stability, br$boot$stability$item.stability$plot)
    print(br$boot$stability$item.stability$plot + theme_minimal() + ggtitle("Item Stability"))
  })

  output$downloadBootEGAPlot <- downloadHandler(
    filename="item_stability_plot.jpg",
    content=function(file){
      br <- bootEGA_res()
      if(!is.null(br) && !is.null(br$boot$stability$item.stability$plot)) {
        ggsave(
          file,
          plot   = br$boot$stability$item.stability$plot + theme_minimal(),
          width  = input$widthBoot,
          height = input$heightBoot,
          dpi    = input$dpiBoot,
          bg     = "white"
        )
      }
    }
  )

  output$structuralConsistency <- renderTable({
    br <- bootEGA_res()
    req(br, br$sc, br$sc$dimension.stability, br$sc$dimension.stability$structural.consistency)
    sc <- br$sc
    data.frame(
      Dimension   = names(sc$dimension.stability$structural.consistency),
      Consistency = sc$dimension.stability$structural.consistency
    )
  })

  output$downloadStructuralConsistency <- downloadHandler(
    filename="structural_consistency.xlsx",
    content=function(file){
      br <- bootEGA_res()
      if(!is.null(br) && !is.null(br$sc) && !is.null(br$sc$dimension.stability$structural.consistency)) {
        df_sc <- data.frame(
          Dimension   = names(br$sc$dimension.stability$structural.consistency),
          Consistency = br$sc$dimension.stability$structural.consistency
        )
        openxlsx::write.xlsx(df_sc, file)
      }
    }
  )

  output$downloadBootEGARCode <- downloadHandler(
    filename = "bootega_analysis_code.R",
    content = function(file) {
      params <- list(
        corr = input$corr,
        model = input$model,
        algorithm = input$algorithm,
        iter = input$iter,
        seed = input$seed,
        type = input$type,
        resolution_parameter = input$resolution_parameter,
        objective_function = input$objective_function
      )
      code <- generate_r_code("bootega", params, data_info)
      writeLines(code, file)
    }
  )

  # ----- Measurement Invariance ----------------------------------------------
  invariance_res <- eventReactive(input$runInvariance, {
    req(data_bfi(), input$groupColumn)

    withProgress(message = "Running invariance analysis...", value = 0, {
      incProgress(0.1)

      df <- data_bfi()[, !names(data_bfi()) %in% input$groupColumn]
      if (!is.null(input$removeItemsManual)) {
        df <- df[, !names(df) %in% input$removeItemsManual]
      }

      df <- as.data.frame(df)
      df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
      df <- as.data.frame(as.matrix(df))

      grp <- as.character(data_bfi()[[input$groupColumn]])
      unique_groups <- sort(unique(grp))
      grp <- factor(grp, levels = unique_groups)
      grp <- as.character(grp)

      incProgress(0.2)

      seed_val <- NULL
      if (!is.null(input$seedInv)) {
        tmp <- suppressWarnings(as.integer(input$seedInv))
        if (length(tmp) == 1 && !is.na(tmp)) seed_val <- tmp
      }
      RNGkind(sample.kind = "Rounding")

      ncores_inv <- detect_cores_for_cloud()

      args_inv <- list(
        data = df,
        group = grp,
        corr = input$corr,
        model = input$model,
        algorithm = input$algorithm,
        iter = as.numeric(input$iterInv),
        seed = seed_val,
        uni.method = "LE",
        configural.type = "resampling",
        configural.threshold = as.numeric(input$configural_threshold),
        ncores = ncores_inv,
        verbose = FALSE,
        loading.method = "revised"
      )
      if (identical(input$algorithm, "leiden")) {
        args_inv$resolution_parameter <- input$resolution_parameter %||% NULL
        args_inv$objective_function   <- input$objective_function   %||% NULL
      }

      res <- tryCatch(
        do.call(EGAnet::invariance, compact(args_inv)),
        error = function(e) {
          showNotification(paste("Error in invariance analysis:", e$message), type = "error", duration = 10)
          NULL
        }
      )

      if (!is.null(res)) {
        saveRDS(res, "invariance_results_shiny.rds")
        cat("Invariance results saved to invariance_results_shiny.rds\n")
      }

      incProgress(0.7)
      if(is.null(res)) {
        showNotification("Error in invariance analysis", type = "error", duration = 10)
        return(NULL)
      }
      res
    })
  })

  output$invariancePlot <- renderPlot({
    inv <- invariance_res(); req(inv)
    plot(inv, p_type = input$p_type, p_value = input$p_value)
  })

  output$downloadInvariancePlot <- downloadHandler(
    filename = "invariance_plot.jpg",
    content = function(file) {
      inv <- invariance_res(); req(inv)
      ggsave(
        file,
        plot = plot(inv, p_type = input$p_type, p_value = input$p_value),
        width = input$widthInv, height = input$heightInv, dpi = input$dpiInv, bg = "white"
      )
    }
  )

  output$invarianceTable <- renderTable({
    inv <- invariance_res(); req(inv)
    as.data.frame(inv$results) %>%
      tibble::rownames_to_column("Item")
  })

  output$downloadInvarianceTable <- downloadHandler(
    filename = "invariance_results.xlsx",
    content = function(file) {
      df_results <- as.data.frame(invariance_res()$results) %>%
        tibble::rownames_to_column("Item")
      openxlsx::write.xlsx(df_results, file)
    }
  )

  output$downloadInvarianceRCode <- downloadHandler(
    filename = "invariance_analysis_code.R",
    content = function(file) {
      params <- list(
        corr = input$corr,
        model = input$model,
        algorithm = input$algorithm,
        iter = input$iterInv,
        seed = input$seedInv,
        configural_threshold = input$configural_threshold,
        p_type = input$p_type,
        p_value = input$p_value,
        resolution_parameter = input$resolution_parameter,
        objective_function = input$objective_function
      )
      code <- generate_r_code("invariance", params, data_info)
      writeLines(code, file)
    }
  )

  # ----- Hierarchical EGA -----------------------------------------------------
  hier_res <- reactive({
    req(filtered_data())
    tryCatch({
      result <- EGAnet::hierEGA(filtered_data(), scores="network", plot.EGA=TRUE)
      if(!is.null(result) && !is.null(result$plot.hierEGA)) result else NULL
    }, error=function(e){
      showNotification(paste("Error in hierEGA:", e$message), type="error", duration=10)
      NULL
    })
  })

  output$plotHierEGA <- renderPlot({
    hr <- hier_res()
    if (is.null(hr) || is.null(hr$plot.hierEGA)) {
      output$hierEGAError <- renderText("Cannot generate hierarchical model. This may occur when the data structure is not suitable for hierarchical analysis.")
      return(NULL)
    }
    output$hierEGAError <- renderText("")
    tryCatch({
      print(hr$plot.hierEGA + theme_void() + ggtitle("Hierarchical EGA Plot"))
    }, error = function(e) {
      output$hierEGAError <- renderText(paste("Error displaying plot:", e$message))
      NULL
    })
  })

  output$downloadHierEGAPlot <- downloadHandler(
    filename="hierEGA_plot.jpg",
    content=function(file){
      hr <- hier_res()
      if(!is.null(hr) && !is.null(hr$plot.hierEGA)) {
        tryCatch({
          ggsave(
            file,
            plot   = hr$plot.hierEGA + theme_void(),
            width  = input$widthHier,
            height = input$heightHier,
            dpi    = input$dpiHier,
            bg     = "white"
          )
        }, error = function(e) {
          showNotification(paste("Error saving plot:", e$message), type="error")
        })
      }
    }
  )

  output$downloadHierRCode <- downloadHandler(
    filename = "hierega_analysis_code.R",
    content = function(file) {
      code <- generate_r_code("hierega", list(), data_info)
      writeLines(code, file)
    }
  )

  # ----- Wording Effects ------------------------------------------------------
  wording_result <- reactiveVal(NULL)

  observeEvent(input$runWordingEffects, {
    req(filtered_data())
    result <- tryCatch(
      EGAnet::riEGA(filtered_data(),
                    corr=input$corr,
                    model=input$model,
                    algorithm=input$algorithm,
                    plot.EGA=TRUE,
                    seed=as.numeric(input$seed),
                    ncores=detect_cores_for_cloud(),
                    loading.method="revised"),
      error=function(e){
        showNotification(paste("Error in wording effects:", e$message), type="error", duration=10)
        e
      }
    )
    wording_result(result)
  })

  output$wordingPlotUI <- renderUI({
    result <- wording_result()
    if (!is.null(result) && !inherits(result, "error") && !is.null(result$Plot.EGA)) {
      plotOutput("plotWordingEffects", height = "600px")
    }
  })

  output$plotWordingEffects <- renderPlot({
    result <- wording_result()
    if (!is.null(result) && !inherits(result, "error") && !is.null(result$Plot.EGA)) {
      print(result$Plot.EGA + theme_void() + ggtitle("Wording Effects EGA Plot"))
    }
  })

  output$wordingEffectsError <- renderText({
    result <- wording_result()
    if (is.null(result)) {
      NULL
    } else if (inherits(result, "error") || is.null(result$Plot.EGA)) {
      "Unable to generate Wording Effects with these data"
    } else {
      NULL
    }
  })

  output$downloadWordingUI <- renderUI({
    result <- wording_result()
    if (!is.null(result) && !inherits(result, "error") && !is.null(result$Plot.EGA)) {
      downloadBttn("downloadWordingEffectsPlot", "Download Wording Effects Plot",
                   style="jelly", color="success")
    }
  })

  output$downloadWordingEffectsPlot <- downloadHandler(
    filename = "wording_effects_plot.jpg",
    content = function(file) {
      result <- wording_result()
      ggsave(
        file,
        plot = result$Plot.EGA + theme_void(),
        width = input$widthWord,
        height = input$heightWord,
        dpi = input$dpiWord,
        bg = "white"
      )
    }
  )

  output$downloadWordingRCode <- downloadHandler(
    filename = "wording_effects_code.R",
    content = function(file) {
      params <- list(
        corr = input$corr,
        model = input$model,
        algorithm = input$algorithm,
        seed = input$seed
      )
      code <- generate_r_code("wording", params, data_info)
      writeLines(code, file)
    }
  )

  # ----- Almacenamiento de resultados ----------------------------------------
  analysis_results <- reactiveValues(
    ega = NULL, bootega = NULL, invariance = NULL, hierega = NULL,
    wording = NULL, uva = NULL, likert_data = NULL
  )
  observeEvent(ega_result(),        { analysis_results$ega        <- ega_result() })
  observeEvent(bootEGA_res(),       { analysis_results$bootega    <- bootEGA_res() })
  observeEvent(invariance_res(),    { analysis_results$invariance <- invariance_res() })
  observeEvent(hier_res(),          { analysis_results$hierega    <- hier_res() })
  observeEvent(wording_result(),    { analysis_results$wording    <- wording_result() })
  observeEvent(filtered_data(), {
    req(filtered_data())
    df <- filtered_data()
    likert_summary <- list()
    for(col in names(df)) {
      if(is.numeric(df[[col]])) {
        tbl <- table(df[[col]])
        likert_summary[[col]] <- as.list(prop.table(tbl) * 100)
      }
    }
    analysis_results$likert_data <- likert_summary
  })

  # ----- Limpieza para JSON ---------------------------------------------------
  clean_for_json <- function(obj) {
    if(is.null(obj)) return(NULL)
    if(is.numeric(obj) || is.character(obj) || is.logical(obj)) return(obj)
    if(is.list(obj)) return(lapply(obj, clean_for_json))
    if(is.data.frame(obj) || is.matrix(obj)) return(as.list(as.data.frame(obj)))
    if(is.object(obj)) {
      if(inherits(obj, "EGA.community")) return(tryCatch({ as.list(as.numeric(obj)) }, error = function(e) NULL))
      return(tryCatch({ if(length(obj) > 0) as.list(obj) else NULL }, error = function(e) NULL))
    }
    tryCatch({ as.list(obj) }, error = function(e) NULL)
  }

  # ----- JSON para el reporte -------------------------------------------------
  json_output <- reactive({
    if(input$generateJSON == 0 && input$generateReport == 0) return(NULL)
    if(is.null(filtered_data())) {
      showNotification("Please load data first", type = "warning")
      return(NULL)
    }
    if(is.null(analysis_results$ega) &&
       is.null(analysis_results$bootega) &&
       is.null(analysis_results$invariance) &&
       is.null(analysis_results$hierega) &&
       is.null(analysis_results$wording)) {
      showNotification("No analysis results available. Please run at least one analysis first.", type = "warning")
      return(NULL)
    }
    output_data <- list(
      metadata = list(
        date = Sys.Date(),
        software = "ExGraf",
        r_version = R.version.string
      ),
      analysis_params = list(
        correlation = input$corr,
        model = input$model,
        algorithm = input$algorithm,
        iterations = if(!is.null(input$iter)) input$iter else NA,
        seed = if(!is.null(input$seed)) input$seed else if(!is.null(input$seedInv)) input$seedInv else NA
      ),
      likert_summary = list(),
      ega_results = list(),
      reliability_results = list(),
      invariance_results = list(),
      hierarchical_results = list(),
      wording_results = list(),
      uva_results = list()
    )
    if(input$algorithm == "leiden") {
      output_data$analysis_params$resolution_parameter <- input$resolution_parameter
      output_data$analysis_params$objective_function  <- input$objective_function
    }
    if(!is.null(analysis_results$likert_data)) {
      output_data$likert_summary <- analysis_results$likert_data
    }
    if(!is.null(analysis_results$ega)) {
      ega <- analysis_results$ega
      nl <- tryCatch({ EGAnet::net.loads(ega)$std }, error = function(e) NULL)
      loadings_list <- list()
      if(!is.null(nl)) {
        for(i in 1:nrow(nl)) loadings_list[[rownames(nl)[i]]] <- as.list(nl[i,])
      }
      membership <- if(!is.null(ega$wc)) {
        if(is.numeric(ega$wc) || is.integer(ega$wc)) {
          as.list(ega$wc)
        } else {
          tryCatch({ as.list(as.numeric(ega$wc)) }, error = function(e) list())
        }
      } else list()
      output_data$ega_results <- list(
        n_dimensions = ega$n.dim,
        tefi = if(!is.null(ega$TEFI)) ega$TEFI else NA,
        network_loadings = loadings_list,
        membership = membership
      )
    }
    if(!is.null(analysis_results$bootega)) {
      boot_res <- analysis_results$bootega
      structural_consistency <- tryCatch({
        if(!is.null(boot_res$sc) && !is.null(boot_res$sc$dimension.stability) &&
           !is.null(boot_res$sc$dimension.stability$structural.consistency)) {
          as.list(boot_res$sc$dimension.stability$structural.consistency)
        } else list()
      }, error = function(e) list())
      item_stability <- tryCatch({
        if(!is.null(boot_res$boot) && !is.null(boot_res$boot$stability) &&
           !is.null(boot_res$boot$stability$item.stability) &&
           !is.null(boot_res$boot$stability$item.stability$membership)) {
          membership <- boot_res$boot$stability$item.stability$membership
          if(is.numeric(membership) || is.integer(membership)) {
            as.list(membership)
          } else if(is.matrix(membership) || is.data.frame(membership)) {
            apply(membership, 1, function(x) as.list(x))
          } else {
            tryCatch({
              if(!is.null(names(membership))) {
                result <- list()
                for(nm in names(membership)) result[[nm]] <- as.numeric(membership[[nm]])
                result
              } else as.list(as.numeric(membership))
            }, error = function(e2) list())
          }
        } else list()
      }, error = function(e) list())
      output_data$reliability_results <- list(
        structural_consistency = structural_consistency,
        item_stability = item_stability
      )
    }
    if(!is.null(analysis_results$invariance)) {
      inv <- analysis_results$invariance
      if(!is.null(inv$results)) {
        inv_df <- as.data.frame(inv$results)
        inv_list <- list()
        for(i in 1:nrow(inv_df)) inv_list[[rownames(inv_df)[i]]] <- as.list(inv_df[i,])
        output_data$invariance_results <- inv_list
      }
    }
    if(!is.null(analysis_results$hierega)) {
      hier <- analysis_results$hierega
      output_data$hierarchical_results <- list(
        lower_order = hier$lower_order$n.dim,
        higher_order = hier$higher_order$n.dim
      )
    }
    # NUEVO: UVA sobre datos pre-manual + remoción manual
    sug <- character(0)
    if (!is.null(uva_pre_result()) && !is.null(uva_pre_result()$keep_remove$remove))
      sug <- as.character(uva_pre_result()$keep_remove$remove)
    output_data$uva_results <- list(
      suggested_removed = sug,
      manual_removed    = as.character(data_info$removed_items %||% character(0))
    )

    output_data_clean <- clean_for_json(output_data)
    tryCatch({
      jsonlite::toJSON(output_data_clean, pretty = TRUE, auto_unbox = TRUE, null = "null")
    }, error = function(e) {
      showNotification(paste("Error generating JSON:", e$message), type = "error")
      jsonlite::toJSON(list(
        error = "Failed to generate complete JSON",
        message = e$message,
        metadata = output_data$metadata
      ), pretty = TRUE, auto_unbox = TRUE)
    })
  })

  output$jsonPreview <- renderText({
    json <- json_output()
    if(!is.null(json)) json else "No data to display. Please run some analyses first."
  })
  output$jsonGenerated <- reactive({ !is.null(json_output()) })
  outputOptions(output, "jsonGenerated", suspendWhenHidden = FALSE)

  output$downloadJSON <- downloadHandler(
    filename = function() paste0("exgraf_analysis_", Sys.Date(), ".json"),
    content = function(file) { writeLines(json_output(), file) }
  )

  # ----- Generación de reporte con OpenAI ------------------------------------
  observeEvent(input$generateReport, {
    req(input$apiKey)
    if(!grepl("^sk-", input$apiKey) && !grepl("^sk-proj-", input$apiKey)) {
      showNotification("Invalid API key format. Please check your OpenAI API key.", type = "error", duration = 5)
      return()
    }
    if (!ping_openai(input$apiKey)) {
      showNotification("No hay conectividad estable con api.openai.com (o proxy/firewall).", type="error", duration=8)
      return()
    }

    json_data <- json_output()
    if (is.null(json_data)) {
      showNotification("Failed to generate JSON data. Please check your analyses.", type = "error")
      return()
    }

    json_for_api <- shrink_json_for_prompt(json_data, target_chars = 2800)
    current_hash <- digest::digest(json_for_api)

    if(!is.null(report_cache$json_hash) &&
       report_cache$json_hash == current_hash &&
       !is.null(report_cache$cached_analysis) &&
       !is.null(report_cache$cached_results)) {
      report_output$data_analysis <- report_cache$cached_analysis
      report_output$results       <- report_cache$cached_results
      showNotification("Report retrieved from cache!", type = "message", duration = 3)
      return()
    }

    lang_instruction <- if(input$reportLanguage == "es") "Write in Spanish. " else "Write in English. "

    citation_json <- jsonlite::toJSON(citation_map, auto_unbox = TRUE)

    # Data Analysis (forzar párrafo sin viñetas en parámetros)
    analysis_prompt <- paste0(
      lang_instruction,
      "You are an expert in psychometric analysis and academic writing. ",
      "Generate a 'Data Analysis' section for a research paper based on the provided JSON data from an Exploratory Graph Analysis (EGA).\n\n",
      "STRUCTURE:\n",
      "• Software and packages introduction\n",
      "• Methodological description\n",
      "• Technical parameters and criteria (THIS MUST BE A SINGLE COHESIVE PARAGRAPH; no bullet points; use connectors)\n",
      "• Justification for methodological choices\n\n",
      "CITATION RULES (VERY IMPORTANT):\n",
      "1) Use APA 7 in-text citations exactly as (Author, Year).\n",
      "2) Only use the sources listed in the IDEA↔CITATION MAP below.\n",
      "3) Attach the relevant citation to the sentence that expresses the corresponding idea.\n",
      "4) Do NOT invent new sources. Do NOT add a reference list here.\n",
      "5) Keep the report language as selected by the user.\n\n",
      "IDEA↔CITATION MAP (JSON):\n", citation_json, "\n\n",
      "Mention in the text: R/RStudio, EGAnet, psychonetrics, LCT, tidyverse, gridExtra, walktrap, Spearman vs polychoric under skew in EBICglasso, network-loading thresholds (.15/.25/.35), bootEGA with 1000 resamples and LE, 75% criterion, structural vs internal consistency, latent network modeling, and Cohen cutoffs.\n\n",
      "JSON DATA (context for methods choices):\n", json_for_api
    )

    report_output$data_analysis <- tryCatch(
      openai_chat_retry(
        model = DEFAULT_GPT_MODEL,
        prompt = analysis_prompt,
        api_key = input$apiKey,
        max_tokens = 1500,
        temperature = 0.3,
        timeout_sec = 120
      ),
      error = function(e) {
        showNotification(paste("Error (Data Analysis):", e$message), type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(report_output$data_analysis)) return()

    # Results con HECHOS obligatorios (incluye intro cargas + UVA manual/sugerido)
    build_report_facts <- function() {
      ega_facts <- compact_loadings_for_prompt(analysis_results$ega, top_per_dim = 5)
      hier_available <- !is.null(analysis_results$hierega) &&
        !is.null(analysis_results$hierega$lower_order) &&
        !is.null(analysis_results$hierega$higher_order)
      hier_facts <- list(
        available    = isTRUE(hier_available),
        lower_order  = if (hier_available) analysis_results$hierega$lower_order$n.dim else NA,
        higher_order = if (hier_available) analysis_results$hierega$higher_order$n.dim else NA
      )
      wr <- try(wording_result(), silent = TRUE)
      wording_available <- (!inherits(wr, "try-error") && !is.null(wr) && !inherits(wr, "error") && !is.null(wr$Plot.EGA))
      wording_facts <- list(available = isTRUE(wording_available))

      uva_removed_suggested <- character(0)
      if (!is.null(uva_pre_result()) && !is.null(uva_pre_result()$keep_remove$remove)) {
        uva_removed_suggested <- as.character(uva_pre_result()$keep_remove$remove)
      }
      manual_removed <- as.character(data_info$removed_items %||% character(0))

      likert_present <- !is.null(analysis_results$likert_data) && length(analysis_results$likert_data) > 0

      list(
        likert_present = likert_present,
        ega            = ega_facts,
        hierarchical   = hier_facts,
        wording        = wording_facts,
        uva            = list(
          suggested_removed = uva_removed_suggested,
          manual_removed    = manual_removed
        )
      )
    }

    facts_json <- jsonlite::toJSON(build_report_facts(), auto_unbox = TRUE)
    results_prompt <- paste0(
      lang_instruction,
      "You are an expert in psychometric analysis and academic writing. ",
      "Generate a 'Results' section for a research paper based on the provided JSON data from an Exploratory Graph Analysis (EGA).\n\n",
      "INSTRUCTIONS:\n",
      "1) Write in academic style, past tense.\n",
      "2) Include concrete values/percentages when available.\n",
      "3) Order the section as follows (and after each subsection add the exact Spanish placeholder indicated):\n",
      "   - Preliminary Item Analysis (Likert) → after the paragraph add a standalone line: [Inserta la Figura 1]\n",
      "   - Dimensionality and Stability (EGA/BootEGA) → after the paragraph add: [Inserta la Figura 2]\n",
      "   - Network Loadings (top) → describe WITHOUT any table and then add: [Inserta la Tabla 1]\n",
      "   - Structural Consistency (BootEGA)\n",
      "   - Invariance (if applicable)\n",
      "   - Additional analyses (hierarchical, wording, UVA)\n\n",
      "RENDERING RULES:\n",
      "• Do NOT output any markdown tables or HTML tables anywhere. Only text + placeholders.\n",
      "• The placeholder lines must appear exactly as:\n",
      "  [Inserta la Figura 1]\n",
      "  [Inserta la Figura 2]\n",
      "  [Inserta la Tabla 1]\n",
      "• Keep the placeholders in Spanish regardless of the report language selected.\n",
      "• If 'hierarchical.available' is true, report both lower_order and higher_order dimensions; otherwise state that hierEGA could not be produced with these data.\n",
      "• State whether 'wording.available' is true or false.\n",
      "• For UVA, explicitly list manual_removed and suggested_removed (if any).\n\n",
      "MANDATORY FACTS (compact JSON). You MUST reflect these facts faithfully.\n",
      facts_json, "\n\n",
      "JSON DATA (full context; may be pruned):\n", json_for_api
    )

    report_output$results <- tryCatch(
      openai_chat_retry(
        model = DEFAULT_GPT_MODEL,
        prompt = results_prompt,
        api_key = input$apiKey,
        max_tokens = 2000,
        temperature = 0.3,
        timeout_sec = 120
      ),
      error = function(e) {
        showNotification(paste("Error (Results):", e$message), type = "error", duration = 10)
        NULL
      }
    )

    if (!is.null(report_output$data_analysis) && !is.null(report_output$results)) {
      report_cache$json_hash       <- current_hash
      report_cache$cached_analysis <- report_output$data_analysis
      report_cache$cached_results  <- report_output$results
      showNotification("Report generated successfully!", type = "message", duration = 5)
    }
  })

  # Debug info
  output$debugInfo <- renderText({
    paste(
      "Report Generation Status:",
      paste("- Data Analysis exists:", !is.null(report_output$data_analysis)),
      paste("- Results exists:", !is.null(report_output$results)),
      paste("- Report should be visible:", !is.null(report_output$data_analysis) || !is.null(report_output$results)),
      paste("- Model (fixed):", DEFAULT_GPT_MODEL),
      paste("- Current time:", Sys.time()),
      sep = "\n"
    )
  })

  # ----- Mostrar secciones unificadas en la UI --------------------------------
  output$dataAnalysisSection <- renderUI({
    fs <- final_sections()
    req(nzchar(fs$analysis))
    div(
      style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
      HTML(md_to_html(fs$analysis))
    )
  })

  output$resultsSection <- renderUI({
    fs <- final_sections()
    req(nzchar(fs$results))
    div(
      style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
      HTML(md_to_html(fs$results))
    )
  })

  output$reportGenerated <- reactive({
    !is.null(report_output$data_analysis) || !is.null(report_output$results)
  })
  outputOptions(output, "reportGenerated", suspendWhenHidden = FALSE)

  # ----- Descargas de reporte -------------------------------------------------
  output$downloadReport <- downloadHandler(
    filename = function() paste0("exgraf_report_", Sys.Date(), ".txt"),
    content = function(file) {
      fs <- final_sections()
      report_content <- c(
        "=====================================",
        "EXGRAF ANALYSIS REPORT",
        paste("Generated on:", Sys.Date()),
        "=====================================",
        "",
        "DATA ANALYSIS",
        "=============",
        "",
        if(nzchar(fs$analysis)) fs$analysis else "No data analysis section generated.",
        "",
        "",
        "RESULTS",
        "=======",
        "",
        if(nzchar(fs$results)) fs$results else "No results section generated.",
        "",
        "=====================================",
        "End of Report"
      )
      writeLines(report_content, file)
    }
  )

  output$downloadReportWord <- downloadHandler(
    filename = function() paste0("exgraf_report_", Sys.Date(), ".docx"),
    content = function(file) {
      if (!requireNamespace("rmarkdown", quietly = TRUE)) install.packages("rmarkdown")
      fs <- final_sections()
      md_analysis   <- fs$analysis
      results_final <- fs$results
      citas_en_texto <- paste(
        "Citas en texto (APA 7):",
        "(Christensen & Golino, 2021; Christensen, Garrido, & Guerra-Peña, 2024; ",
        "Golino & Epskamp, 2017; Jamison, Christensen, & Golino, 2024; ",
        "Cohen, 1988; Epskamp, 2020).",
        sep = ""
      )
      referencias_apa <- paste(
        "Auguie, B. (2017). gridExtra: Miscellaneous Functions for \"Grid\" Graphics. R package.",
        "",
        "Christensen, A. P., & Golino, H. F. (2021). Estimating the stability of psychological dimensions via Bootstrap Exploratory Graph Analysis: A Monte Carlo simulation and tutorial. *Psych, 3*(3), 479–500. https://doi.org/10.3390/psych3030032",
        "",
        "Christensen, A. P., Golino, H., & ... (2020). (Referencia sobre consistencia estructural).",
        "",
        "Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Routledge.",
        "",
        "Epskamp, S. (2024). *psychonetrics*: Structural equation modeling using network psychometrics. R package.",
        "",
        "Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating psychological networks and their accuracy: A tutorial paper. *Behavior Research Methods, 50*, 195–212.",
        "",
        "Epskamp, S., Rhemtulla, M., & Borsboom, D. (2017). (Latent Network Modeling).",
        "",
        "Garcia-Pardina, A., et al. (2022). (Aplicación/explicación del algoritmo walktrap).",
        "",
        "Golino, H. F., & Christensen, A. P. (2021). EGAnet (paquete R).",
        "",
        "Golino, H. F., et al. (2021). (Criterio del 75% para replicación).",
        "",
        "Isvoranu, A.-M., & Epskamp, S. (2021). Which estimation method to use in network psychometrics under skew? (Spearman vs polychoric in EBICglasso).",
        "",
        "RStudio Team. (2023). RStudio: Integrated Development Environment for R. Posit PBC.",
        "",
        "Wickham, H., et al. (2019). Welcome to the tidyverse. *Journal of Open Source Software, 4*(43), 1686.",
        sep = "\n"
      )

      rmd_text <- paste0(
        "---\n",
        "title: \"EXGRAF ANALYSIS REPORT\"\n",
        "date: \"", Sys.Date(), "\"\n",
        "output:\n  word_document:\n    toc: false\n",
        "---\n\n",
        "# 2. Análisis de datos (métodos)\n\n",
        if (nzchar(md_analysis)) paste0(md_analysis, "\n\n") else "",
        citas_en_texto, "\n\n",
        results_final, "\n\n",
        "# Referencias\n\n",
        referencias_apa, "\n"
      )
      tf <- tempfile(fileext = ".Rmd")
      writeLines(rmd_text, tf, useBytes = TRUE)
      on.exit(unlink(tf), add = TRUE)
      rmarkdown::render(
        tf,
        output_format = rmarkdown::word_document(),
        output_file   = basename(file),
        output_dir    = dirname(file),
        quiet = TRUE,
        encoding = "UTF-8"
      )
    }
  )

  # ----- API example code (debug) --------------------------------------------
  output$apiRCode <- renderText({
    paste0(
      "# R Code for ChatGPT API Integration\n",
      "# Install required package if needed: install.packages('httr')\n\n",
      "library(httr)\n",
      "library(jsonlite)\n\n",
      "api_key <- 'your-api-key-here'\n\n",
      "json_data <- '", if(!is.null(json_output())) substr(json_output(), 1, 100) else "", "...'\n\n",
      "generate_section <- function(prompt, api_key, model = '", DEFAULT_GPT_MODEL, "') {\n",
      "  response <- POST(\n",
      "    url = 'https://api.openai.com/v1/chat/completions',\n",
      "    add_headers(\n",
      "      Authorization = paste('Bearer', api_key),\n",
      "      `Content-Type` = 'application/json'\n",
      "    ),\n",
      "    body = toJSON(list(\n",
      "      model = model,\n",
      "      messages = list(\n",
      "        list(role = 'system', content = 'You are an expert in psychometric analysis.'),\n",
      "        list(role = 'user', content = prompt)\n",
      "      ),\n",
      "      temperature = 0.3,\n",
      "      max_tokens = 1500\n",
      "    ), auto_unbox = TRUE),\n",
      "    encode = 'json'\n",
      "  )\n",
      "  if(status_code(response) == 200) content(response)$choices[[1]]$message$content else stop('API Error')\n",
      "}\n"
    )
  })
}

shinyApp(ui, server)
