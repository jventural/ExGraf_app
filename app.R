

# app.R

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



ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$div("ExGraf Shiny", style = "font-weight:bold;")
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
                menuItem("Introducción", tabName = "intro", icon = icon("info-circle")),
                menuItem("Item Analysis", tabName = "item_analysis", icon = icon("chart-bar")),
                menuItem("EGA Validation",         tabName = "ega",       icon = icon("project-diagram")),
                menuItem("Reliability",            tabName = "bootega",   icon = icon("chart-line")),
                menuItem("Measurement Invariance", tabName = "invariance",icon = icon("balance-scale")),
                menuItem("Hierarchical Model",     tabName = "hier",      icon = icon("layer-group")),
                menuItem("Wording Effects",        tabName = "wording",   icon = icon("font")),
                menuItem("Redundancy Analysis",    tabName = "uva",       icon = icon("redo")),
                menuItem("References", tabName = "references", icon = icon("book-open"))
    )
  ),
  dashboardBody(
    tabItems(
      # 0) Introducción
      tabItem(tabName = "intro",
              fluidRow(
                box(
                  title = "¡Welcome to ExGraf Shiny!", status = "primary", solidHeader = TRUE, width = 12,
                  HTML('

          <p><strong>ExGraf</strong> is a Shiny application designed to perform <strong>Exploratory Graph Analysis (EGA)</strong> to       identify the dimensional structure of psychometric data using network-based methods.</p>

      <h4>What can you do with ExGraf Shiny?</h4>
    <ul>
      <li><strong>Data Import:</strong> Upload files in <code>.csv</code> or <code>.xlsx</code> format.</li>
      <li><strong>Item Analysis:</strong> Explore descriptive statistics and visualize response distributions using Likert plots.</li>
      <li><strong>EGA Validation:</strong> Analyze and visualize dimensions using networks.</li>
      <li><strong>Reliability:</strong> Assess internal consistency of the identified dimensions.</li>
      <li><strong>Measurement Invariance:</strong> Compare structures across groups.</li>
      <li><strong>Hierarchical Model:</strong> Explore relationships between first- and second-order dimensions.</li>
      <li><strong>Wording Effects:</strong> Detect reverse-worded items that may distort dimensionality.</li>
      <li><strong>Redundancy Analysis:</strong> Identify redundant items within your scale.</li>
    </ul>

    <p style="color:#a94442; font-weight:bold;">
    ⚠️ To properly run the Measurement Invariance, your dataset must include at least one <strong>categorical variable</strong> (e.g., <em>sex</em>).
    </p>

    <p>Don&apos;t have a dataset to test? Download a sample dataset here:</p>
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
                box(title = "Descriptive Statistics", width = 12, status = "info", solidHeader = TRUE,
                    withSpinner(DT::dataTableOutput("tabla_descriptivos"), type = 4)
                )
              ),
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
                downloadBttn("download_descriptivos", "Download Descriptive Table", style = "jelly", color = "primary"),
                downloadBttn("download_likertplot",   "Download Likert Plot",       style = "jelly", color = "success")
              )
      ),

      # 1) EGA Validation
      tabItem(tabName = "ega",
              fluidRow(
                column(width = 4,
                       box(
                         title = "EGA Settings", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, width = NULL,
                         pickerInput("corr",      "Correlation type",
                                     choices = c("auto"="cor_auto","Spearman"="spearman","Pearson"="pearson")),
                         pickerInput("model",     "Model", choices = c("glasso","TMFG")),
                         pickerInput("algorithm", "Algorithm",
                                     choices = c("louvain","walktrap","leiden","fast_greedy")),
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
                downloadBttn("downloadInformativeTable", "Download Informative Table",  style="jelly", color="success")
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
                         numericInput("seed","Seed",value=2024,min=1),
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
                downloadBttn("downloadStructuralConsistency","Download Structural Consistency", style="jelly", color="success")
              )
      ),

      # 3) Measurement Invariance
      tabItem(tabName = "invariance",
              fluidRow(
                column(width = 4,
                       box(
                         title="Invariance Settings", status="danger", solidHeader=TRUE,
                         collapsible=TRUE, width=NULL,
                         pickerInput("p_type","p adjustment",choices=c("BH"="p_BH","none"="p")),
                         numericInput("p_value","p-value",value=0.05,step=0.01),
                         numericInput("configural_threshold","Configural threshold",value=0.70,step=0.01),
                         actionBttn("runInvariance","Run Invariance",style="stretch", color="danger", icon=icon("play")),
                         hr(),
                         h5("Download Settings"),
                         numericInput("widthInv","Width (inches):",8, min=1),
                         numericInput("heightInv","Height (inches):",6, min=1),
                         numericInput("dpiInv","Resolution (dpi):",300, min=50)
                       )
                ),
                column(width = 8,
                       withSpinner(plotOutput("invariancePlot", height = "600px"), type = 6),
                       br(),
                       withSpinner(tableOutput("invarianceTable"), type = 3, color.background = "#FFFFFF")
                )
              ),
              fluidRow(
                downloadBttn("downloadInvariancePlot", "Download Invariance Plot",  style="jelly", color="danger"),
                downloadBttn("downloadInvarianceTable","Download Invariance Table", style="jelly", color="success")
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
                downloadBttn("downloadHierEGAPlot","Download Hierarchical Plot", style="jelly", color="primary")
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
                       plotOutput("plotWordingEffects", height = "600px"),
                       textOutput("wordingEffectsError")
                )
              ),
              fluidRow(
                downloadBttn("downloadWordingEffectsPlot","Download Wording Effects Plot", style="jelly", color="success")
              )
      ),

      # 6) Redundancy Analysis (UVA)
      tabItem(tabName = "uva",
              fluidRow(
                column(width = 12,
                       box(
                         title="UVA Summary", status="purple", solidHeader=TRUE, collapsible=FALSE,
                         withSpinner(textOutput("uvaSummary"), type = 4)
                       )
                )
              )
      ),

      # 7) Referencias
      tabItem(tabName = "references",
              fluidRow(
                box(
                  title = "How to Cite ExGraf", width = 12, status = "primary", solidHeader = TRUE,
                  HTML('
              <p>If you use <strong>ExGraf</strong> in your research or teaching, please cite it as follows:</p>
              <p>Ventura-León, J., Lino-Cruz, C., Tocto-Muñoz, S., & Sánchez-Villena, A. R. (in development). ExGraf: A Shiny interface for accessible Exploratory Graph Analysis in psychological and behavioral research. GitHub Repository. <a href="https://github.com/jventural/ExGraf_app" target="_blank">https://github.com/jventural/ExGraf_app</a></p>
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

  # Data descargada
  output$descargar_ejemplo <- downloadHandler(
    filename = function() {
      "Data_Rosenberg.xlsx"
    },
    content = function(file) {
      datos <- data.frame(
        sexo = rep(c("M", "F"), each = 5),
        item1 = rnorm(10),
        item2 = rnorm(10),
        item3 = rnorm(10),
        item4 = rnorm(10),
        item5 = rnorm(10)
      )
      write.csv(datos, file, row.names = FALSE)
    }
  )

  # Load data, auto-detect CSV delimiter
  data_bfi <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (tolower(ext) == "csv") {
      hdr <- readLines(input$file$datapath, n = 1)
      sep <- if (grepl(";", hdr)) ";" else ","
      read.csv(input$file$datapath, sep = sep, header = TRUE,
               stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      read_excel(input$file$datapath)
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

  # Filtered data
  filtered_data <- reactive({
    df <- data_bfi()
    if (!is.null(input$groupColumn))
      df <- df %>% select(-all_of(input$groupColumn))
    if (!is.null(input$removeItemsManual))
      df <- df %>% select(-all_of(input$removeItemsManual))
    df
  })

  # Item-Analysis
  output$tabla_descriptivos <- DT::renderDataTable({
    req(filtered_data())
    descr <- psych::describe(filtered_data())
    descr_rounded <- round(descr, 2)
    DT::datatable(descr_rounded, options = list(pageLength = 10))
  })

  output$download_descriptivos <- downloadHandler(
    filename = "descriptive_stats.xlsx",
    content = function(file) {
      descr <- psych::describe(filtered_data())
      descr <- round(descr, 2)
      openxlsx::write.xlsx(descr, file)
    }
  )

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
        labs(title = "",
             x = "Percentage", y = "Items") +
        theme(legend.position = "bottom")

      ggsave(file, plot = g, width = input$widthLikert, height = input$heightLikert, dpi = input$dpiLikert)
    }
  )

  # EGA Validation
  ega_result <- eventReactive(input$runEGA, {
    req(filtered_data())
    ncores <- min(detectCores(FALSE),2)
    args <- list(
      data      = filtered_data(),
      corr      = input$corr,
      model     = input$model,
      algorithm = input$algorithm,
      plot.EGA  = TRUE,
      seed      = input$seed,
      ncores    = ncores
    )
    if (input$algorithm=="leiden") {
      args$resolution_parameter <- input$resolution_parameter
      args$objective_function   <- input$objective_function
    }
    tryCatch(do.call(EGA,args), error=function(e){ showNotification(e$message,type="error"); NULL })
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
      ggsave(
        file,
        plot   = ega_result()$plot.EGA + theme_void(),
        width  = input$widthEGA,
        height = input$heightEGA,
        dpi    = input$dpiEGA,
        bg     = "white"
      )
    }
  )

  output$networkLoads <- renderTable({
    res <- ega_result(); req(res)
    net.loads(res)$std %>% as.data.frame() %>% rownames_to_column("Item")
  })

  convert_EGA_to_df <- function(res) {
    m    <- res$network
    meth <- attr(m,"methods")
    metrics <- list(
      Model                = toupper(meth$model),
      Correlations         = meth$corr,
      Lambda               = if(!is.null(meth$lambda)) formatC(meth$lambda,format="f",digits=3) else NA,
      `Number of nodes`    = nrow(m),
      `Number of edges`    = sum(m!=0)/2,
      `Edge density`       = formatC(mean(m!=0),format="f",digits=3),
      M                    = formatC(mean(m[m!=0]),format="f",digits=3),
      SD                   = formatC(sd(m[m!=0]),format="f",digits=3),
      Min                  = formatC(min(m[m!=0]),format="f",digits=3),
      Max                  = formatC(max(m[m!=0]),format="f",digits=3),
      `Number of communities` = res$n.dim,
      TEFI                 = formatC(res$TEFI,format="f",digits=3)
    )
    tibble(Index=names(metrics),Value=unlist(metrics))
  }

  output$informativeTable <- renderTable({
    res <- ega_result(); req(res)
    convert_EGA_to_df(res)
  })

  output$downloadNetworkLoads <- downloadHandler(
    filename="network_loads.xlsx",
    content=function(file){
      write.xlsx(
        net.loads(ega_result())$std %>% as.data.frame() %>% rownames_to_column("Item"),
        file
      )
    }
  )

  output$downloadInformativeTable <- downloadHandler(
    filename="informative_table.xlsx",
    content=function(file){
      write.xlsx(convert_EGA_to_df(ega_result()), file)
    }
  )

  # Reliability
  bootEGA_res <- eventReactive(input$runBootEGA, {
    req(filtered_data())
    withProgress(message="Running reliability analysis...", value=0, {
      incProgress(0.1)
      ncores <- min(detectCores(FALSE),2)
      args <- list(
        data      = filtered_data(),
        iter      = as.numeric(input$iter),
        model     = input$model,
        corr      = input$corr,
        algorithm = input$algorithm,
        seed      = as.numeric(input$seed),
        type      = input$type,
        ncores    = ncores
      )
      if (input$algorithm=="leiden") {
        args$objective_function   <- input$objective_function
        args$resolution_parameter <- input$resolution_parameter
      }
      incProgress(0.2)
      res <- tryCatch(do.call(bootEGA,args),error=function(e)NULL)
      incProgress(0.6)
      validate(need(!is.null(res),"Error in reliability"))
      sc <- EGAnet::dimensionStability(res)
      incProgress(0.1)
      list(boot=res, sc=sc)
    })
  })

  output$plotBootEGA <- renderPlot({
    br <- bootEGA_res(); req(br)
    print(br$boot$stability$item.stability$plot + theme_minimal() + ggtitle("Item Stability"))
  })

  output$downloadBootEGAPlot <- downloadHandler(
    filename="item_stability_plot.jpg",
    content=function(file){
      ggsave(
        file,
        plot   = bootEGA_res()$boot$stability$item.stability$plot + theme_minimal(),
        width  = input$widthBoot,
        height = input$heightBoot,
        dpi    = input$dpiBoot,
        bg     = "white"
      )
    }
  )

  output$structuralConsistency <- renderTable({
    sc <- bootEGA_res()$sc; req(sc)
    data.frame(
      Dimension   = names(sc$dimension.stability$structural.consistency),
      Consistency = sc$dimension.stability$structural.consistency
    )
  })

  output$downloadStructuralConsistency <- downloadHandler(
    filename="structural_consistency.xlsx",
    content=function(file){
      df_sc <- data.frame(
        Dimension   = names(bootEGA_res()$sc$dimension.stability$structural.consistency),
        Consistency = bootEGA_res()$sc$dimension.stability$structural.consistency
      )
      write.xlsx(df_sc, file)
    }
  )

  # Measurement Invariance
  invariance_res <- eventReactive(input$runInvariance, {
    req(data_bfi(), input$groupColumn)
    withProgress(message="Running invariance analysis...", value=0, {
      incProgress(0.1)
      df  <- data_bfi()[ , !names(data_bfi()) %in% input$groupColumn]
      if (!is.null(input$removeItemsManual))
        df <- df[ , !names(df) %in% input$removeItemsManual]
      grp <- data_bfi()[[input$groupColumn]]
      incProgress(0.2)
      res <- tryCatch(
        invariance(
          data                  = df,
          group                 = grp,
          corr                  = input$corr,
          model                 = input$model,
          algorithm             = input$algorithm,
          iter                  = as.numeric(input$iter),
          p_type                = input$p_type,
          p_value               = input$p_value,
          seed                  = as.numeric(input$seed),
          configural.threshold  = input$configural_threshold,
          ncores                = min(detectCores(FALSE),2)
        ),
        error=function(e)NULL
      )
      incProgress(0.7)
      validate(need(!is.null(res),"Error in invariance"))
      res
    })
  })

  output$invariancePlot <- renderPlot({
    inv <- invariance_res(); req(inv)
    plot(inv, p_type=input$p_type, p_value=input$p_value)
  })

  output$downloadInvariancePlot <- downloadHandler(
    filename="invariance_plot.jpg",
    content=function(file){
      ggsave(
        file,
        plot   = plot(invariance_res(), p_type=input$p_type, p_value=input$p_value),
        width  = input$widthInv,
        height = input$heightInv,
        dpi    = input$dpiInv,
        bg     = "white"
      )
    }
  )

  output$invarianceTable <- renderTable({
    inv <- invariance_res(); req(inv)
    as.data.frame(inv$results) %>% rownames_to_column("Item")
  })

  output$downloadInvarianceTable <- downloadHandler(
    filename="invariance_results.xlsx",
    content=function(file){
      write.xlsx(invariance_res()$results, file)
    }
  )

  # Hierarchical EGA
  hier_res <- reactive({
    req(filtered_data())
    tryCatch(EGAnet::hierEGA(filtered_data(), scores="network", plot.EGA=TRUE),
             error=function(e)NULL)
  })

  output$plotHierEGA <- renderPlot({
    hr <- hier_res()
    if (is.null(hr)) {
      output$hierEGAError <- renderText("Cannot generate hierarchical model.")
      return()
    }
    print(
      hr$plot.hierEGA +
        theme_void() +
        ggtitle("Hierarchical EGA Plot")
    )
  })

  output$downloadHierEGAPlot <- downloadHandler(
    filename="hierEGA_plot.jpg",
    content=function(file){
      ggsave(
        file,
        plot   = hier_res()$plot.hierEGA + theme_void(),
        width  = input$widthHier,
        height = input$heightHier,
        dpi    = input$dpiHier,
        bg     = "white"
      )
    }
  )

  # Wording Effects
  observeEvent(input$runWordingEffects, {
    req(filtered_data())
    result <- tryCatch(
      riEGA(filtered_data(), corr=input$corr, model=input$model,
            algorithm=input$algorithm, plot.EGA=TRUE,
            seed=as.numeric(input$seed), ncores=min(detectCores(FALSE),2)),
      error=function(e)e
    )
    if (inherits(result,"error") || is.null(result$Plot.EGA)) {
      output$wordingEffectsError <- renderText("Unable to generate Wording Effects with these data")
      output$plotWordingEffects  <- renderPlot(NULL)
      output$downloadWordingEffectsPlot <- renderUI(NULL)
    } else {
      output$wordingEffectsError <- renderText(NULL)
      output$plotWordingEffects <- renderPlot({
        print(
          result$Plot.EGA +
            theme_void() +
            ggtitle("Wording Effects EGA Plot")
        )
      }, height = 600)
      output$downloadWordingEffectsPlot <- downloadHandler(
        filename="wording_effects_plot.jpg",
        content=function(file){
          ggsave(
            file,
            plot   = result$Plot.EGA + theme_void(),
            width  = input$widthWord,
            height = input$heightWord,
            dpi    = input$dpiWord,
            bg     = "white"
          )
        }
      )
    }
  })

  # UVA
  output$uvaSummary <- renderText({
    req(filtered_data())
    res <- UVA(filtered_data())
    if (is.null(res$keep_remove$remove)) {
      "No redundant items detected."
    } else {
      paste("Remove item(s):", paste(res$keep_remove$remove, collapse = ", "))
    }
  })

}

shinyApp(ui, server)



