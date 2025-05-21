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

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$div("ExGraf Shiny", style = "font-weight:bold;")
  ),
  dashboardSidebar(
    shinyDashboardThemes(theme = "onenote"),
    useShinyjs(),
    fileInput("file", "Upload Excel file with test items", accept = ".xlsx"),
    uiOutput("groupColumnUI"),
    pickerInput(
      "removeItemsManual", "Select items to remove",
      choices = NULL, multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
    ),
    sidebarMenu(id = "tabs",
                menuItem("EGA Validation",         tabName = "ega",       icon = icon("project-diagram")),
                menuItem("Reliability",            tabName = "bootega",   icon = icon("chart-line")),
                menuItem("Measurement Invariance", tabName = "invariance",icon = icon("balance-scale")),
                menuItem("Hierarchical Model",     tabName = "hier",      icon = icon("layer-group")),
                menuItem("Wording Effects",        tabName = "wording",   icon = icon("font")),
                menuItem("Redundancy Analysis",    tabName = "uva",       icon = icon("redo"))
    )
  ),
  dashboardBody(
    tabItems(

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
                         actionBttn("runEGA","Run EGA", style="stretch", color="success", icon=icon("play"))
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
                         actionBttn("runBootEGA","Run Reliability",style="stretch", color="warning", icon=icon("play"))
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
                         actionBttn("runInvariance","Run Invariance",style="stretch", color="danger", icon=icon("play"))
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
                         helpText("Calculates hierEGA automatically.")
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
                         actionBttn("runWordingEffects","Run Wording Effects",style="stretch", color="success", icon=icon("play"))
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
      )
    )
  )
)

server <- function(input, output, session) {

  # Load data & dynamic UI
  data_bfi <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  output$groupColumnUI <- renderUI({
    req(data_bfi())
    selectInput("groupColumn","Select group column",choices=names(data_bfi()))
  })
  observeEvent(data_bfi(), {
    updatePickerInput(session,"removeItemsManual",
                      choices  = setdiff(names(data_bfi()), input$groupColumn),
                      selected = character(0))
  })

  # Filtered data
  filtered_data <- reactive({
    req(data_bfi())
    df <- data_bfi()
    if (!is.null(input$groupColumn))      df <- df %>% select(-all_of(input$groupColumn))
    if (!is.null(input$removeItemsManual))df <- df %>% select(-all_of(input$removeItemsManual))
    df
  })

  # 1) EGA
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
    tryCatch(do.call(EGA,args),
             error=function(e){ showNotification(e$message,type="error"); NULL })
  })
  output$plotEGA <- renderPlot({
    res <- ega_result(); req(res)
    print(res$plot.EGA + theme_minimal() +
            ggtitle("EGA Plot") +
            annotate("text", x=Inf, y=-Inf,
                     label=paste0("TEFI: ",round(res$TEFI,3)),
                     hjust=1,vjust=-1))
  })
  output$networkLoads <- renderTable({
    res <- ega_result(); req(res)
    net.loads(res)$std %>% as.data.frame() %>% rownames_to_column("Item")
  })
  convert_EGA_to_df <- function(res) {
    m    <- res$network; meth <- attr(m,"methods")
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
    tibble(Index=names(metrics), Value=unlist(metrics))
  }
  output$informativeTable <- renderTable({
    res <- ega_result(); req(res)
    convert_EGA_to_df(res)
  })
  output$downloadEGAPlot <- downloadHandler(
    filename="EGA_plot.jpg",
    content=function(file){
      ggsave(file,
             plot=ega_result()$plot.EGA + theme_minimal(),
             width=8,height=6,dpi=300)
    }
  )
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

  # 2) Reliability with progress
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
      res <- tryCatch(do.call(bootEGA,args), error=function(e) NULL)
      incProgress(0.6)
      validate(need(!is.null(res), "Error in bootEGA"))
      sc <- EGAnet::dimensionStability(res)
      incProgress(0.1)
      list(boot=res, sc=sc)
    })
  })
  output$plotBootEGA <- renderPlot({
    br <- bootEGA_res(); req(br)
    print(br$boot$stability$item.stability$plot + theme_minimal() + ggtitle("Item Stability"))
  })
  output$structuralConsistency <- renderTable({
    sc <- bootEGA_res()$sc; req(sc)
    data.frame(
      Dimension   = names(sc$dimension.stability$structural.consistency),
      Consistency = sc$dimension.stability$structural.consistency
    )
  })
  output$downloadBootEGAPlot <- downloadHandler(
    filename="item_stability_plot.jpg",
    content=function(file){
      ggsave(file,
             plot=bootEGA_res()$boot$stability$item.stability$plot,
             width=8,height=6,dpi=300)
    }
  )
  output$downloadStructuralConsistency <- downloadHandler(
    filename="structural_consistency.xlsx",
    content=function(file){
      df_sc <- data.frame(
        Dimension   = names(bootEGA_res()$sc$dimension.stability$structural.consistency),
        Consistency = bootEGA_res()$sc$dimension.stability$structural.consistency
      )
      write.xlsx(df_sc,file)
    }
  )

  # 3) Invariance with progress
  invariance_res <- eventReactive(input$runInvariance, {
    req(data_bfi(), input$groupColumn)
    withProgress(message="Running invariance analysis...", value=0, {
      incProgress(0.1)
      df  <- data_bfi()[ , !names(data_bfi())%in%input$groupColumn]
      if (!is.null(input$removeItemsManual)) df <- df[ , !names(df)%in%input$removeItemsManual]
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
        error=function(e) NULL
      )
      incProgress(0.7)
      validate(need(!is.null(res), "Error in invariance"))
      res
    })
  })
  output$invariancePlot <- renderPlot({
    inv <- invariance_res(); req(inv)
    plot(inv, p_type=input$p_type, p_value=input$p_value)
  })
  output$invarianceTable <- renderTable({
    inv <- invariance_res(); req(inv)
    as.data.frame(inv$results) %>% rownames_to_column("Item")
  })
  output$downloadInvariancePlot <- downloadHandler(
    filename="invariance_plot.jpg",
    content=function(file){
      ggsave(file,
             plot=plot(invariance_res(), p_type=input$p_type, p_value=input$p_value),
             width=8,height=6,dpi=300)
    }
  )
  output$downloadInvarianceTable <- downloadHandler(
    filename="invariance_results.xlsx",
    content=function(file){
      write.xlsx(invariance_res()$results,file)
    }
  )

  # 4) Hierarchical EGA
  hier_res <- reactive({
    req(filtered_data())
    tryCatch(EGAnet::hierEGA(filtered_data(),scores="network",plot.EGA=TRUE),
             error=function(e)NULL)
  })
  output$plotHierEGA <- renderPlot({
    hr <- hier_res()
    if (is.null(hr)) {
      output$hierEGAError <- renderText("Cannot generate hierarchical model.")
      return()
    }
    print(hr$plot.hierEGA + theme_minimal() + ggtitle("Hierarchical EGA Plot"))
  })
  output$downloadHierEGAPlot <- downloadHandler(
    filename="hierEGA_plot.jpg",
    content=function(file){
      ggsave(file, plot=hier_res()$plot.hierEGA, width=8, height=6, dpi=300)
    }
  )

  # 5) Wording Effects
  observeEvent(input$runWordingEffects, {
    req(filtered_data())
    result <- tryCatch(
      riEGA(filtered_data(), corr=input$corr, model=input$model,
            algorithm=input$algorithm, plot.EGA=TRUE,
            seed=as.numeric(input$seed), ncores=min(detectCores(FALSE),2)),
      error=function(e)e
    )
    if (inherits(result,"error")||is.null(result$Plot.EGA)) {
      output$wordingEffectsError <- renderText("Unable to generate Wording Effects with these data")
      output$plotWordingEffects  <- renderPlot(NULL)
      output$downloadWordingEffectsPlot <- renderUI(NULL)
    } else {
      output$wordingEffectsError <- renderText(NULL)
      output$plotWordingEffects <- renderPlot({
        print(result$Plot.EGA + theme_minimal() + ggtitle("Wording Effects EGA Plot"))
      }, height=600)
      output$downloadWordingEffectsPlot <- downloadHandler(
        filename="wording_effects_plot.jpg",
        content=function(file){
          ggsave(file, plot=result$Plot.EGA + theme_minimal(), width=8, height=6, dpi=300)
        }
      )
    }
  })

  # 6) UVA
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
