# app_simple.R - Versión simplificada para debugging

# ---- Repos seguro + auto-instalación de paquetes ----
options(repos = c(CRAN = "https://cran.r-project.org"))
options(warn = 1)  # Show warnings immediately

pkgs <- c(
  "shiny","shinydashboard","shinyjs","shinyWidgets",
  "readxl","EGAnet","ggplot2","dplyr","tibble","openxlsx","labelled",
  "ggstats","psych","jsonlite","digest"
)

to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) {
  cat("Installing packages:", paste(to_install, collapse = ", "), "\n")
  tryCatch({
    install.packages(to_install, dependencies = TRUE)
    cat("Package installation completed successfully\n")
  }, error = function(e) {
    cat("Package installation error:", conditionMessage(e), "\n")
  })
}

# Load libraries with error handling
load_library <- function(pkg) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("Loaded:", pkg, "\n")
  }, error = function(e) {
    cat("Error loading", pkg, ":", conditionMessage(e), "\n")
    stop("Failed to load critical package: ", pkg)
  })
}

# Load required libraries
load_library("shiny")
load_library("shinydashboard")
load_library("shinyjs")
load_library("shinyWidgets")
load_library("readxl")
load_library("EGAnet")
load_library("ggplot2")
load_library("dplyr")
load_library("tibble")
load_library("openxlsx")
load_library("labelled")
load_library("ggstats")
load_library("psych")
load_library("jsonlite")
load_library("digest")

cat("All libraries loaded successfully\n")

# Simple UI
ui <- dashboardPage(
  dashboardHeader(title = "ExGraf - Test Version"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Test", tabName = "test", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "test",
              fluidRow(
                box(
                  title = "Test Application", status = "primary", solidHeader = TRUE,
                  width = 12,
                  h4("Application loaded successfully!"),
                  p("If you can see this message, the basic application structure is working."),
                  actionButton("test_btn", "Test Button", class = "btn-success")
                )
              )
      )
    )
  )
)

# Simple Server
server <- function(input, output, session) {

  observeEvent(input$test_btn, {
    showNotification("Test successful!", type = "success")
  })

  # Test that critical packages work
  observe({
    tryCatch({
      # Test EGAnet is available
      if (requireNamespace("EGAnet", quietly = TRUE)) {
        cat("EGAnet is available\n")
      }
      # Test ggstats is available
      if (requireNamespace("ggstats", quietly = TRUE)) {
        cat("ggstats is available\n")
      }
    }, error = function(e) {
      cat("Error in package testing:", conditionMessage(e), "\n")
    })
  })
}

# Run the application
cat("Starting Shiny application...\n")
shinyApp(ui, server)
