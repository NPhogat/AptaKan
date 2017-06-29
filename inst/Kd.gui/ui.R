library(shiny)
shinyUI(fluidPage(
  titlePanel("Computation of Dissociation Constant"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Choose .csv or .txt tab separated file to upload"),
      tags$hr(),
      checkboxInput("header","Header", TRUE),
      selectInput("filetype", "Select the type of file", choices = c(".csv",".txt")),
      br(),
      selectInput("calc","Select the method to combine replicates", choices = c("Mean","Median")),
      br(),
      selectInput("method","Select Kd computing method", choices = c("sig","non-sig")),
      br(),
      numericInput("Kdstart", "Initial value to simulate Kd","1"),
      br(),
      actionButton("compute","COMPUTE the analysis results!"),
      p("Click on COMPUTE! button to compute the results"),
      br(),
      downloadButton("download", "Download Results")
    ),
    mainPanel(
      uiOutput("tabset")
  )
  )
))
