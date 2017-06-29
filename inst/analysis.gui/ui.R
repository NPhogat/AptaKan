library(shiny)
shinyUI(fluidPage(
  titlePanel("Analysis of data, confidence intervals and concentration calculation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Choose .csv or .txt tab separated file to upload"),
      tags$hr(),
      checkboxInput("header","Header", TRUE),
      selectInput("filetype", "Select the type of file", choices = c(".csv",".txt")),
      br(),
      selectInput("calc","Select the method to combine replicates", choices = c("Mean","Median", "sd")),
      br(),
      selectInput("met.all","Select the model to analyse", choices = c("lm","loglm","roblm","logroblm","glm")),
      br(),
      selectInput("res.all","Select glm for glm and for other lm", choices = c("lm","glm")),
      br(),
      actionButton("compute","COMPUTE the analysis results!"),
      p("Click on COMPUTE! button to compute the results"),
      br(),
      numericInput("fluo", "Fluorescence value to compute concentration","500"),
      selectInput("Mod.conc","Select the model to compute concentration",choices = c("None","lm","loglm")),
      actionButton("compute2","COMPUTE the concentration!"),
      p("Click on button to compute the concentration"),
      br(),
      downloadButton("download", "Download Results")
    ),
    mainPanel(
      uiOutput("tabset")
    )
  )
))
