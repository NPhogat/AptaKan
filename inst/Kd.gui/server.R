library(shiny)
library(AptaKan)
shinyServer(function(input,output){
  ftype <- reactive({input$filetype})
  crepcalc <- reactive({input$calc})
  kd.method <- reactive({input$method})
  kd.start <- reactive({input$Kdstart})

  file <- reactive({
    if(is.null(input$file)){
      infile <- NULL
    } else
      infile <- ReadAptakan(input$file$datapath, type = ftype())
      x.initdata <- as.data.frame(slot(infile,"initialData"))
      x.initdata
  })

  fileconc <- reactive({
    if(is.null(input$file)){
      infile <- NULL
    } else
      infile <- ReadAptakan(input$file$datapath, type = ftype())
    x.conc <- as.data.frame(slot(infile,"concentration"))
    x.conc
  })

  crep <- eventReactive(input$compute,{
    runif(input$file)
    isolate({
      if(!(is.null(file))){
      x.init <- file()
      x.conc <- fileconc()
      x.data <- new("aptakandt",initialData = x.init, concentration = x.conc)
      x.crep <- CombineReps(x.data,calc = crepcalc())
      x.rep <- as.data.frame(slot(x.crep,"crepData"))
      x.rep
      }
      else{
        return(NULL)
      }
  })
  })

  kd <- reactive({
    if(is.null(crep())){
      return(NULL)
    }
    else{
    x.conc <- fileconc()
    x.crep <- crep()
    data <- new("aptakandt", crepData = x.crep, concentration = x.conc)
    kd.calc <- apta.Kdall(data,plot = "No", method = kd.method(), kdstart = kd.start())
    kd.res <- as.data.frame(slot(kd.calc,"KdData"))
    kd.res
    }
  })

  output$tabset <- renderUI({
    if(is.null(input$file)) {
      tabPanel("No input detected")
    }
    else {
      tabsetPanel(
        tabPanel("Initial Data", tableOutput("file")),
        tabPanel("Concentration", tableOutput("fileconc")),
        tabPanel("Combine replicates", tableOutput("crep")),
        tabPanel("Kd, Correlation", tableOutput("kd"))
      )
    }
  })

  output$file <- renderTable({
    x <- file()
    x
  })

  output$fileconc <- renderTable({
    x <- fileconc()
    x
  })

  output$crep <- renderTable({
    x <- crep()
    x
  })

  output$kd <- renderTable({
    x <- kd()
    x
  })

  output$download <- downloadHandler(
    filename  = "result_report.html",
    content <- function(file) {
      knitr:::knit(input = "result_report.Rmd",
                   output = "result_report.md", quiet = TRUE)
      markdown:::markdownToHTML("result_report.md", "result_report.html")
    }
  )

  })
