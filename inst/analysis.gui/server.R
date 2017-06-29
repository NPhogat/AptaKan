library(shiny)
library(AptaKan)
shinyServer(function(input,output){
  ftype <- reactive({
    input$filetype
  })

  calc.cr <- reactive({
    input$calc
  })

  apta.model <- reactive({
    input$met.all
  })

  apta.res <- reactive({
    input$res.all
  })

  fluorescence <- reactive({
    input$fluo
  })

  aptac <- reactive({
    input$Mod.conc
  })

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

  creps <- eventReactive(input$compute,{
    runif(input$file)
    isolate({
      if(!(is.null(file))){
      x.data <- as.data.frame(file())
      x.conc <- as.data.frame(fileconc())
      data.new <- new("aptakandt", initialData = x.data, concentration = x.conc)
      c.rep <- CombineReps(data.new, calc = calc.cr())
      rep.data <- as.data.frame(slot(c.rep,"crepData"))
      rep.data
      }
      else{
        return(NULL)
      }
    })
  })

  conf.int <- reactive({
    x.data <- as.data.frame(file())
    x.conc <- as.data.frame(fileconc())
    x.reps <- as.data.frame(creps())
    data.conf <- new("aptakandt",initialData = x.data, concentration = x.conc, crepData = x.reps)
    res.conf <- apta.conf(data.conf)
    res <- as.data.frame(slot(res.conf,"confData"))
    res
  })

  apta.all <- reactive({
    x.data <- as.data.frame(file())
    x.conc <- as.data.frame(fileconc())
    x.reps <- as.data.frame(creps())
    data <- new("aptakandt", initialData = x.data, concentration = x.conc, crepData = x.reps)
    aptam <- aptamodelall(data, method = apta.model(), result = apta.res())
    data.res <- as.data.frame(slot(aptam,"modelData"))
    data.res
  })

  calc.conc <- eventReactive(input$compute2,{
    if(is.null((creps()))){
    return(NULL)
    }
    else{
    x.model <- as.data.frame(apta.all())
    data <- new("aptakandt",modelData = x.model)
    data.conc <- aptaconc(data,fluorescence(),aptac())
    #data.conc1 <- as.data.frame(data.conc)
    data.conc
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
        tabPanel("Combine replicates", tableOutput("creps")),
        tabPanel("Confidence Interval", tableOutput("conf.int")),
        tabPanel("Model implementation",tableOutput("apta.all")),
        tabPanel("Calculated concentration",tableOutput("calc.conc"))
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

  output$creps <- renderTable({
    x <- creps()
    x
  })

  output$conf.int <- renderTable({
    x <- conf.int()
    x
  })

  output$apta.all <- renderTable({
    x <- apta.all()
    x
  })

  output$calc.conc <- renderTable({
    x <- calc.conc()
    x
  })

  text1 <- reactive({
    "Select option other than 'None' to implement the method!"
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
