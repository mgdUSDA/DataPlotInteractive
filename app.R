#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize = 200*1024^2)

library(shiny)
library(tidyr)
library(ggplot2)
library(DT)

#library(Cairo)   # For nicer ggplot2 output when deployed on Linux

appName <- "DataPlotInteractive"
appAbbr <- "DPI"
appVer <- "18.03"
nearThreshold <- 0.1

cat(appName)
cat("  Version: ")
cat(appVer)
cat("\n\n")

currentDir <- getwd()
cat("Active directory is :\n")
cat("   > ")
cat(currentDir)
cat("\n\n")

# Set companion files to reside in Active directory.

dataPalette <- paste(currentDir, "/DataPalette.csv", sep = "")
#unitsPath <- paste(currentDir, "/DataPlotUnits.csv", sep = "")

cat("\n\n")
cat(dataPalette)
cat("\n\n")



#---------------------------------------------------------------------
# Martin duSaire modification of RStudio gallery Plot Interactions
# example.
# Added the side panel controls, data import and subsampling code.
#
#  Expects a long datafile with column headers: time, id, measure, reading, port.
#   Any other columns in the data file will not be used in processing, but will
#   be exported in the saved data file.
#
# Still to do: file and plot save.
#   Assign name to saved data file
#   Include header for row number in exported data file
#   Assign name and save plot .png
#   sub-sampling slider
#   Incorporate basic model fitting using nls() with sliders
#
#
#   How to handle variables and colors not listed in DataPlotColorPalletes.csv and optimize loading of these info files
#   In order to extend to Ohaus MB45 data and others, how to execute R-scripts from Shiny App.
#---------------------------------------------------------------------


ui <- fluidPage(
  sidebarPanel(width = 2,
               # fileInput('file1', 'Choose CSV File',
               #           accept=c('text/csv', 
               #                    'text/comma-separated-values,text/plain', 
               #                    '.csv')),
               # tags$hr(),
               actionButton("loaddata", "Select data file..."),
               # fileInput('file1', 'Choose CSV File',
               #           accept=c('text/csv', 
               #                    'text/comma-separated-values,text/plain', 
               #                    '.csv')),
               HTML("<hr>"),
               selectInput(inputId = 'xcol',
                           label = 'X variable',
                           choices = NULL,
                           selected = NULL
               ),
               selectInput(inputId = 'ycol',
                           label = 'Y variable',
                           choices = NULL,
                           selected = NULL,
                           multiple = TRUE
               ),
               
               tags$hr(),
               
               # sliderInput("sampleRate", "Sample Rate", min = 1, max = 20, value = 1, step = 1), 
               # tags$hr(),
               
               checkboxGroupInput("sampleID", "sample data to plot",
                                  c("no data")),
               #    ,
               #                       c("Item A", "Item B", "Item C")),
               
               selectInput(inputId = 'modelType',
                           label = 'Model type',
                           choices = c("none" = 0, "linear model" = 1, "smooth (loess)" = 2),
                           selected = 0
               ),
               
               tags$hr(),
               
               tags$div(style="display:inline-block", title="CTRL+click to save plot", downloadButton("savePlot", "Save PNG")),
               tags$div(style="display:inline-block", title="CTRL+click to save data", downloadButton("saveData", "Save data")),
               tags$button(
                 style ="display:inline-block",
                 title="Browser tab will remain open",
                 id = 'close',
                 type = "button",
                 class = "btn action-button",
                 onclick = "setTimeout(function(){window.close();},500);",  # close browser
                 "Stop App"
               )    
               
  ),
  fluidRow(
    column(width = 4,
           plotOutput("plot1", width = "auto", height = 600,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot1_click",
                      brush = "plot1_brush"
                      # ,
                      # resetOnNew = TRUE
           )
    ),
    column(width = 5,
           plotOutput("plot2", width = "auto", height = 800
           )
    )
    
  ),
  tags$hr(),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Info", verbatimTextOutput('viewInfo')),
      tabPanel("Data", DT::dataTableOutput('viewRawData')),
      tabPanel("Plot Data", DT::dataTableOutput('viewPlotData')),
      tabPanel("Selected points", DT::dataTableOutput('selectPoints'))
    )
  )
)

server <- function(input, output, session) {
  
  fileInfo <- reactiveValues(name = NULL, path = NULL)
  ranges <- reactiveValues(x = NULL, y = NULL)
  near <- reactiveValues(x = NULL, y = NULL)
  values <- reactiveValues(data = NULL)
  
  pColors <- reactive({
    tempColors <- read.csv(dataPalette, sep = ",", header = TRUE, as.is = TRUE, strip.white = TRUE, blank.lines.skip = TRUE)
    pColors <- tempColors$qualitative
    names(pColors) <- tempColors$measure
    pColors
  })
  
  mUnits <- reactive({
    
    cat("\n\n")
    cat("mUnits :\n")
    cat("   > ")
    cat(dataPalette)
    cat("\n\n")
    
    tempUnits <- read.csv(dataPalette, sep = ",", header = TRUE, as.is = TRUE, strip.white = TRUE, blank.lines.skip = TRUE)
    mUnits <- tempUnits$units
    names(mUnits) <- tempUnits$measure
    mUnits
  })
  

  processData <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    # validate(
    #   need(input$file1 != "", "Select data file to begin.")
    # )
    # 
    # inFile <- input$file1
    

    # if (is.null(inFile)) {
    #   fileInfo$name <- NULL
    #   fileInfo$path <- NULL
    #   return(NULL)
    #   
    # } else {
    #   fileInfo$name <- inFile$name
    #   fileInfo$path <- inFile$datapath
    # }
    
    cat("\n > choose.files()\n")
    
    infile <- choose.files(paste(currentDir, "\\*.txt", sep = ""), caption = "Choose sensor datafile")
    
    cat("\n infile:\n")
    cat("  > ")
    cat(infile)
    cat("\n\n")

    dataFileName <- strsplit(infile, "\\\\")[[1]][length(strsplit(infile, "\\\\")[[1]])]
    
    cat("\n separate filename:\n")
    cat("    > ")
    cat(dataFileName)
    
    # infile <- params$infile
    # dataFileName <- params$dataFileName
    filePath <- strsplit(infile, dataFileName)[[1]][[1]]
    
    cat("\n read data\n")
    
    #    rd <- as.data.frame(read.csv(inFile$datapath, header = FALSE))
    rd <- read.csv(infile, header = TRUE, as.is = TRUE, strip.white = TRUE, blank.lines.skip = TRUE)
    
    
    cat("\n\n head of rd: \n")
    cat("   >>\n")
    print(head(rd))
    cat("\n\n")
    
    # The current version of cozirReader18.02 inserts column headers each time data collection is restarted within the same campaign.
    # This is bad, and adds an extra row of "values" for each data set.  Identify and remove extra header rows:
    
    if (length(which(rd$measure == "measure")) > 0) {
      rd <- rd[-which(rd$measure == "measure"), ]
    }
    
    # Use long version of data file
    
    rd$reading <- as.numeric(rd$reading)
    rd$time <- as.numeric(rd$time)
    cat("\n\n is.na rawData(): ")
    cat(sum(is.na(rd)))
    cat("\n\n")
    rd <- na.omit(rd)
    cat("\n\n Removed?  ")
    cat(sum(is.na(rd)))
    cat("\n\n")
    rd
  }) #End of processData()
  
  rawData <- eventReactive(input$loaddata, {
    
    # Process sensor data.  Select data file and load sample info file.
    
    processData()
  })
  
  plotVariables <- reactive({
    unique(rawData()$measure)
  }) 
  
  observe({
    
    click <- input$plot1_click
    if (is.null(click)) {
      near$x <- NULL
      near$y <- NULL
    } else {
      x_n <- click$x
      y_n <- click$y
      near$x <- c(x_n * (1 - nearThreshold), x_n * (1 + nearThreshold))
      near$y <-  c(y_n * (1 - nearThreshold), y_n * (1 + nearThreshold))
      cat("\n\n near$x: ")
      cat(near$x)
    }
    
    brush <- input$plot1_brush
    if(!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  observe({
    updateSelectInput(session,
                      inputId = "ycol",
                      choices = plotVariables(),
                      selected = plotVariables()[1]
    )
  })
  
  idLen <- reactive({
    length(input$sampleID)
  })
  
  observe({
    ySelected <- names(rawData())[1]
    if ("time" %in% names(rawData())) {
      xSelected <- "time"
    } else {
      xSelected <- ySelected
    }
    #idLen <- length(input$sampleID)
    if (idLen() == 1) {
      updatedXChoices <- unique(c(xSelected, plotVariables()))
    } else {
      updatedXChoices <- xSelected
    }
    updateSelectInput(session,
                      inputId = "xcol",
                      choices = updatedXChoices,
                      selected = xSelected
    )   
  })
  
  observe({
    idList <- as.character(unique(rawData()$id))
    # Can use character(0) to remove all choices
    if (is.null(idList))
      idList <- character()
    # Can also set the label and select items
    updateCheckboxGroupInput(session,
                             inputId = "sampleID",
                             label = "Sample data to plot:",
                             choices = idList,
                             selected = idList
    )
    
    
    if (input$close > 0)
      stopApp()                             # stop shiny
  })
  
  
  # plotData <- reactive({
  #   pd <- NULL
  #   rData <- na.omit(rawData())
  #   pd <- spread(rData[, which(names(rData) != "port")], measure, reading)
  #   
  #   pd <- subset(pd, id %in% input$sampleID)
  #   pd
  #   
  # })
  
  plotData <- reactive({
    
    xChoice <- input$xcol
    rData <- na.omit(rawData())
    pdBase <- subset(rData, measure %in% plotVariables() & id %in% input$sampleID) #Using Long version of rawData
    pdBase <- na.omit(pdBase)
    pd <- pdBase
    if (idLen() == 1 & xChoice != "time") {
      # The port data gets scrambled during the spread() and gather().  Why?
      pdBase$port <- NULL
      
      pdX <- subset(pdBase, select = c(time, measure, reading))
      pdXs <- spread(pdX, measure, reading)
      
      # Remove NA from column xChoice.  The independent variable is the only one that needs to be "complete".
      # Identify the corresponding rows in pdBase based on time. These will need to be removed before merging
      # the final dataframe pd.
      
      if (length(which(is.na(pdXs[, xChoice]))) > 0) {
        removeTimes <- pdXs$time[which(is.na(pdXs[, xChoice]))]
        pdXs <- pdXs[-which(is.na(pdXs[, xChoice])),]
        pdBase <- pdBase[-which(pdBase$time == removeTimes),]
      }
      xTemp <- pdXs[, xChoice]
      pdXs <- cbind(pdXs, xTemp)
      # Sort data by time
      # pdXs <- pdXs[order(pdXs$time), ]
      pdX <- gather(pdXs, key = measure, value = reading, -c(time, xTemp))
      colnames(pdX)[which(colnames(pdX) == "xTemp")] <- xChoice
      pd <- cbind(subset(pdBase, select = -c(time, measure, reading)), pdX)
      
    }
    # Now reduce data frame pd to only variables selected with input$ycol
    
    pd <-  subset(pd, measure %in% input$ycol & id %in% input$sampleID)
    
    pd
  })
  
  pdSelected <- reactive({
    
    in.between <- function(x, r) {
      return((r[1] - x) * (x - r[2]) > 0)
    }
    if (is.null(ranges$x) & is.null(near$x)) {
      pdS <- NULL
    } else {
      if (is.null(ranges$x)) {
        pSelect <- in.between(plotData()[, input$xcol], near$x) & in.between(plotData()[, "reading"], near$y) 
        pdS <- plotData()[pSelect, ]
      } else {
        pSelect <- in.between(plotData()[, input$xcol], ranges$x) & in.between(plotData()[, "reading"], ranges$y) 
        pdS <- plotData()[pSelect, ]
      }
    }
    pdS
  })
  
  output$viewInfo <- renderPrint({
    req(pdSelected) 
    summary(pdSelected())
    
  })
  
  output$viewPlotData <- DT::renderDataTable({
    DT::datatable(plotData(), options = list(lengthMenu = c(10, 20, 50), pageLength = 10))
  })
  
  output$viewRawData <- DT::renderDataTable({
    rdView <- subset(rawData(), id %in% input$sampleID) #Using Long version of rawData
    rdView <- spread(rdView[, which(names(rdView) != "port")], measure, reading, fill = NA, convert = FALSE)
    rdView <- na.omit(rdView)
    DT::datatable(rdView, options = list(lengthMenu = c(10, 20, 50), pageLength = 10))
  })
  
  output$selectPoints <- DT::renderDataTable({
    req(pdSelected())
    DT::datatable(pdSelected(),  options = list(lengthMenu = c(10, 20, 50), pageLength = 10))
  })
  
  output$plot1 <- renderPlot({
    validate(
      need(!is.null(plotData()), "No data available for plotting.")
    )
    wd <- as.character(getwd())
    
    g <- ggplot(plotData(), aes(x = get(input$xcol), y = reading, color = measure, shape = id)) +
      
      #g <- ggplot(data = plotData(), aes(x = time, y = reading, color = measure, shape = id)) +
      geom_point(size = 2.5) +
      scale_shape_manual(values = c(0:18)) +
      scale_colour_manual(values = pColors()) +
      xlab(paste(input$xcol, " (", mUnits()[input$xcol], ")", sep = "")) +
      ggtitle(paste("Data for", fileInfo$name))
    
    switch(input$modelType,
           "0" = gfit <- g,
           "1" = gfit <- g + geom_smooth(method = "lm"),
           "2" = gfit <- g + geom_smooth(method = "loess")
    )
    gfit   
  })
  
  output$plot2 <- renderPlot({
    wd <- as.character(getwd())
    g <- ggplot(data = plotData(), aes(x = get(input$xcol), y = reading, color = measure, shape = id)) +
      geom_point(size = 2.5) +
      scale_shape_manual(values = c(0:18)) +
      scale_colour_manual(values = pColors()) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
      xlab(paste(input$xcol, " (", mUnits()[input$xcol], ")", sep = "")) +
      ggtitle(paste("Data for", fileInfo$name))
    
    switch(input$modelType,
           "0" = gfit <- g,
           "1" = gfit <- g + geom_smooth(method = "lm"),
           "2" = gfit <- g + geom_smooth(method = "loess")
    )
    gfit <- gfit + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
    gfit
  })
  
  # Downloads are saved in browser designated downloads folder, wherever that may be.
  # The app is set to save a copy of the plot and the datafile that generated the plot.
  
  # Output plot using downloadHandler
  
  plotInput <- function(){
    wd <- as.character(getwd())
    g <- ggplot(data = plotData(), aes(x = get(input$xcol), y = reading, color = measure, shape = id)) +
      geom_point(size = 2.5) +
      scale_shape_manual(values = c(0:18)) +
      scale_colour_manual(values = pColors()) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
      xlab(paste(input$xcol, " (", mUnits()[input$xcol], ")", sep = "")) +
      ggtitle(paste("Data for", fileInfo$name))
    switch(input$modelType,
           "0" = gsave <- g,
           "1" = gsave <- g + geom_smooth(method = "lm"),
           "2" = gsave <- g + geom_smooth(method = "loess")
    )    
    gsave
  }
  
  output$savePlot <- downloadHandler(
    filename = function() {
      switch(input$modelType,
             "0" = model <- "none",
             "1" = model <- "lm",
             "2" = model <- "loess"
      )    
      paste(appName, "_", strsplit(fileInfo$name, ".csv")[[1]][1], "_",
            as.character(input$ycol), "v", as.character(input$xcol), "fit_",
            model, "_",
            format(Sys.time(), "%Y-%m-%d %H:%M"), ".png", sep = "") 
    },
    content = function(file) {
      #      png(file)
      #      print(plotInput())
      ggsave(file, plot = plotInput(), device = "png")
      #      dev.off()
    },
    contentType = "image/png"
  )
  
  # Output .csv using downloadHandler.  This works.  
  
  output$saveData <- downloadHandler(
    filename = function() {
      paste(appVer, "_", strsplit(fileInfo$name, ".txt")[[1]][1], "_",
            as.character(input$ycol), "v", as.character(input$xcol), "_",
            format(Sys.time(), "%Y-%m-%d %H:%M"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(plotData(), file)
    }
  )
}

shinyApp(ui, server)
