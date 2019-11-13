library(shiny) #  Shiny web app
library(DT)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
    selectInput("outcome", label = h3("Outcome"),
                choices = list("Fertility" = "Fertility",
                               "Agriculture" = "Agriculture",
                               "Examination" = "Examination",
                               "Education" = "Education",
                               "Catholic" = "Catholic",
                               "Infant.Mortality" = "Infant.Mortality"), selected = 1),
    
    selectInput("indepvar", label = h3("Explanatory variable"),
                choices = list("Fertility" = "Fertility",
                               "Agriculture" = "Agriculture",
                               "Examination" = "Examination",
                               "Education" = "Education",
                               "Catholic" = "Catholic",
                               "Infant.Mortality" = "Infant.Mortality"), selected = 1),

    
    fileInput("file1", "Choose CSV File",
              accept = c(
                ".xls",
                ".xlsx",
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    tags$hr(),
    checkboxInput("header", "Header", TRUE)
  
    
  ),
 
mainPanel(
  
  tabsetPanel(type = "tabs",
             # tabPanel("Contents", div(dataTableOutput("contents"))),
            
              tabPanel("Contents", DT::dataTableOutput('contents')),# Data as datatable
              tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
              tabPanel("Distribution", # Plots of distributions
                       fluidRow(
                         column(6, plotOutput("distribution1")),
                         column(6, plotOutput("distribution2")))
              ),
              tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
              tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
              
  )
)
)
)


# SERVER
server <- function(input, output) {
  
  IC <- JS("function(settings, json) {",
           "$(this.api().table().header()).find('th').css({'padding': '4px'});",
           "$(this.api().table().footer()).find('th').css({'padding': '4px'});",
           "}")
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
  inFile <- input$file1
   if (is.null(inFile))
      return(NULL)
   read(inFile$datapath, header = input$header)
 
  })
  output$contents = DT::renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    mydata <- read.csv(inFile$datapath, header = input$header)
   # DT::datatable(mydata,filter ='top',class = "display nowrap compact", options = list( autoWidth = TRUE, pageLength = 100,lengthChange = TRUE))
    DT::datatable(mydata,class = "display nowrap compact",filter = list( position = 'top', clear = FALSE, plain = FALSE),options = list(autoWidth = TRUE))
    
    })
  
  
# Regression output
  output$summary <- renderPrint({
    fit <- lm(swiss[,input$outcome] ~ swiss[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(swiss, options = list(lengthChange = FALSE))
  })
  
  
  # Scatterplot output
  output$scatterplot <- renderPlot({
    plot(swiss[,input$indepvar], swiss[,input$outcome], main="Scatterplot",
         xlab=input$indepvar, ylab=input$outcome, pch=19)
    abline(lm(swiss[,input$outcome] ~ swiss[,input$indepvar]), col="red")
    lines(lowess(swiss[,input$indepvar],swiss[,input$outcome]), col="blue")
  }, height=400)
  
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(swiss[,input$outcome], main="", xlab=input$outcome)
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    hist(swiss[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
}

shinyApp(ui = ui, server = server)