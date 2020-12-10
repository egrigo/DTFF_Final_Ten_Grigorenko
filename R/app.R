rm(list=ls())

library(shiny)
library(hash)
library(rstudioapi)
library(sqldf)
library(zoo)
library(ggplot2)
library(stargazer)


tickers <- c("SNP","PTR",'RDS',"BP","XOM","TOT","CVX","MPC","LUKOY")
comp_names <- c("China Petroleum & Chemical","PetroChina","Royal Dutch Shell PLC", 
                "BP PLC", "Exxon Mobil Corp.","Total SE", "Chevron Corp.", "Marathon Petroleum Corp.", "PJSC Lukoil")
comp_dictionary <- hash(tickers, comp_names)

ui <- fluidPage(
  titlePanel ("Plot stock price of:"),  
  sidebarLayout(
    sidebarPanel(  
      selectInput(
        inputId = "company", label = "Choose company:", 
        choices = c("China Petroleum & Chemical" = "SNP", "PetroChina" = "PTR", 
                    "Royal Dutch Shell PLC" = 'RDS', "BP PLC" = "BP", "Exxon Mobil Corp." = "XOM", "Total SE" = "TOT", 
                    "Chevron Corp." = "CVX", "Marathon Petroleum Corp." = "MPC", "PJSC Lukoil" = "LUKOY"), 
        selected = "SNP"
      ),
      
      checkboxInput(
        inputId = "gspc", label = "S&P 500"
      ),
      
      dateRangeInput(
        inputId = "dates", label="Choose interval:", start = "2010-01-01", end = '2020-11-30'
      ),
    ),
    
    mainPanel(
      plotOutput("pricePlot"),
      tableOutput("summaryTable")
    )  
  )
)



server <- function(input, output) {
  # define reactive variable for the table
  summaryStockData <- reactiveVal()
  
  # create plot
  output$pricePlot <- renderPlot({
    # print plot title  
    title <- comp_dictionary[[input$company]]
    title <- paste("Stock Dynamics of", title)
    
    # change directory
 #   current_path = rstudioapi::getActiveDocumentContext()$path 
 #   setwd(dirname(current_path ))
    setwd("data")
    
    # get dates
    date_start <- as.numeric(as.Date(input$dates[1]))
    date_end <- as.numeric(as.Date(input$dates[2]))    
    
    # get filename
    fileid <- paste(input$company, ".csv", sep="")
    stockdata <- data.frame(read.csv(fileid))
    stockdata$Date <- as.numeric(as.Date(stockdata$Date))
    
    # sql filtering query
    stockdata_filtered <- sqldf(paste('SELECT DATE, CLOSE FROM stockdata WHERE DATE BETWEEN', date_start, 'AND', date_end))  
    
    # correct date field
    stockdata_filtered$Date <- as.Date(as.numeric(stockdata_filtered$Date))
    
    
    # print plot 
    g <- ggplot(stockdata_filtered,aes(x=Date)) + 
      geom_line(aes(y=Close)) + 
      scale_y_continuous(name="Stock Price") + 
      theme_light()
    print(g)
    
    # set reactive variable with summary data  
    table_headers <- c('Min', 'Max', "Mean", "Median")
    table_values <- c(min(stockdata_filtered$Close),max(stockdata_filtered$Close),mean(stockdata_filtered$Close),median(stockdata_filtered$Close))
    summaryStockData(data.frame(table_headers, table_values))
    
    # check S&P checkbox
    if (input$gspc == 1){
      # get S&P data
      gspcdata <- data.frame(read.csv('GSPC.csv'))
      gspcdata$Date <-as.numeric(as.Date(gspcdata$Date))
      # SQL filtering query
      gspcdata_filtered <-sqldf(paste('SELECT DATE, CLOSE FROM gspcdata WHERE DATE BETWEEN', date_start, 'AND', date_end))
      
      # set scalefactor for y-axis
      stock_max <- max(stockdata_filtered$Close)
      gspc_max <- max(gspcdata_filtered$Close)
      gspcdata_filtered$Date <- as.Date(as.numeric(gspcdata_filtered$Date))  
      scale_factor <- gspc_max / stock_max
      
      # pring graph with S&P
      g <- g + 
        geom_line(data=gspcdata_filtered,aes(x=Date, y=Close/scale_factor), color='red') +     
        scale_y_continuous(name = "Stock Price", 
                           sec.axis = sec_axis(~.*scale_factor, name = "S&P")) + 
        theme(axis.text.y.right =  element_text(color = 'red'),
              axis.title.y.right = element_text(color='red')) 
      
      print(g)
    }
    
  })
  
  # print reactive table
  output$summaryTable <- renderTable(summaryStockData(), colnames = FALSE)
  
}




shinyApp(ui, server)
