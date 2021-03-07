library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(shinyWidgets)
library(tidyverse)


# data source
# https://data.world/fivethirtyeight/college-majors

# Load and clean data ----------------------------------------------
major <- read.csv("all-ages.csv")
major <- major[1:9]
major <- major%>% mutate(major,
                         income_class = case_when(Median < 48000 ~ 'low pay',
                                                  Median >= 48000 & Median < 60000 ~ 'medium pay',
                                                  Median >= 60000 ~ 'high pay'))

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "US College Majors")

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plots", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    
    # Inputs: select variables to plot ----------------------------------------------
    pickerInput(inputId = "majorSelect",
                label = "Select Major Category:",
                choices = sort(unique(major$Major_category)),
                multiple = TRUE,
                selected = unique(major$Major_category)),
    
    # Unemployment Selection ----------------------------------------------
    sliderInput("unemploymentSelect",
                "Unemployment Rate:",
                min = min(major$Unemployment_rate, na.rm = T),
                max = max(round(major$Unemployment_rate,2), na.rm = T),
                value = c(min(major$Unemployment_rate, na.rm = T), max(major$Unemployment_rate, na.rm = T)),
                step = 0.01),
    
    # Income Selection ----------------------------------------------
    checkboxGroupInput(inputId = "incomeSelect", 
                label = "Income:",
                choices = unique(major$income_class),
                selected = unique(major$income_class)
                ),
    
    # Reactive download button
    actionButton(inputId = "download",
                 label = "Download csv data")
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("num"),
            valueBoxOutput("unemployment"),
            valueBoxOutput("income")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plots",
                   width = 12,
                   tabPanel("Total Students", br(),br(),plotlyOutput("pie")),
                   tabPanel("Income Distribution",br(),br(),plotlyOutput("boxplot")),
                   tabPanel("Correlation between Unemployment Rate and Income", br(),br(),plotlyOutput("scatter")))
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidRow(
            box(title = "Selected Majors", DT::dataTableOutput("table"), width = 12))
  )
  
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  major_subset <- reactive({
    req(c(input$majorSelect,input$incomeSelect,input$unemploymentSelect))
    filter(major, Major_category %in% input$majorSelect & 
             income_class %in% input$incomeSelect &
             Unemployment_rate >= input$unemploymentSelect[1] & Unemployment_rate <= input$unemploymentSelect[2])
  })
  
  # A plot showing the total students under each major category -----------------------------
  output$pie<- renderPlotly({
    major_cat <-  major_subset() %>% 
                  group_by(Major_category) %>%
                  summarize(Total = sum(Total))
    fig <- plot_ly(labels = ~major_cat$Major_category, values = major_cat$Total, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF')) %>% 
           layout(title = 'Total number of students under each major category',
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  
  # A plot showing the income of each major category -----------------------------------
  output$boxplot <- renderPlotly({
    fig <- plot_ly(data = major_subset(), x = ~Median, color = ~Major_category, type = "box",                                   quartilemethod="exclusive",
                   showlegend = FALSE) %>% 
          layout(title = 'Income Distribution')
    fig
  })
  
  # A plot showing the correlation between unemployment rate and income -----------------------------------
  output$scatter <- renderPlotly({
    fig <- plot_ly(data = major_subset(), x = ~Unemployment_rate, y = ~Median, color = ~income_class) %>% 
          layout(title = 'Correlation between Unemployment Rate and Income')
    fig
  })
  
  # Data table of characters ----------------------------------------------
  output$table <- DT::renderDataTable({
    major_subset()
  })
  
  # Mass mean info box ----------------------------------------------
  output$num <- renderInfoBox({
    mj <- major_subset()
    num <- nrow(mj)
    num2 <- length(unique(mj$Major_category))
    
    infoBox("Total Majors Selected:", value = num, subtitle = paste("Total major categories:",num2), icon = icon("balance-scale"), color = "purple")
  })
  
  # unemployment rate value box ----------------------------------------------
  output$unemployment <- renderValueBox({
    mj <- major_subset()
    num <- round(mean(mj$Unemployment_rate, na.rm = T), 4) *100
    
    valueBox("Avg Unemployment Rate", value = paste(num,'%'), icon = icon("book"), color = "green")
  })
  
  # income value box ----------------------------------------------
  output$income <- renderValueBox({
    mj <- major_subset()
    num <- round(mean(mj$Median, na.rm = T), 0)
    
    valueBox("Avg Income", value = paste("$ ",num), icon = icon("money"), color = "blue")
  })
  
  # Write sampled data as csv ---------------------------------------
  observeEvent(eventExpr = input$download, 
               handlerExpr = {
                 filename <- paste0("majors_", str_replace_all(Sys.time(), ":|\ ", "_"), ".csv")
                 write.csv(major_subset(), file = filename) 
               }
  )
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)