# Relevant libraries
library(shiny)
library(dplyr)
library(lubridate)
library(plotly)
library(shinydashboard)
library(shinycssloaders)

# Imports the data from a csv file
credit <- read.csv("~/R_credit/Credit-App/credit_card_transaction_flow.csv", stringsAsFactors = FALSE)

# Convert the 'Date' column to Date object
credit$Date <- as.Date(credit$Date)

more_credit <- credit %>%
  select(Customer.ID, Birthdate, Date, Transaction.Amount, Category, Gender, Name, Surname, Merchant.Name) %>%
  distinct() %>%
  filter(!is.na(Birthdate)) %>%
  mutate( 
    Birthdate = dmy(Birthdate), 
    Age = as.integer(interval(Birthdate, Sys.Date()) / years(1)),
    Month = month(Date),
    Year = year(Date),
    Transaction_Frequency = ave(Transaction.Amount, Customer.ID, FUN = length),
    Transaction_Category_Count = ave(Transaction.Amount, Category, FUN = length)
  )

# New table for the the heatmap
heatmap_data <- table(more_credit$Age, more_credit$Category)

# Convert more_credit$Category to a factor
more_credit$Category <- as.character(more_credit$Category)

# Define UI
ui <- dashboardPage(
  
  # Header Area of the web dashboard.
  dashboardHeader(title = "Credit Card Transactions Analysis Dashboard", titleWidth = 650,
                  tags$li(class="dropdown",tags$a(href="" ,icon("linkedin"), "My Profile", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://www.github.com/Csasaka19/R_credit", icon("github"), "Source Code", target="_blank"))),
  
  # Main side menu displaying options of the dashboard
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "data", icon = icon("database")),
      menuItem("Visualization", tabName = "vis", icon = icon("chart-line")),
      menuItem("Customer Details", tabName = "add", icon = icon("info-circle")),
      menuItem("Developers", tabName = "devs", icon = icon("code"))
    )
  ),
  
  # Various main pages to be displayed when a certain menu item is chosen from the sidebar
  dashboardBody(
    tabItems(
      # Intro to the dataset and some options to play with the data.
      tabItem(
        tabName = "data",
        tabBox(
          id = "t1",
          width = 12,
          tabPanel(
            "About",
            icon = icon("address-card"),
            fluidRow(
              column(width = 8, tags$img(src = "crime.jpg", width = 600, height = 300)),
              tags$br(),
              tags$a("Photo of Credit card transactions", align = "center"),
              column(
                width = 4,
                tags$br(),
                tags$p("This dataset description placeholder......"),
                tags$br(),
                tags$p(tags$strong("Why Analyze?")),
                tags$ul(
                  tags$li("Insight into Customer Behavior: Analyzing transaction frequency, amount, and categories provides insights into customer behavior and preferences."),
                  tags$li("Temporal Trends: Analyzing transactions over time helps identify temporal trends, seasonality, or patterns valuable for understanding customer behavior."),
                  tags$li("Identifying Outliers: Plots like boxplots and histograms aid in identifying outliers in transaction amounts, allowing for further investigation."),
                  tags$li("Demographic Analysis: Age and gender analysis helps understand the demographics of customers and their spending patterns."),
                  tags$li("Category Insights: Analyzing transaction categories provides insights into which types of merchants or transactions are more common among customers.")
                )
              )
            )
          ),
          tabPanel("Data", dataTableOutput("dataT"), icon = icon("table")),
          tabPanel("Structure", verbatimTextOutput("structure"), icon = icon("uncharted")),
          tabPanel("Summary Stats", verbatimTextOutput("summary"), icon = icon("chart-pie"))
        )
      ),
           
  # The visual part of the project(some basic plots created by the group.
  tabItem(tabName = "vis",
          dateRangeInput("dateRange", "Select Date Range", start = min(more_credit$Date), end = max(more_credit$Date)),
                 br(),
                selectInput("categoryFilter", "Select Category: ", choices = c("All", unique(more_credit$Category))),
          
          tabBox(id = "t2", width = 12,
                 tabsetPanel(
                   tabPanel("Histogram", value = "hist", withSpinner(plotlyOutput("histogram"), type = 1, color = "blue", size = 3)),
                   tabPanel("Boxplot", value = "box", withSpinner(plotlyOutput("boxplot"), type = 1, color = "blue", size = 3)),
                   tabPanel("Stacked Bar Plot", value = "stack", withSpinner(plotlyOutput("stackedbar"), type = 1, color = "blue", size = 3)),
                   tabPanel("Pie Chart", value = "pie", withSpinner(plotlyOutput("piechart"), type = 1, color = "blue", size = 3)),
                   tabPanel("Scatter Plot", value = "scatter", withSpinner(plotlyOutput("scatterplot"), type = 1, color = "blue", size = 3)),
                   tabPanel("Bar Plot", value = "bar", withSpinner(plotlyOutput("barplot"), type = 1, color = "blue", size = 3)),
                   tabPanel("Heatmap", value = "heat", withSpinner(plotlyOutput("heatmap"), type = 1, color = "blue", size = 3))
                        ))
          ),
  
  
  
  tabItem(
    tabName = "add",
    tabBox(id = "t3", width = 12,
           tabPanel(
             "Browse Customer Details",
             sidebarLayout(
               sidebarPanel(
                 h1("Customers in the Dataset"),
                 br(),
                 checkboxInput("showDetails", "Show Customer Details", value = FALSE)
               ),
               mainPanel(
                 h1("Customer Details"),
                 conditionalPanel(
                   condition = "input.showDetails",
                   dataTableOutput("customerTable")
                 )
               )
             )
             
           )
  )),
  
tabItem(
    tabName = "devs",
    tabBox(id = 4, width = 12,
           tabPanel(
       tags$h1("Developers placeholder")
  ))
))))


# Define server
server <- function(input, output) {
  
  filtered_data <- reactive({
    # Apply filters to the data
    filtered_data <- more_credit
    
    if (input$categoryFilter != "All") {
      filtered_data <- filtered_data %>% filter(Category %in% input$categoryFilter)
    }
    
    #filtered_data <- filtered_data %>% filter(Transaction.Amount >= input$amountFilter[1] & Transaction.Amount <= input$amountFilter[2])
    
    #filtered_data <- filtered_data %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    
    return(filtered_data)
  })
  
  # Histogram plotted by category
  output$histogram <- renderPlotly({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    plot_ly(filtered_data_plot, x = ~Transaction.Amount, type = "histogram", color = ~Category) %>%
      layout(title = "Histogram on Transaction Amount",
             xaxis = list(title = "Transaction Amount in Dollars"),
             yaxis = list(title = "Frequency"),
             barmode = "stack")
  })
  
  # Boxplot
  output$boxplot <- renderPlotly({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    plot_ly(filtered_data_plot, x = ~Category, y = ~Transaction.Amount, type = "box") %>%
      layout(title = "Boxplot of Categories by Transaction",
             xaxis = list(title = "Categories"),
             yaxis = list(title = "Transaction Amount in Dollars"))
  })
  
  # Stacked Bar Plot
  output$stackedbar <- renderPlotly({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    plot_ly(filtered_data_plot, x = ~Month, y = ~Transaction.Amount, color = ~Category, type = "bar") %>%
      layout(title = "Stacked Bar Plot of Categories by Month",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Transaction Amount"))
  })
  
  # Pie Chart
  output$piechart <- renderPlotly({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    gender_counts <- table(filtered_data_plot$Gender)
    colors <- c("green", "pink", "skyblue")
    
    plot_ly(labels = c("Not specified", "Female", "Male"), values = gender_counts, type = "pie", marker = list(colors = colors)) %>%
      layout(title = "Pie Chart of Gender Distribution")
  })
  
  # Scatter Plot
  output$scatterplot <- renderPlotly({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    plot_ly(filtered_data_plot, x = ~Age, y = ~Transaction.Amount, type = "scatter", mode = "markers") %>%
      layout(title = "Scatter Plot of Transaction Amount vs. Age",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Transaction Amount"))
  })
  
  # Bar Plot
  output$barplot <- renderPlotly({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    plot_ly(filtered_data_plot, x = ~Category, y = ~Transaction_Category_Count / 1000, type = "bar", marker = list(color = ~Transaction_Category_Count)) %>%
      layout(title = "Bar Plot of Transaction Category Count",
             xaxis = list(title = "Transaction Category"),
             yaxis = list(title = "Count"),
             xaxis = list(tickangle = 45))
  })
  
  # Heatmap
  output$heatmap <- renderPlotly({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    heatmap_data <- table(filtered_data_plot$Age, filtered_data_plot$Category)
    
    plot_ly(z = heatmap_data, x = levels(filtered_data_plot$Category), y = levels(filtered_data_plot$Age), type = "heatmap") %>%
      layout(title = "Heatmap with Age and Categories",
             xaxis = list(title = "Category"),
             yaxis = list(title = "Age"))
  })
  
  # Customer Details Table
  output$customerTable <- renderDataTable({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    filtered_data_plot %>% select(Customer.ID, Name, Surname, Gender, Birthdate, Transaction.Amount, Date, Merchant.Name, Category)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)