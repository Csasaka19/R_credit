# Relevant libraries
library(shiny)
library(dplyr)
library(lubridate)
library(plotly)

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
more_credit$Category <- as.factor(more_credit$Category)

# Define UI
ui <- navbarPage(
  title = "Credit Card Transactions Analysis Dashboard",
  
  tabPanel(
    "Plots View",
    sidebarLayout(
      sidebarPanel(
        h1("SideBar"),
        br(),
        img(src = "credit2.jpg", height = 200, width = 200),
        br(),
        br(),
        dateRangeInput("dateRange", "Select Date Range", start = min(more_credit$Date), end = max(more_credit$Date)),
        br(),
        checkboxGroupInput("categoryFilter", "Select Category:", choices = c("All", unique(more_credit$Category))),
        sliderInput(inputId = "amountFilter", label = "Filter by Transaction Amount:", min = 0, max = max(more_credit$Transaction.Amount), value = c(0, max(more_credit$Transaction.Amount)))
      ),
      mainPanel(
        h1("Credit Main Panel"),
        h3("Features"),
        h5(strong("Customer ID: "),"Unique identifiers for every customer"),
        h5(strong("Name: "),"First name of the customer."),
        h5(strong("Surname: "),"Last name of the customer."),
        h5(strong("Gender: "),"The gender of the customer."),
        h5(strong("Birthdate: "),"Date of birth for each customer."),
        h5(strong("Transaction Amount: "),"The dollar amount for each transaction."),
        h5(strong("Date: "),"Date when the transaction occurred."),
        h5(strong("Merchant Name: "),"The name of the merchant where the transaction took place."),
        h5(strong("Category: "),"Categorization of the transaction."),
        h3("Why Analyze?"),
        h5("1.Insight into Customer Behavior: Analyzing transaction frequency, amount, and categories provides insights into customer behavior and preferences."),
        h5("2.Temporal Trends: Analyzing transactions over time helps identify temporal trends, seasonality, or patterns valuable for understanding customer behavior."),
        h5("3.Identifying Outliers: Plots like boxplots and histograms aid in identifying outliers in transaction amounts, allowing for further investigation."),
        h5("4.Demographic Analysis: Age and gender analysis helps understand the demographics of customers and their spending patterns."),
        h5("5.Category Insights: Analyzing transaction categories provides insights into which types of merchants or transactions are more common among customers."),
        tabsetPanel(
          tabPanel("Histogram", plotlyOutput("histogram")),
          tabPanel("Boxplot", plotlyOutput("boxplot")),
          tabPanel("Stacked Bar Plot", plotlyOutput("stackedbar")),
          tabPanel("Pie Chart", plotlyOutput("piechart")),
          tabPanel("Scatter Plot", plotlyOutput("scatterplot")),
          tabPanel("Bar Plot", plotlyOutput("barplot")),
          tabPanel("Heatmap", plotlyOutput("heatmap"))
        )
      )
    )
  ),
  
  tabPanel(
    "Additional Options",
    sidebarLayout(
      sidebarPanel(
        h1("Additional Options"),
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
)

# Define server
server <- function(input, output) {
  
  filtered_data <- reactive({
    # Apply filters to the data
    filtered_data <- more_credit
    
    if (input$categoryFilter != "All") {
      filtered_data <- filtered_data %>% filter(Category %in% input$categoryFilter)
    }
    
    filtered_data <- filtered_data %>% filter(Transaction.Amount >= input$amountFilter[1] & Transaction.Amount <= input$amountFilter[2])
    
    filtered_data <- filtered_data %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    
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