# Relevant libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

# Imports the data from a csv file
credit <- read.csv("C:/Users/MissD/Documents/R_Project_311/R_credit/Credit_card_analysis_static/credit_card_transaction_flow.csv", stringsAsFactors = FALSE)

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

#This is for the heatmap, so we decided to preprocess the data
# Convert Gender and Category columns to factors
more_credit$Category <- as.factor(more_credit$Category)

# Create a contingency table (matrix)
heatmap_data <- table( more_credit$Age,more_credit$Category)

# Define UI
ui <- fluidPage(
  titlePanel("Credit Card Transactions Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(h1("SideBar"),
      selectInput("categoryFilter", "Select Category", choices = c("All", unique(more_credit$Category))),
      # Adds default slider values to be used in the histogram
      sliderInput(inputId = "bins",
                min = 1,
                label="Number of bins in the histogram:",
                max = 50,
                value = 30),
      
      dateRangeInput("dateRange", "Select Date Range", start = min(more_credit$Date), end = max(more_credit$Date)),
      img(src = "Credit.png", height = 100, width = 100)
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
      plotOutput("histogram"),
      plotOutput("boxplot"),
      plotOutput("stackedbar"),
      plotOutput("piechart"),
      plotOutput("scatterplot"),
      plotOutput("barplot")
    )
    
  )
)

# Define server
server <- function(input, output) {
  
  filtered_data <- reactive({
    # Apply filters to the data
    filtered_data <- more_credit
    
    if (input$categoryFilter != "All") {
      filtered_data <- filtered_data %>% filter(Category == input$categoryFilter)
    }
    
    return(filtered_data)
  })
  
  # Histogram plotted by category
  output$histogram <- renderPlot({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    x <- filtered_data_plot$Transaction.Amount
    bins <- seq(min(x), max(x), length.out = input$bins + 2)
    print(input$bins)
    
    hist(x,
         breaks = bins,
         main = "Histogram on Transaction Amount",
         xlab = "Transaction Amount in Dollars",
         ylab = "Frequency",
         col = "blue",
         border = "black",
         
    )
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    boxplot(filtered_data_plot$Transaction.Amount ~ filtered_data_plot$Category,
            main = "Boxplot of Categories by Transaction",
            xlab = "Categories",
            ylab = "Transaction Amount in Dollars",
            border = "black",
            col = "grey"
    )
  })
  
  # Stacked Bar Plot
  output$stackedbar <- renderPlot({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    ggplot(filtered_data_plot, aes(fill = Category, y = Transaction.Amount, x = Month)) + 
      geom_bar(stat = 'identity') +
      labs(title = "Stacked Bar Plot of Categories by Month",
           x = "Month",
           y = "Transaction Amount")
  })
  
  # Pie Chart
  output$piechart <- renderPlot({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    gender_counts <- table(filtered_data_plot$Gender)
    colors <- c("green", "pink", "skyblue")
    pie(gender_counts,
        main = "Pie Chart of Gender Distribution",
        labels = c("Not specified", "Female", "Male"),
        edges = 400,
        radius = 0.8,
        col = colors,
        border = "black",
        clockwise = TRUE,
        angle = 60
    )
  })
  
  # Scatter Plot
  output$scatterplot <- renderPlot({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    ggplot(filtered_data_plot, aes(x = Age, y = Transaction.Amount)) +
      geom_point() +
      labs(title = "Scatter Plot of Transaction Amount vs. Age",
           x = "Age",
           y = "Transaction Amount")
  })
  
  # Bar Plot
  output$barplot <- renderPlot({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    ggplot(filtered_data_plot, aes(x = Category, y = Transaction_Category_Count / 1000, fill = Transaction_Category_Count)) +
      geom_bar(stat = "identity") +
      labs(title = "Bar Plot of Transaction Category Count",
           x = "Transaction Category",
           y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

#I'm a contributor
