# Relevant libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

# Imports the data from a csv file
credit <- read.csv("~/Documents/R_credit/Credit_card_analysis_static/credit_card_transaction_flow.csv", stringsAsFactors = FALSE)

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

# Define UI
ui <- fluidPage(
  titlePanel("Credit Card Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("categoryFilter", "Select Category", choices = c("All", unique(more_credit$Category))),
    ),
    mainPanel(
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
  
  # Histogram
  output$histogram <- renderPlot({
    filtered_data_plot <- filtered_data()
    
    if (is.null(filtered_data_plot) || nrow(filtered_data_plot) == 0) {
      # Handle the case when the filtered data is empty or NULL
      return(NULL)
    }
    
    hist(filtered_data_plot$Transaction.Amount,
         main = "Histogram on Transaction Amount",
         xlab = "Transaction Amount in Dollars",
         ylab = "Frequency",
         col = "blue",
         border = "black",
         breaks = 12
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
shinyApp(ui, server)
