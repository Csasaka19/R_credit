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

# New table for the heatmap
heatmap_data <- table(more_credit$Age, more_credit$Category)

# Convert more_credit$Category to a factor
more_credit$Category <- as.character(more_credit$Category)

# Define UI
ui <- dashboardPage(
  
  # Header Area of the web dashboard.
  dashboardHeader(title = "Credit Card Transactions Analysis Dashboard", titleWidth = 650,
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
<<<<<<< HEAD
                  column(width = 8, tags$img(src = "credit-card.jpg", width = 600, height = 300), tags$a("Photo of a Credit Card", align = "left")),
=======
                    column(width = 8, tags$img(src = "credit-card.jpg", width = 600, height = 300)),
>>>>>>> a0129e3c318ff394e6e0960f34af11386f9d0a5a
                  tags$br(),
                  column(
                    width = 4,
                    tags$br(),
                    tags$h2("Description:"),
                    tags$p("Welcome to the world of credit card transactions! This dataset provides a treasure trove of insights into customers' spending habits, transactions, and more. Whether you're a data scientist, analyst, or just someone curious about how money moves, this dataset is for you."),
                    tags$br(),
                    tags$p(tags$strong("Why this dataset matters:")),
                    tags$p("Understanding consumer spending patterns is crucial for businesses and financial institutions. This dataset is a goldmine for exploring trends, patterns, and anomalies in financial behavior. It can be used for fraud detection, marketing strategies, and much more.")
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
                           tabPanel("Violin Plot", value = "violin", withSpinner(plotlyOutput("violinplot"), type = 1, color = "blue", size = 3)),
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
                     "Contributors: ",
                     fluidRow(
                       column(3,
                              box(
                                title = "Clive Sasaka",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 12,
                                align = "center",
                                tags$img(src = "github.png", width = 60, height = 60),
                                tags$p(strong("Csasaka19")),
                                tags$a("GitHub Profile", href = "https://github.com/Csasaka19", target = "_blank")
                              )
                       ),
                       column(3,
                              box(
                                title = "Baraka Mbugua",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 12,
                                align = "center",
                                tags$img(src = "github.png", width = 60, height = 60),
                                tags$p(strong("eiidoubleyuwes")),
                                tags$a("GitHub Profile", href = "https://github.com/eiidoubleyuwes", target = "_blank")
                              )
                       ),
                       column(3,
                              box(
                                title = "Eugene Mwangi",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 12,
                                align = "center",
                                tags$img(src = "github.png", width = 60, height = 60),
                                tags$p(strong("Eugene600")),
                                tags$a("GitHub Profile", href = "https://github.com/Eugene600", target = "_blank")
                              )
                       ),
                       column(3,
                              box(
                                title = "Desiree Michere",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 12,
                                align = "center",
                                tags$img(src = "github.png", width = 60, height = 60),
                                tags$p(strong("DeeMichere")),
                                tags$a("GitHub Profile", href = "https://github.com/DeeMichere", target = "_blank")
                              )
                       )
                     )
                   )
            )
          )
        )))


# Define server
server <- function(input, output) {
  
  # Data table output for browsing.
  output$dataT <- renderDataTable(more_credit)
  
  # For structure output for the user to see
  output$structure <- renderPrint(
    {
      more_credit %>% str()
    }
  )
  
  # Display summary of the data
  output$summary <- renderPrint({
    more_credit %>% summary()
  })
  
  
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
    
    # Check if the filtered data frame is empty
    if (nrow(filtered_data_plot) == 0) {
      return(NULL)
    }
    
    plot_ly(filtered_data_plot, x = ~Transaction.Amount, type = "histogram", color = ~Category) %>%
      layout(title = "Histogram on Transaction Amount",
             xaxis = list(title = "Transaction Amount in Dollars"),
             yaxis = list(title = "Frequency"),
             barmode = "stack")
  })
  
  # Boxplot
  output$boxplot2 <- renderPlotly({
    filtered_data_plot <- filtered_data()
    
    # Check if the filtered data frame is empty
    if (nrow(filtered_data_plot) == 0) {
      return(NULL)
    }
    
    plot_ly(filtered_data_plot, y = ~Transaction.Amount, color = ~Category, type = "box") %>%
      layout(title = "Box Plot on Transaction Amount",
             xaxis = list(title = "Category"),
             yaxis = list(title = "Transaction Amount in Dollars"))
  })
  
  # Stacked Bar Plot
  output$stackedbar <- renderPlotly({
    filtered_data_plot <- filtered_data()
    
    # Check if the filtered data frame is empty
    if (nrow(filtered_data_plot) == 0) {
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
    
    # Check if the filtered data frame is empty
    if (nrow(filtered_data_plot) == 0) {
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
    
    # Check if the filtered data frame is empty
    if (nrow(filtered_data_plot) == 0) {
      return(NULL)
    }
    
    plot_ly(filtered_data_plot, x = ~Age, y = ~Transaction.Amount, type = "scatter", mode = "markers") %>%
      layout(title = "Scatter Plot of Transaction Amount vs. Age",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Transaction Amount"))
  })##
  output$violinplot <- renderPlotly({
    filtered_data_plot <- filtered_data()
    
    # Check if the filtered data frame is empty
    if (nrow(filtered_data_plot) == 0) {
      return(NULL)
    }
    
    # Assuming 'Category' is a factor variable
    plot_ly(x = filtered_data_plot$Category, y = filtered_data_plot$Transaction.Amount,
            type = "violin", box = list(visible = TRUE, width = 0.1),
            line = list(color = "black"),
            points = "all", jitter = 0.3, marker = list(color = "orange")) %>%
      layout(title = "Violin Plot of Transaction Amount by Category",
             xaxis = list(title = "Category"),
             yaxis = list(title = "Transaction Amount"))
  })
  
  
  
  # Bar Plot
  output$barplot <- renderPlotly({
    filtered_data_plot <- filtered_data()
    
    # Check if the filtered data frame is empty
    if (nrow(filtered_data_plot) == 0) {
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
    
    # Check if the filtered data frame is empty
    if (nrow(filtered_data_plot) == 0) {
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
    
    # Check if the filtered data frame is empty
    if (nrow(filtered_data_plot) == 0) {
      return(NULL)
    }
    
    filtered_data_plot %>% select(Customer.ID, Name, Surname, Gender, Birthdate, Transaction.Amount, Date, Merchant.Name, Category)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
