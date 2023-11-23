# Relevant libraries 
library(dplyr)          # Data manipulation library
library(ggplot2) # Static visualization library
library(lubridate)      # Datetime library

# Imports the data from a csv file
credit <- read.csv("C:/Users/MissD/Documents/R_Project_311/R_credit/Credit_card_analysis_static/credit_card_transaction_flow.csv")

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
str(more_credit)
head(more_credit)

# Plots

# A histogram to track transaction amount and identify outliers
hist(more_credit$Transaction.Amount,
     main="Histogram on transaction Amount",
     xlab="Transaction Amount in Dollars",
     ylab="Frequency",
     col="blue",
     border="black",
     xlim= c(min(more_credit$Transaction.Amount), max(more_credit$Transaction.Amount)),
     ylim = c(0, max(more_credit$Transaction.Amount)),
     breaks= 12
)

# Box plot for Category analysis
boxplot(more_credit$Transaction.Amount ~ more_credit$Category,
        main = "Boxplot of Categories by Transaction",
        xlab = "Categories",
        ylab = "Transaction Amount in Dollars",
        border= "black",
        col= "grey",
)

# Creating a Stacked bar plot for category analysis
ggplot(more_credit, aes(fill=Category, y=Transaction.Amount, x=Month)) + 
  geom_bar(stat='identity')


# Create a pie chart on Gender
gender_counts <- table(more_credit$Gender)
colors <- c("green", "pink","skyblue")

#pie chart 
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

# Scatter plot
ggplot(more_credit, aes(x = Age, y = Transaction.Amount)) +
  geom_point() +
  labs(title = "Scatter Plot of Transaction Amount vs. Age",
       x = "Age",
       y = "Transaction Amount")


#Bar plot
ggplot(more_credit, aes(x = Category, y = Transaction_Category_Count /1000, fill = Transaction_Category_Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Plot of Transaction Category Count",
       x = "Transaction Category",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#This is for the heatmap, so we decided to preprocess the data
# Convert Gender and Category columns to factors
more_credit$Category <- as.factor(more_credit$Category)

# Create a contingency table (matrix)
heatmap_data <- table( more_credit$Age,more_credit$Category)
# Create heatmap
heatmap(
  heatmap_data,
  Colv = NULL,  # Turn off column clustering
  Rowv = NULL,  # Turn off row clustering
  col = c("orange","light blue"),  # Use a color palette
  scale = "none",  # Corrected scale argument
  xlab = "Category",
  ylab = "Age",
  main = "Heatmap with Age and Categories"
)