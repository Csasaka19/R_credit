# Relevant libraries 
library(dplyr)          # Data manipulation library
library(ggplot2) # Static visualization library
library(lubridate)      # Datetime library

# Imports the data from a csv file
credit <- read.csv("~/R_credit/credit-card-transaction/credit_card_transaction_flow.csv")

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

ggplot(more_credit, aes(x = paste(Year, Month, sep = "-"), group = 1)) +
  geom_line(stat = "count", aes(y = after_stat(count), color = "blue")) +
  scale_x_discrete(labels = scales::date_format("%Y-%m")) +
  labs(title = "Line Plot of Transaction Frequency Over Time",
       x = "Month and Year",
       y = "Transaction Frequency") +
  theme_minimal()
