# Relevant libraries 
library(dplyr)          # Data manipulation library
library(ggplot2)        # Static visualization library
library(lubridate)      # Datetime library

# Imports the data from a csv file
credit <- read.csv("~/Ubuntu_projects/R/credit-card-transaction/credit_card_transaction_flow.csv")

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

boxplot(more_credit$Transaction.Amount ~ more_credit$Category,
        main = "Boxplot of Categories by Transaction",
        xlab = "Categories",
        ylab = "Transaction Amount in Dollars",
        border= "black",
        col= "grey",
)

#Creating a Stacked bar plot
ggplot(more_credit, aes(fill=Category, y=Transaction.Amount, x=Month)) + 
  geom_bar(Category='stack',stat='identity')

