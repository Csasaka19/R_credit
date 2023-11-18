# Credit Card Transaction Dataset

## Table of Contents
- [Features](#features)
- [Dataset Overview](#dataset-overview)
- [Initial Cleaning and Analysis](#initial-cleaning-and-analysis)
- [Plots](#plots)
    - [Histogram of Transaction Amount](#histogram-of-transaction-amount)
    - [Boxplot of Transaction Amount by Category](#boxplot-of-transaction-amount-by-category)
    - [Line Plot of Transaction Frequency Over Time](#line-plot-of-transaction-frequency-over-time)
    - [Stacked bar_plot of Category Transaction Amount](#stacked-bar-plot-of-transaction-amount-by-category)
    - [Bar Plot of Transaction Category Count](#bar-plot-of-transaction-category-count)
    - [Scatter Plot of Transaction Amount vs. Age](#scatter-plot-of-transaction-amount-vs-age)
    - [Pie Chart of Gender Distribution](#pie-chart-of-gender-distribution)
    - [Heatmap of Correlations](#heatmap-of-correlations)
- [Why Analyze?](#why-analyze)

## Features:

1. **Customer ID:** Unique identifiers for every customer.
2. **Name:** First name of the customer.
3. **Surname:** Last name of the customer.
4. **Gender:** The gender of the customer.
5. **Birthdate:** Date of birth for each customer.
6. **Transaction Amount:** The dollar amount for each transaction.
7. **Date:** Date when the transaction occurred.
8. **Merchant Name:** The name of the merchant where the transaction took place.
9. **Category:** Categorization of the transaction.

## Dataset Overview:

This dataset is valuable for analytics of various trends within the transaction world and can be particularly useful in fraud detection by isolating irregular transaction activities.

## Initial Cleaning and Analysis:

1. **Age of Credit Card Owners:** Calculate and analyze the age of credit card owners based on their birthdate.

2. **Frequency of Transactions:** Determine the frequency of transactions to understand how often customers engage in transactions.

3. **Category Transaction Count:** Analyze the count of transactions within each category.

4. **Month and Exact Dates of Transactions:** Extract month and exact dates of transactions (Year is constant - 2023).

## Plots:

### Histogram of Transaction Amount:
- Visualize the distribution of transaction amounts using a histogram.
- Identify the most common transaction amounts and detect outliers.

### Boxplot of Transaction Amount by Category:
- Compare the distribution of transaction amounts across different categories.
- Identify categories with higher or lower transaction amounts.

### Stacked Bar plot of Transaction Amount by Category:
- The stacked bar plot shows the distinct category of items purchased by amount over a 12-month period.
- This identifies categories with higher or lower transaction amounts.

### Line Plot of Transaction Frequency Over Time:
- Show how transaction frequency changes over time (using month and year).
- Identify trends or seasonality in transaction frequency.

### Bar Plot of Transaction Category Count:
- Display the count of transactions in each category.
- Identify the most popular transaction categories.

### Scatter Plot of Transaction Amount vs. Age:
- Explore the relationship between transaction amount and customer age using a scatter plot.
- Identify patterns or correlations between age and transaction amount.

### Pie Chart of Gender Distribution:
- Visualize the distribution of gender among customers using a pie chart.
- Provide an overview of the gender distribution in the dataset.

### Heatmap of Correlations:
- Create a heatmap to visualize correlations between numerical variables (e.g., transaction amount, age, transaction frequency).
- Identify relationships between different variables.

## Why Analyze?

1. **Insight into Customer Behavior:**
   Analyzing transaction frequency, amount, and categories provides insights into customer behavior and preferences.

2. **Temporal Trends:**
   Analyzing transactions over time helps identify temporal trends, seasonality, or patterns valuable for understanding customer behavior.

3. **Identifying Outliers:**
   Plots like boxplots and histograms aid in identifying outliers in transaction amounts, allowing for further investigation.

4. **Demographic Analysis:**
   Age and gender analysis helps understand the demographics of customers and their spending patterns.

5. **Category Insights:**
   Analyzing transaction categories provides insights into which types of merchants or transactions are more common among customers.

## Static analysis of this data was done in the credit.R file.
