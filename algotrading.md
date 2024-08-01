---
title: "ACTL1101 Assignment Part A"
author: "Jayden Lay"
date: "2024 T2"
output:
  html_document:
    df_print: paged
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("/Users/jaydenlay/Desktop/Uni/2024 T2/ACTL1101/ACTL1101 Assignment V1/AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

# Select only relevant columns
amd_df <- amd_df[, c("date", "close")]

```
##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  if (previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
  } else if (amd_df$close[i] < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
  } else if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * accumulated_shares
    accumulated_shares <- 0
  }
  previous_price <- amd_df$close[i]
  amd_df$accumulated_shares[i] <- accumulated_shares
}

```


## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}

start_date <- as.Date("2021-09-17")
end_date <- as.Date("2024-05-17")
amd_df <- subset(amd_df, date >= start_date & date <= end_date)
plot(amd_df$date, amd_df$close, type = 'l')

```


## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}

#I opted to implement a Profit and Loss Algorithm

total_profit_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE)

total_invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)

ROI <- (total_profit_loss / total_invested_capital) * 100

cat("Total Profit/Loss: ", total_profit_loss, "\n")
cat("Total Invested Capital: ", total_invested_capital, "\n")
cat("ROI: ", ROI, "%", "\n")

```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option}

#I chose to implement a Profit Taking Mechanism (Option 1)
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0

previous_price <- 0
share_size <- 100
accumulated_shares <- 0
average_purchase_price <- 0

for (i in 1:nrow(amd_df)) {
  if (previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
    average_purchase_price <- (average_purchase_price * (accumulated_shares - share_size) + amd_df$close[i] * share_size) / accumulated_shares
  } else if (amd_df$close[i] < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
    average_purchase_price <- (average_purchase_price * (accumulated_shares - share_size) + amd_df$close[i] * share_size) / accumulated_shares
  } else if (amd_df$close[i] >= average_purchase_price * 1.2) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * (accumulated_shares / 2)
    accumulated_shares <- accumulated_shares / 2
  }
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * accumulated_shares
    accumulated_shares <- 0
  }
  previous_price <- amd_df$close[i]
  amd_df$accumulated_shares[i] <- accumulated_shares
}


```


## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}

total_profit_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE)
total_invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)
ROI <- (total_profit_loss / total_invested_capital) * 100

cat("Total Profit/Loss after strategy modification: ", total_profit_loss, "\n")
cat("Total Invested Capital after strategy modification: ", total_invested_capital, "\n")
cat("ROI after strategy modification: ", ROI, "%", "\n")

# Discussion

# On September 17th 2022, AMD's CEO Lisa Su released its Q3 earnings report, which far outperformed close competitor Nvidia's quarterly revenue and net income as a result of major developmental strides in 3nm CPU architectures. This breakthrough captivated millions of professional and recreational PC users increasing TAM market share of AMD CPUs by an unprecedented 0.96% just this quarter. As a result, bullish future focused investors prompted a surge in AMD's stock prices as a byproduct of promising future prospects and existing growth in financial performance; My initial strategy benefited from this case as it held shares through the earnings and product announcements, having high levels of leverage during the stock price surge. This created an ROI of 176.71% and a total P/L of $6,491,039, which far outperformed the Price-Taking mechanism which only produced a 22.85% ROI and $839,461 P/L. This is due to the Price-taking mechanism selling shares prematurely after each 20% surge in AMD's share price, lowering leverage as AMD continued to grow during this time period. Overall, indicating that the second strategy saw missed opportunities in AMD's long term upwards trend.

```

Sample Discussion: On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD shares on the following Thursday suggests that investors believe in the chipmaker's upward potential and market expectations; My first strategy earned X dollars more than second strategy on this day, therefore providing a better ROI.






