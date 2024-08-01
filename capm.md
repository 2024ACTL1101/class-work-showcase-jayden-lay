---
title: "ACTL1101 Assignment Part B"
author: "Jayden Lay"
date: "2024 T2"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}

#I will first ensure all necessary libraries are installed.

knitr::opts_chunk$set(echo = TRUE)
library(quantmod)
library(ggplot2)
library(tidyverse)
```

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data processing}
colSums(is.na(df))
df <- df %>%
  fill(RF, .direction = "down") 

```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r Daily Return}

df <- df %>%
  mutate(
    AMD_Return = (AMD / lag(AMD) - 1),
    GSPC_Return = (GSPC / lag(GSPC) - 1)
  ) %>%
  na.omit()

print(head(df))
print(head(df$AMD_Return))
print(head(df$GSPC_Return))

```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}

df <- df %>%
  mutate(
    RF_Rate = ((1 + RF / 100)^(1/360) - 1)
  )

print(head(df$RF_Rate))

```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}

df <- df %>%
  mutate(
    Excess_AMD = AMD_Return - RF_Rate,
    Excess_GSPC = GSPC_Return - RF_Rate
  )

print(head(df$Excess_AMD))
print(head(df$Excess_GSPC))

```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}

capm_model <- lm(Excess_AMD ~ Excess_GSPC, data = df)

summary(capm_model)

beta <- coef(capm_model)[2]
alpha <- coef(capm_model)[1]
cat("Beta:", beta, "\n")
cat("Alpha:", alpha, "\n")

if (beta > 1) {
  cat("AMD is more volatile than the market. High risk, potentially high returns.\n")
} else if (beta < 1) {
  cat("AMD is less volatile than the market. Lower risk, potentially lower returns.\n")
} else {
  cat("AMD has the same volatility as the market.\n")
}

```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**The estimated beta for AMD is about 1.57, suggesting that AMD's stock exhibits greater responsiveness to market fluctuations, making it more volatile than the overall market. This high beta signifies AMD's increased sensitivity to market dynamics, illustrated by significant events like its $35 billion acquisition of Xilinx, which was announced in October 2020 and finalized in February 2022. This strategic acquisition was intended to boost AMD's capabilities and expand its market share in the semiconductor sector, sparking heightened interest and speculation among investors. This impact was evident when AMD's stock surged by 7% on the day of the announcement due to favorable market sentiment.**


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}

plot_data <- data.frame(
  Market = df$Excess_GSPC,
  AMD = df$Excess_AMD
)

ggplot(plot_data, aes(x = Market, y = AMD)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "CAPM: AMD vs Market",
       x = "Market Excess Return",
       y = "AMD Excess Return") +
  theme_minimal()

```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Predicted AMD Return: 0.1803099 
90% Prediction Interval: -0.4900223 0.8506421 **

```{r pi}

sf <- summary(capm_model)$sigma

annual_sf <- sf * sqrt(252)

current_rf_rate <- 0.05
expected_market_return <- 0.133

predicted_amd_return <- current_rf_rate + beta * (expected_market_return - current_rf_rate)

annual_pred_interval <- c(
  predicted_amd_return - 1.645 * annual_sf,
  predicted_amd_return + 1.645 * annual_sf
)

cat("Predicted AMD Return:", predicted_amd_return, "\n")
cat("90% Prediction Interval:", annual_pred_interval, "\n")


```

