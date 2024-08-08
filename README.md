# RFM Analysis: Customer Segmentation

This repository contains an implementation of RFM (Recency, Frequency, Monetary) analysis for customer segmentation using R. RFM analysis is a method used to evaluate customer value and behavior based on purchasing patterns, which can help businesses tailor their marketing strategies effectively.

## Table of Contents

- [Introduction](#introduction)
- [Components of RFM](#components-of-rfm)
- [Project Structure](#project-structure)
- [Project Conclusion](#project-conclusion)
- [Requirements](#requirements)


## Introduction

RFM analysis is a data-driven marketing technique used to categorize customers based on their purchasing behavior:

- **Recency**: How recently a customer made a purchase.
- **Frequency**: How often a customer makes a purchase.
- **Monetary**: How much money a customer spends.

By analyzing these dimensions, businesses can segment their customers, allowing for more personalized marketing and sales strategies.

## Components of RFM

1. **Recency**: Number of days since the customer's last purchase.
2. **Frequency**: Total number of purchases made by the customer.
3. **Monetary Value**: Total spending by the customer.

## Project Structure

- **Data Loading**: The project loads data from an Excel file and preprocesses it for analysis.
- **Data Cleaning**: Handles missing values, duplicates, and negative values.
- **Exploratory Data Analysis (EDA)**: Provides insights into sales trends, top products, and customer segments.
- **RFM Segmentation**: Computes RFM scores and segments customers into groups.
- **Visualization**: Includes plots to visualize customer distribution and segments.

## Project Conclusion
The RFM analysis revealed the following customer distribution across segments:

- **Champions**: 636 customers
- **Loyal Customers**: 773 customers
- **Potential Loyalists**: 510 customers
- **New Customers**: 66 customers
- **Promising**: 112 customers
- **Need Attention**: 175 customers
- **At Risk**: 601 customers
- **Can't Lose**: 97 customers
- **About to Sleep**: 337 customers
- **Hibernating**: 1005 customers

The analysis highlights that the largest segment is "Hibernating," indicating many customers have not engaged recently. Meanwhile, "Champions" and "Loyal Customers" are key groups for nurturing with tailored marketing strategies. By focusing on "Potential Loyalists" and "Promising" customers, businesses can further enhance customer retention and conversion rates.

## Requirements

- R version 4.0 or higher
- R packages: `caret`, `dplyr`, `ggplot2`, `lubridate`, `tidyr`, `readxl`, `skimr`, `treemap`


