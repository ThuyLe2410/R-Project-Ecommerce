detach("package:datasets", unload=TRUE)
rm(list=ls())

# I. LOAD and EXPLORE DATA

## 1.1 Load necessary library
install.packages(c("caret", "dplyr", "ggplot2", "lubridate", "tidyr"))
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(tidyr)

## 1.2 Download and Load Data
install.packages("readxl")
library(readxl)
retail_data <- read_excel('./data/online_retail_II.xlsx', sheet='Year 2009-2010')
head(retail_data,5)

## 1.3 Explore the Dataset
typeof(retail_data)
is.list(retail_data)
retail_df <- as.data.frame(retail_data)
is.data.frame(retail_df)
names(retail_df) <- c("Invoice_Number", "Stock_Code", "Description", "Quantity",	"Invoice_Date", "Price",	"Customer_ID",	"Country")
summary(retail_df) 
str(retail_df)

library(skimr) # skim function display the numerical attributes from summary, missing values, quantile information and histogram
skim(retail_df)


# II. CLEAN DATA
## 2.1 Handle missing values
sapply(retail_df, function(x) sum(is.na(x))) # check for missing values
retail_df <- na.omit(retail_df) # remove missing values
summary(retail_df)

## 2.2 Handle duplicated and negative values
retail_df <- retail_df[!duplicated(retail_df),] # remove duplicated values
summary(retail_df)
retail_df <- retail_df[retail_df$Quantity > 0,] # remove negative quantity
retail_df <- retail_df[retail_df$Price > 0,] # remove negative price
summary(retail_df)

# III. EXPLORATORY DATA ANALYSIS (EDA)
## 3.1 Visualize Sales over time
library(ggplot2)
# Create a scatter plot with red points
ggplot(retail_df, aes(x=Invoice_Date, y=retail_df$Quantity*retail_df$Price)) +
  geom_line(color = 'blue') +
  labs(title = "Sales over time",
       x = 'Invoice Date',
       y = 'Total Revenue') +
  theme_gray()

## 3.2 Identify top product
retail_df$Amount <- retail_df$Quantity * retail_df$Price
top_products <- retail_df %>%
  group_by(Description) %>%
  summarise(Total_Revenue=sum(Amount)) %>%
  arrange(desc(Total_Revenue)) %>%
  head(10)

print(top_products)

ggplot(top_products, aes(x=reorder(Description, Total_Revenue), y=Total_Revenue)) +
  geom_bar(stat='identity', fill='steelblue')+
  coord_flip() +
  labs(title='Top 10 Products by Revenue', x="Product Description", y="Total Revenue") +
  theme_gray()

## 3.3 Analyze Customer Segments
top_customers <- retail_df %>%
  group_by(Customer_ID) %>%
  summarise(Total_Revenue=sum(Amount)) %>%
  arrange(desc(Total_Revenue)) %>%
  head(10)

ggplot(top_customers, aes(x=reorder(Customer_ID, -Total_Revenue), y=Total_Revenue))+
  geom_bar(stat='identity', fill='steelblue')+
  labs(title='Top 10 Customers by Revenue', x= 'Customer ID', y ='Total Revenue') +
  theme_gray()

## 3.4 Sales Distribution by Country
revenue_by_country <- retail_df %>%
  group_by(Country) %>%
  summarise(Total_Revenue=sum(Amount)) %>%
  arrange(desc(Total_Revenue))
  
revenue_by_country
ggplot(revenue_by_country, aes(x=reorder(Country, Total_Revenue), y=Total_Revenue)) +
  geom_bar(stat='identity', fill='steelblue') +
  coord_flip()
  labs(title='Revenue by country', x= 'Country', y ='Total Revenue') +
  theme_gray()

# IV. DATA PREPARATION
## 4.1 RFM Metrics 
#      We are going to analysis the Customers based on below 3 factors:
#           R (Recency): Number of days since last purchase
#           F (Frequency): Number of transactions
#           M (Monetary): Total amount of transactions (revenue contributed)

# Add new attribute: Monetary
retail_grp_monetary <- retail_df %>% group_by(Customer_ID) %>%
                              summarise(monetary = sum(Amount))
retail_grp_monetary

# Add new attribute: Frequency
retail_grp_freq <- retail_df %>% group_by(Customer_ID) %>%
                              summarise(frequency = n(), .groups = "drop")

retail_grp_freq

# Add new attribute: Recency
retail_df$Invoice_Date <- as.POSIXct(retail_df$Invoice_Date,format='%d-%m-%Y %H:%M')
retail_df$Invoice_Date <- as.Date(retail_df$Invoice_Date)
max_date <- max(retail_df$Invoice_Date)
max_date
retail_df$Diff <- (max_date - retail_df$Invoice_Date)

retail_grp_recency <- retail_df %>% group_by(Customer_ID) %>% 
                      summarize(recency = min(Diff, na.rm = TRUE))
retail_grp_recency$recency <- as.numeric(retail_grp_recency$recency, units = "days")
retail_grp_recency

# Merge the attributes to get the RFM data frame
rfm_df <- retail_grp_monetary %>%
  left_join(retail_grp_freq, by = "Customer_ID") %>%
  left_join(retail_grp_recency, by = "Customer_ID")
rfm_df

# 4.2 Create RFM score
rfm_df$revenue_score <- cut(rfm_df$total_revenue, 
                                  breaks=quantile(rfm_df$revenue, probs=seq(0,1,0.2), na.rm=TRUE),
                                  include.lowest = TRUE,
                                  labels=c(1,2,3,4,5))
rfm_df$frequency_score <- cut(rfm_df$frequency, 
                                  breaks=quantile(rfm_df$frequency, probs=seq(0,1,0.2), na.rm=TRUE),
                                  include.lowest = TRUE,
                                  labels=c(1,2,3,4,5))
rfm_df$recency_score <- cut(rfm_df$recency, 
                              breaks=quantile(rfm_df$recency, probs=seq(0,1,0.2), na.rm=TRUE),
                              include.lowest = TRUE,
                            labels=c(5,4,3,2,1))
rfm_df$rfm_score <- paste0(
  as.character(rfm_df$recency_score),
  as.character(rfm_df$frequency_score)
)

head(rfm_df,5)
# Function to map RFM_SCORE to segments
seg_map <- c(
  'Hibernating' = '^[1-2][1-2]$',
  'At Risk' = '^[1-2][3-4]$',
  "Can't Lose" = '^[1-2]5$',
  'About to Sleep' = '^3[1-2]$',
  'Need Attention' = '^33$',
  'Loyal Customers' = '^[3-4][4-5]$',
  'Promising' = '^41$',
  'New Customers' = '^51$',
  'Potential Loyalists' = '^[4-5][2-3]$',
  'Champions' = '^5[4-5]$'
)

# Function to assign segments based on the RFM score
segment_assign <- function(score, seg_map) {
  for (segment_name in names(seg_map)) {
    pattern <- seg_map[[segment_name]]
    if (grepl(pattern, score)) {
      return(segment_name)
    }
  }
  return(NA) # Return NA if no pattern matches
}


rfm_df$Segment <- sapply(rfm_df$rfm_score, function(x) segment_assign(x, seg_map))

head(rfm_df)


# Group by Segment and calculate statistics
rfmStats <- rfm_df %>%
  group_by(Segment) %>%
  summarise(
    Recency_mean = mean(recency, na.rm = TRUE),
    Recency_median = median(recency, na.rm = TRUE),
    Recency_count = n(),
    Recency_std = sd(recency, na.rm = TRUE),
    Frequency_mean = mean(frequency, na.rm = TRUE),
    Frequency_median = median(frequency, na.rm = TRUE),
    Frequency_count = n(),
    Frequency_std = sd(frequency, na.rm = TRUE),
    Monetary_mean = mean(monetary, na.rm = TRUE),
    Monetary_median = median(monetary, na.rm = TRUE),
    Monetary_count = n(),
    Monetary_std = sd(monetary, na.rm = TRUE)
  )

head(rfmStats)

# Visualization
segment_summary <- rfm_df %>%
  group_by(Segment) %>%
  summarise(Customer_Count = n())

segment_summary

ggplot(segment_summary, aes(x=reorder(Segment, Customer_Count), y=Customer_Count)) +
  geom_col(fill = 'blue', color = 'black') +
  coord_flip()
  labs(title = 'Number of customers by segments',
       x = 'Segment',
       y= 'Number of customers') +
  theme_gray()
  
