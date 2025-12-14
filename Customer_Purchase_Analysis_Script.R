# Load the library
library(readr)
# Import CSV
data <- read.csv("C:/Users/Nabeel Shamsi/OneDrive/Desktop/R Project/dataset.csv")
# Load the skimr library for data summarization
library(skimr)
# Overview of the dataset
skim_without_charts(data)
# Load tidyverse for data manipulation and cleaning
library(tidyverse)

# R Script for Data Cleaning

# Convert empty strings to NA
data[data == ""] <- NA
# Check missing values per column
colSums(is.na(data))
# Remove rows with any NA values (complete cases)
data_clean <- na.omit(data)
# Check the number of rows after cleaning
nrow(data_clean)
# view a summary 
library(skimr)
skim_without_charts(data_clean)
# Clean column names
library(janitor)
data_clean <- clean_names(data_clean)
# Ensure column names are clean
library(tidyverse)
library(lubridate)
data_clean <- data_clean %>%
  mutate(purchase_date = as.Date(purchase_date))
# Check date is valid
min_valid_date <- as.Date("2019-01-01")
max_valid_date <- Sys.Date()
# Filter for illogical dates outside
illogical_dates <- data_clean %>%
  filter(purchase_date < min_valid_date | purchase_date > max_valid_date)
# Print results
print(illogical_dates)
# Check the maximum and minimum age in the dataset
max(data_clean$age)
min(data_clean$age)
# Standardize gender column
library(dplyr)
library(stringr)
data_clean <- data_clean %>%
  mutate(
    gender = str_to_lower(gender),
    gender = str_to_title(gender),
    gender = case_when(
      gender %in% c("Male", "Female", "Other") ~ gender,
      TRUE ~ NA_character_))
# Standardize gender country
data_clean <- data_clean %>%
  mutate(
    country = str_to_lower(country),   
    country = str_to_title(country))
# Standardize gender product category
data_clean <- data_clean %>%
  mutate(
    product_category = str_to_lower(product_category),
    product_category = str_to_title(product_category))
# Load library
library(ggplot2)
# Checking data is clean by helping visualization
# Gender
ggplot(data_clean, aes(x = gender)) + geom_bar()
# Country
ggplot(data_clean, aes(x = country)) + geom_bar()
# Product Category
ggplot(data_clean, aes(x = country)) + geom_bar()
# Summary
summary(data_clean)
# Total revenue per category
total_revenue_overall <- sum(data_clean$purchase_amount)
product_summary <- data_clean %>%
  group_by(product_category) %>%
  summarise(
    total_revenue = sum(purchase_amount),   
    sales_volume = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = total_revenue / total_revenue_overall * 100
  ) %>%
  arrange(desc(total_revenue))
product_summary

# View the summary
# Calculate overall total revenue
total_revenue_overall <- sum(data_clean$purchase_amount)

# Total revenue per country with percentage
country_summary <- data_clean %>%
  group_by(country) %>%
  summarise(
    total_revenue = sum(purchase_amount),          
    avg_purchase_amount = mean(purchase_amount),  
    sales_volume = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = paste0(round(total_revenue / total_revenue_overall * 100, 1), "%")
  ) %>%
  arrange(desc(total_revenue))

# View results
country_summary

# Average spend per gender
# Calculate overall total revenue
total_revenue_overall <- sum(data_clean$purchase_amount)

# Average spend per gender with percentage
gender_summary <- data_clean %>%
  group_by(gender) %>%
  summarise(
    avg_purchase_amount = mean(purchase_amount),  
    total_revenue = sum(purchase_amount),        
    sales_volume = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = total_revenue / total_revenue_overall * 100
  )
# View results
gender_summary

# Plot total revenue per product category
library(scales)
ggplot(category_revenue, aes(x = reorder(product_category, total_revenue), 
                             y = total_revenue, fill = product_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Total Revenue per Product Category", x = "Product Category", y = "Total Revenue") +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(legend.position = "none")
category_volume <- data_clean %>%
  group_by(product_category) %>%
  summarise(transaction_count = n()) %>%
  arrange(desc(transaction_count))

# Plot transaction volume per category
category_volume <- data_clean %>%
  group_by(product_category) %>%
  summarise(transaction_count = n()) %>%
  arrange(desc(transaction_count))
ggplot(category_volume, aes(x = reorder(product_category, transaction_count), 
                            y = transaction_count, fill = product_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Horizontal bars
  labs(
    title = "Transaction Volume per Product Category",
    x = "Product Category",
    y = "Number of Transactions"
  ) +
  theme_classic() +      
  theme(legend.position = "none")
country_summary <- data_clean %>%
  group_by(country) %>%
  summarise(
    total_revenue = sum(purchase_amount),
    avg_purchase_amount = mean(purchase_amount),
    transaction_count = n()
  ) %>%
  arrange(desc(total_revenue))

# Total Revenue per Country
country_summary <- data_clean %>%
  group_by(country) %>%
  summarise(
    total_revenue = sum(purchase_amount),
    avg_purchase_amount = mean(purchase_amount),
    transaction_count = n()
  ) %>%
  arrange(desc(total_revenue))
ggplot(country_summary, aes(x = reorder(country, total_revenue), y = total_revenue, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = comma) +  
  labs(
    title = "Total Revenue per Country",
    x = "Country",
    y = "Total Revenue"
  ) +
  theme_classic() +
  theme(legend.position = "none")
#Average Purchase Amount by Age Group
age_summary$age_group <- factor(age_summary$age_group, 
                                levels = c("18-24", "25-34", "35-44", "45-54", "55+"))
ggplot(age_summary, aes(x = age_group, y = avg_purchase_amount, fill = age_group)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Horizontal bars
  labs(
    title = "Average Purchase Amount by Age Group",
    x = "Age Group",
    y = "Average Purchase Amount"
  ) +
  theme_classic() +
  theme(legend.position = "none")

#Product Mix by Country
country_product <- data_clean %>%
  group_by(country, product_category) %>%
  summarise(total_revenue = sum(purchase_amount)) %>%
  ungroup()
ggplot(country_product, aes(x = country, y = total_revenue, fill = product_category)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set2") +        
  labs(
    title = "Product Mix by Country",
    x = "Country",
    y = "Total Revenue",
    fill = "Product Category"
  ) +
  theme_classic()

# Sales Trend Over Time
data_clean$purchase_date <- as.Date(data_clean$purchase_date)
monthly_revenue <- data_clean %>%
  mutate(year_month = floor_date(purchase_date, "month")) %>% 
  group_by(year_month) %>%
  summarise(total_revenue = sum(purchase_amount)) %>%
  arrange(year_month)
ggplot(monthly_revenue, aes(x = year_month, y = total_revenue)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred") +
  scale_y_continuous(labels = comma) +  
  labs(
    title = "Sales Trend Over Time",
    x = "Month",
    y = "Total Revenue"
  ) +
  theme_classic()


# Plot monthly seasonality
monthly_season <- data_clean %>%
  mutate(month = month(purchase_date, label = TRUE, abbr = FALSE)) %>%  # Month name
  group_by(month) %>%
  summarise(total_revenue = sum(purchase_amount)) %>%
  mutate(month = factor(month, levels = month.name)) %>%  # Ensure calendar order
  arrange(month)
ggplot(monthly_season, aes(x = month, y = total_revenue, group = 1)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "red", size = 2) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Monthly Sales Pattern (Seasonality)",
    x = "Month",
    y = "Total Revenue"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total Revenue by Day of the Week
day_of_week <- data_clean %>%
  mutate(day = wday(purchase_date, label = TRUE, abbr = FALSE)) %>% 
  group_by(day) %>%
  summarise(total_revenue = sum(purchase_amount),
            transaction_count = n()) %>%
  arrange(match(day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

ggplot(day_of_week, aes(x = day, y = total_revenue, fill = day)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Revenue by Day of the Week",
    x = "Day of Week",
    y = "Total Revenue"
  ) +
  theme_classic() +
  theme(legend.position = "none")
