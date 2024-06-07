
##############################Group No: 13 Assignment 5#########################
#Name: Praful Patil, Sanjana Belde, Sreejanya Onteddu, Tulasi Smrithi Yaralagadda
###############NetID: PVP220001, SXB220133, SXO220000, TXY220003################

rm(list = ls())
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
####################################PART 1######################################

# 1).
# A).
# a)
# Read in the CSV file
data <- read.csv("cell2cell.csv")

# Get the total number of customers (rows)
total_customers <- nrow(data)
total_customers

# b)
# Count how many are in the calibration set
calibration_count <- sum(data$calibrat == 1)

# Count how many are in the validation set
validation_count <- sum(data$calibrat == 0)

calibration_count 
validation_count

# c) Churn rate in calibration set
calibration_churn_rate <- mean(data$churn[data$calibrat == 1] == 1)

# d) Churn rate in validation set
validation_churn_rate <- mean(data$churn[data$calibrat == 0] == 1)

# Results
cat("Total number of customers:", total_customers, "\n")
cat("Customers in calibration set:", calibration_count, "\n")
cat("Customers in validation set:", validation_count, "\n")
cat("Churn rate in calibration set:", format(calibration_churn_rate, digits=6), "\n")
cat("Churn rate in validation set:", format(validation_churn_rate, digits=6), "\n")

# 1).
# B). Data Cleaning
# a).
 


#a)
# Selecting relevant columns
Data <- data %>%
  select(churndep, revenue:retcall, calibrat)

# b).
# Split the data into training and validation sets
training_set <- Data %>%
  filter(calibrat == 1) %>%
  select(-calibrat) 

validation_set <- Data %>%
  filter(calibrat == 0) %>%
  select(-calibrat) 


# C) Running logistic model for training dataset
logistic_model <- glm(churndep ~ ., data = training_set, family = binomial)

# Summarizing the model to check coefficients and p-values
summary_logistic <- summary(logistic_model)

# P value and Odds ratio of Top and Bottom two variables
sorted_odds_desc <- sort(exp(coef(logistic_model)), decreasing = TRUE)
top_bottom_odds <- c(head(sorted_odds_desc, 2), tail(sorted_odds_desc, 2))
top_bottom_pvalues <- summary_logistic$coefficients[c(names(head(sorted_odds_desc, 2)), names(tail(sorted_odds_desc, 2))), "Pr(>|z|)"]
top_bottom_odds
top_bottom_pvalues

# D)
# Use the model to predict attrition probabilities
validation_set$attrition_probability <- predict(logistic_model, newdata = validation_set, type = "response")

# Arrange the data in descending order of attrition probabilities
validation_set <- validation_set %>%
  arrange(desc(attrition_probability))

# Display the top 5 attrition probabilities
head(validation_set$attrition_probability)

# 2)
# A)
# a) 
OR <- sort(exp(coef(logistic_model)), decreasing = TRUE)

# b)
pvales <- coef(summary(logistic_model))[,'Pr(>|z|)']

# c)
df1 <- data.frame(OR, pvales)

# d) The data looks as expected

# 2)
# b) -> a), b)
# Function to calculate standard deviation if there are non-missing values
calculate_sd <- function(x) {
  # Check if there are any non-missing values
  if (sum(!is.na(x)) > 0) {
    # Calculate and return standard deviation, excluding NA values
    return(sd(x, na.rm = TRUE))
  } else {
    # Return NA if all values are missing
    return(NA)
  }
}

# Apply the custom function (#calculate_sd) to each column in the dataframe to calibrate data i.e. Validation Set
standard_deviations <- sapply(validation_set, calculate_sd)

# b) c)
# The result is a named vector of standard deviations
df2 <- data.frame(standard_deviations)

# c) a)
# Add a column with row names to df1
df1$VarName <- row.names(df1)

# Add a column with row names to df2
df2$VarName <- row.names(df2)

# c) b)
# Merge the data frames based on VarName
# all = FALSE ensures that only the matched rows are kept
df_merged <- merge(df1, df2, by = "VarName", all = FALSE)

# d)
# Round numeric columns to 5 decimal points
df_merged[sapply(df_merged, is.numeric)] <- round(df_merged[sapply(df_merged, is.numeric)], 5)


df_sorted <- df_merged %>% 
      filter(pvales < 0.05) %>% 
      arrange(-OR)

# e)
# Export to CSV
write.csv(df_sorted, "filtered_data.csv", row.names = FALSE)






















