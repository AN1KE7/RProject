library(tidyverse)
library(ggplot2)
library(scales)
library(moments)

budget_data <- read.csv("Cleaned_Union_Budget.csv")

# 1. Data Description
# Structure of the data
str(budget_data)

# Summary statistics for numerical columns
numerical_summary <- summary(budget_data[,c(
  "actuals_2021_2022_revenue",
  "actuals_2021_2022_capital", 
  "actuals_2021_2022_total",
  "budget_estimates_2022_2023_revenue",
  "budget_estimates_2022_2023_capital",
  "budget_estimates_2022_2023_total",
  "revised_estimates2022_2023_revenue",
  "revised_estimates2022_2023_capital", 
  "revised_estimates2022_2023_total",
  "budget_estimates2023_2024_revenue",
  "budget_estimates2023_2024_capital",
  "budget_estimates2023_2024_total"
)])
print(numerical_summary)

# Calculate mean 
mean_stats <- data.frame(
  Metric = c("Revenue 2021-22", "Capital 2021-22", "Total 2021-22"),
  Mean = c(
    mean(budget_data$actuals_2021_2022_revenue, na.rm = TRUE),
    mean(budget_data$actuals_2021_2022_capital, na.rm = TRUE),
    mean(budget_data$actuals_2021_2022_total, na.rm = TRUE)
  )
)
print("Mean Statistics:")
print(mean_stats)

# Calculate median
median_stats <- data.frame(
  Metric = c("Revenue 2021-22", "Capital 2021-22", "Total 2021-22"),
  Median = c(
    median(budget_data$actuals_2021_2022_revenue, na.rm = TRUE),
    median(budget_data$actuals_2021_2022_capital, na.rm = TRUE),
    median(budget_data$actuals_2021_2022_total, na.rm = TRUE)
  )
)
print("\nMedian Statistics:")
print(median_stats)

# Visualization comparing mean and median
comparison_stats <- data.frame(
  Metric = c("Revenue 2021-22", "Capital 2021-22", "Total 2021-22"),
  Mean = mean_stats$Mean,
  Median = median_stats$Median
)

# Reshape the data for plotting
comparison_long <- tidyr::pivot_longer(comparison_stats, 
                                     cols = c("Mean", "Median"),
                                     names_to = "Statistic",
                                     values_to = "Value")

# Create a grouped bar plot
ggplot(comparison_long, aes(x = Metric, y = Value, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Comparison of Mean and Median Values",
       x = "Metric",
       y = "Value (in Crores)") +
  scale_fill_manual(values = c("Mean" = "steelblue", "Median" = "darkred")) 

# Calculate mode function
get_modes <- function(v) {
  uniqv <- unique(v)  # Get unique values
  freq <- tabulate(match(v, uniqv))  # Tabulate the frequency of each value
  uniqv[freq == max(freq)]  # Return all values with the maximum frequency
}
# Calculate all modes for the "actuals_2021_2022_revenue" column
modes_revenue <- get_modes(budget_data$actuals_2021_2022_revenue)

# Print the result
print(paste("Modes of Actuals 2021-2022 Revenue:", paste(modes_revenue, collapse = ", ")))



# Calculate IQR for revenue columns
revenue_iqr <- IQR(budget_data$actuals_2021_2022_revenue, na.rm = TRUE)
print(paste("IQR for Actuals 2021-2022 Revenue:", revenue_iqr))

# 2. Visualizations

# Bar Chart - Top 10 Departments by 2021-22 Revenue
top_10_revenue <- budget_data %>%
  group_by(ministry.department) %>%
  summarise(total_revenue = sum(actuals_2021_2022_revenue)) %>%
  arrange(desc(total_revenue)) %>%
  head(10)

ggplot(top_10_revenue, aes(x = reorder(ministry.department, total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Departments by Revenue (2021-22)",
       x = "Department",
       y = "Revenue (in Crores)")

# Pie Chart - Budget Distribution by Category
category_total <- budget_data %>%
  group_by(category) %>%
  summarise(total_budget = sum(actuals_2021_2022_total)) %>%
  arrange(desc(total_budget))

ggplot(category_total, aes(x = "", y = total_budget, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Budget Distribution by Category (2021-22)")

# Histogram - Distribution of Revenue
ggplot(budget_data, aes(x = actuals_2021_2022_revenue)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Revenue (2021-22)",
       x = "Revenue",
       y = "Frequency")

# Scatter Plot - Revenue vs Capital Expenditure
ggplot(budget_data, aes(x = actuals_2021_2022_revenue, y = actuals_2021_2022_capital)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Revenue vs Capital Expenditure (2021-22)",
       x = "Revenue",
       y = "Capital Expenditure")

# Box Plot - Revenue Distribution by Category
ggplot(budget_data, aes(x = category, y = actuals_2021_2022_revenue)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Revenue Distribution by Category (2021-22)",
       x = "Category",
       y = "Revenue") 

