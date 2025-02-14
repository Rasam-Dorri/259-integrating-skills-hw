# 259 Homework - integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

read_weather <- function(station) {
  # Create file path assuming files are named like "KCLT.csv", etc.
  filepath <- file.path("us-weather-history", paste0(station, ".csv"))
  
  # Read the CSV and mutate the date and station columns
  read_csv(filepath) %>% 
    mutate(date = ymd(date),
           station = station)
}

# Check the function with one station (for example, "KCLT")
glimpse(read_weather("KCLT"))

# ========================================================
# QUESTION 2:
# Use map_dfr() and the read_weather() function to read in all 10 stations,
# automatically binding the resulting tibbles into one.
# ========================================================

ds <- map_dfr(stations, read_weather)

# ========================================================
# QUESTION 3:
# Create a new factor variable "city" based on the station variable,
# with station codes as levels and city names as labels.
# Then, check (using fct_count) that each city has 365 days of data.
# ========================================================

ds <- ds %>% 
  mutate(city = factor(station, levels = stations, labels = cities))

# Check the counts per city
fct_count(ds$city)

# ========================================================
# QUESTION 4:
# Convert all temperature values (in Fahrenheit) to Celsius.
# First, write a function to convert F to C (rounded to a tenth),
# then use mutate(across()) to apply it to all temperature columns.
# ========================================================

# Conversion function: (F - 32) * 5/9, rounded to one decimal place.
f_to_c <- function(temp_f) {
  round((temp_f - 32) * 5/9, 1)
}

# Identify the temperature columns to convert
temp_cols <- c("actual_mean_temp", "actual_min_temp", "actual_max_temp", 
               "average_min_temp", "average_max_temp", "record_min_temp", "record_max_temp")

ds <- ds %>% 
  mutate(across(all_of(temp_cols), f_to_c))

# ========================================================
# QUESTION 5:
# Write a function that counts the number of extreme temperature days,
# where either the actual minimum equals the record minimum or
# the actual maximum equals the record maximum.
# Group by city and sort descending.
# ========================================================

count_extreme_days <- function(df) {
  df %>%
    # Create a logical flag for extreme days
    mutate(extreme = (actual_min_temp == record_min_temp) | 
                     (actual_max_temp == record_max_temp)) %>%
    group_by(city) %>%
    summarise(extreme_days = sum(extreme, na.rm = TRUE)) %>%
    arrange(desc(extreme_days))
}

# Run the function on the dataset (without modifying ds)
extreme_summary <- count_extreme_days(ds)
print(extreme_summary)

# ========================================================
# QUESTION 6:
# Extract the month from the date, making it a factor.
# Then, split the tibble by month into a list of tibbles.
# ========================================================

# Here we extract month as an abbreviated label (e.g., "Jan", "Feb", etc.)
ds <- ds %>% 
  mutate(month = factor(month(date, label = TRUE, abbr = TRUE)))

# Split the dataset by month into a list (each list element is a tibble for that month)
ds_by_month <- split(ds, ds$month)
# (You can inspect the list with str(ds_by_month) or names(ds_by_month))

# ========================================================
# QUESTION 7:
# For each month, compute and print the correlations:
#   - between actual_precipitation and average_precipitation,
#   - between actual_min_temp and average_min_temp,
#   - between actual_max_temp and average_max_temp.
# Use a for loop.
# ========================================================

# Ensure that missing values (if any) are handled (use complete.obs)
for (m in levels(ds$month)) {
  data_month <- filter(ds, month == m)
  
  # Calculate correlations using complete observations
  cor_precip <- cor(data_month$actual_precipitation, data_month$average_precipitation, use = "complete.obs")
  cor_min    <- cor(data_month$actual_min_temp, data_month$average_min_temp, use = "complete.obs")
  cor_max    <- cor(data_month$actual_max_temp, data_month$average_max_temp, use = "complete.obs")
  
  # Print the month and correlations
  cat("Month:", m, "\n")
  cat("  Correlation (Actual vs Average Precipitation):", round(cor_precip, 2), "\n")
  cat("  Correlation (Actual vs Average Min Temp):", round(cor_min, 2), "\n")
  cat("  Correlation (Actual vs Average Max Temp):", round(cor_max, 2), "\n\n")
}

# ========================================================
# QUESTION 8:
# Use the DataExplorer package to create:
#   - Boxplots of all numeric variables grouped by city,
#   - Boxplots grouped by month,
#   - A correlation plot of the continuous (numeric) variables.
# ========================================================

# Boxplots grouped by city
plot_boxplot(ds, by = "city")

# Boxplots grouped by month
plot_boxplot(ds, by = "month")

# Correlation plot (select only numeric columns)
numeric_vars <- ds %>% select(where(is.numeric))
plot_correlation(numeric_vars)

# ========================================================
# QUESTION 9:
# Create a scatterplot of actual_mean_temp (y) by date (x).
# Use facet_wrap to create separate plots for each city (3 columns),
# and color the points by month.
# ========================================================

ggplot(ds, aes(x = date, y = actual_mean_temp, color = month)) +
  geom_point() +
  facet_wrap(~ city, ncol = 3) +
  labs(x = "Date", y = "Actual Mean Temperature (°C)", title = "Daily Mean Temperature by City") +
  theme_minimal()

# ========================================================
# QUESTION 10:
# Write a function that, for a given month (abbreviated, e.g., "Jan"),
# creates a scatter and line plot of actual_mean_temp by date.
# The plot should have a title with the month name and be saved as "eda/month.png".
# Then, call the function for each month.
# ========================================================

# Ensure the "eda" folder exists
if (!dir.exists("eda")) dir.create("eda")

plot_month_temp <- function(data, month_abbr) {
  # Filter data for the given month abbreviation
  data_month <- data %>% 
    filter(month == month_abbr)
  
  # Create the plot
  p <- ggplot(data_month, aes(x = date, y = actual_mean_temp, color = city)) +
    geom_point() +
    geom_line(aes(group = city)) +
    labs(x = "Date", y = "Actual Mean Temperature (°C)",
         title = paste("Temperature Trends for", month_abbr)) +
    theme_minimal()
  
  # Save the plot to the eda folder with the filename "month_abbr.png"
  ggsave(filename = file.path("eda", paste0(month_abbr, ".png")),
         plot = p, width = 10, height = 6)
  
  return(p)
}

# Call the function for each month using map (from purrr)
unique_months <- levels(ds$month)
plots_list <- map(unique_months, ~ plot_month_temp(ds, .x))
