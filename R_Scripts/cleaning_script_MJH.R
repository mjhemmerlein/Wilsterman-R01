library(dplyr)
library(zoo)
library(tidyverse)
library(lubridate)
library(readxl)
library(cetcolor)

# Read in data
Tb <- read.csv("Raw_Data/2025_01_09.csv", header = TRUE)
Tb <- rbind(Tb, read.csv("Raw_Data/2025_01_16.csv", header = TRUE)) 

## CONTINUE TO ADD RAW DATA HERE
## CONTINUE TO ADD RAW DATA HERE
## CONTINUE TO ADD RAW DATA HERE

Tb <- Tb %>%
  rename("Temp" = "Temperature")

# Read in pittag IDs
PTBlist <- read_xlsx("Raw_Data/Pittag_Metadata.xlsx")
PTBlist <- PTBlist %>%
  rename("UID" = "PIT_ID") %>%
  select(-c(Initials, Comment))

# Nest the data
data <- Tb %>%
  filter(UID != "00000000") %>%
  left_join(PTBlist, by = "UID") %>%
  group_by(UID) %>%
  nest(data = -c(colnames(PTBlist)))

# Function to process the data
process_window <- function(data) {
window_width <- 11
test <- 1:(length(data$Temp))
n_iterations <- length(data$Temp) - 4

keep_or_discard <- character(n_iterations)
sd_val_storage <- numeric(n_iterations)
median_val_storage <- numeric(n_iterations)


for (i in test) {
  # take the values in the window
  window_values <- data$Temp[i:(i + window_width - 1)]
  window_values <- as.numeric(window_values)
  
  # calc median and standard deviation for the window, excluding missing values
  median_val <- median(window_values, na.rm = TRUE)
  median_val <- as.numeric(median_val)
  sd_val <- sd(window_values, na.rm = TRUE)
  
  sd_val_storage[i] <- sd_val
  median_val_storage[i] <- median_val
}


for (i in test) {
  # take the values in the window
  window_values <- data$Temp[i:(i + window_width - 1)]
  window_values <- as.numeric(window_values)
  
  # calc median and standard deviation for the window, excluding missing values
  median_val <- median(window_values, na.rm = TRUE)
  median_val <- as.numeric(median_val)
  
  low_bound <- median_val - (1.5*median(sd_val_storage[-which(is.na(sd_val_storage))]))
  high_bound <- median_val + (1.5*median(sd_val_storage[-which(is.na(sd_val_storage))]))
  
  test_value <- window_values[6]
  if (!is.na(test_value) && !is.na(median_val)) {
    if (test_value > low_bound && test_value < high_bound) {
      keep_or_discard[i+5] <- "Keep"
    } else {
      keep_or_discard[i+5] <- "Discard"
    }
  } else {
    keep_or_discard[i+5] <- "discard"  # Discard if any missing value
  }
}


# Return results as a dataframe
result <- data.frame(Temp = data$Temp[6:(length(data$Temp) - 5)],
                     keep_or_discard = keep_or_discard[6:(length(data$Temp) - 5)],
                     TimeStamp = data$TimeStamp[6:(length(data$TimeStamp) - 5)])

return(result)
}


# Use map to apply the process_window function to each UID group
nested_results <- data %>%
  mutate(
    processed_data = map(data, ~process_window(.)),
    keep_data = map(processed_data, ~.x %>% filter(keep_or_discard == "Keep")))

# Unnest the processed data
unnested_results <- nested_results %>%
  select(UID, Female, Plate, processed_data) %>%
  unnest(processed_data)


# Quality Control Visualization
unnested_results %>%
  group_by(Female, keep_or_discard) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = keep_or_discard, values_from = count, values_fill = 0)

unnested_results %>%
  ggplot(aes(as.numeric(Temp), fill = keep_or_discard)) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.5, position = "identity") +
  facet_wrap(~ Female) +
  scale_fill_manual(values = c("black", "lightgray")) +
  labs(x = "Temperature", y = "Count", fill = "Keep or Discard") +
  theme_minimal() +
  xlim(35,41)


# Raw and filtered plots
unnested_results$Temp <- as.numeric(unnested_results$Temp)
unnested_results$TimeStamp <- as.POSIXct(unnested_results$TimeStamp, format = "%Y/%m/%d %H:%M:%S")

# Extract year
year <- format(unnested_results$TimeStamp, "%Y")
unique(year)

#RAW
ggplot(data = unnested_results) +
  geom_line(aes(x = TimeStamp, y = Temp)) +
  scale_y_continuous(limits = c(25,40)) +
  geom_abline(slope = 0, intercept = 36.5, col = "red") +
  facet_wrap(~ Female)

#CLEAN
ggplot(data = unnested_results[which(unnested_results$keep_or_discard=="Keep"),]) +
  geom_line(aes(x = TimeStamp, y = Temp)) +
  scale_y_continuous(limits = c(25,40)) +
  geom_abline(slope = 0, intercept = 36.5, col = "red") +
  facet_wrap(~ Female)


# AVG MIN BODY TEMP LINE

# Start date
start_dates <- unnested_results %>%
  group_by(Female) %>%
  summarise(start_date = min(as.Date(TimeStamp))) %>%
  mutate(day_7 = start_date + 7) 

first_7_days <- unnested_results %>%
  filter(keep_or_discard == "Keep") %>%
  group_by(Female, date = as.Date(TimeStamp)) %>%
  summarise(daily_min = min(Temp, na.rm = TRUE), .groups = "keep") %>%
  group_by(Female) %>%
  slice_min(order_by = date, n = 7) %>%
  summarise(avg_min = mean(daily_min, na.rm = TRUE), .groups = "drop")

# Print the values
print(first_7_days)


days_2_7 <- unnested_results %>%
  filter(keep_or_discard == "Keep") %>%
  group_by(Female, date = as.Date(TimeStamp)) %>%
  summarise(daily_min = min(Temp, na.rm = TRUE), .groups = "keep") %>%
  group_by(Female) %>%
  slice_min(order_by = date, n = 7) %>%
  slice(2:7) %>%  # This line selects days 2-7
  summarise(avg_min = mean(daily_min, na.rm = TRUE), .groups = "drop")

# Print the values
print(days_2_7)


# Plot
ggplot(data = unnested_results[which(unnested_results$keep_or_discard=="Keep"),]) +
  geom_line(aes(x = TimeStamp, y = Temp)) +
  scale_y_continuous(limits = c(25,40)) +
  geom_hline(data = days_2_7, aes(yintercept = avg_min), 
             color = "blue", linetype = "solid") +
  geom_hline(data = first_7_days, aes(yintercept = avg_min), 
             color = "lightblue", linetype = "solid") +  
  geom_vline(data = start_dates, aes(xintercept = as.POSIXct(day_7)), 
             color = "red", linetype = "solid") +
  facet_wrap(~ Female)



ggplot(data = unnested_results[which(unnested_results$keep_or_discard=="Keep"),]) +
  geom_line(aes(x = TimeStamp, y = Temp)) +
  scale_y_continuous(limits = c(25,40), 
                     breaks = seq(25, 40, 1),     # Major grid lines at whole degrees
                     minor_breaks = seq(25, 40, 0.5)) +  # Minor grid lines at half degrees
  theme_bw() +  # Makes the grid visible
  geom_hline(data = days_2_7, aes(yintercept = avg_min), 
             color = "blue", linetype = "solid") +
  geom_hline(data = first_7_days, aes(yintercept = avg_min), 
             color = "lightblue", linetype = "solid") +  
  geom_vline(data = start_dates, aes(xintercept = as.POSIXct(day_7)), 
             color = "red", linetype = "solid") +
  facet_wrap(~ Female)





















