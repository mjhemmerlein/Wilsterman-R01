library(dplyr)
library(zoo)
library(tidyverse)
library(lubridate)
library(readxl)

# Read in data
Tb <- read.csv("Raw_Data/2025_01_09.csv", header = TRUE)
Tb <- rbind(Tb, read.csv("Raw_Data/2025_01_16.csv", header = TRUE))
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
  mutate(processed_data = map(data, ~process_window(.)))

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
  scale_fill_manual(values = c("black", "orange")) +
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
  geom_line(aes(x = DT, y = Temp)) +
  scale_y_continuous(limits = c(25,40)) +
  geom_abline(slope = 0, intercept = 36.5, col = "red") 


unnested_results %>%
  ggplot(aes = )



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


avg_min_temp <- unnested_results %>%
  filter(keep_or_discard == "Keep") %>%
  group_by(Female, date = as.Date(TimeStamp)) %>%
  summarise(daily_min = min(Temp, na.rm = TRUE), .groups = "keep") %>%
  group_by(Female) %>%
  summarise(avg_min = mean(daily_min, na.rm = TRUE), .groups = "drop")


# Print the values
print(avg_min_temp)

# Raw plot with female-specific average minimum lines
ggplot(data = unnested_results[which(unnested_results$keep_or_discard=="Keep"),]) +
  geom_line(aes(x = TimeStamp, y = Temp)) +
  scale_y_continuous(limits = c(25,40)) +
  geom_hline(data = avg_min_temp, aes(yintercept = avg_min), 
             color = "blue", linetype = "solid") +
  facet_wrap(~ Female)




avg_min_temp_first_4_days <- unnested_results %>%
  filter(keep_or_discard == "Keep") %>%
  # Group by Female and date to calculate daily minimums
  group_by(Female, date = as.Date(TimeStamp)) %>%
  summarise(daily_min = min(Temp, na.rm = TRUE), .groups = "keep") %>%
  # Keep only the first 4 days for each Female
  group_by(Female) %>%
  slice_min(order_by = date, n = 4) %>%
  # Calculate the average daily minimum for those 4 days
  summarise(avg_min_first_4_days = mean(daily_min, na.rm = TRUE), .groups = "drop")

# Print the values
print(avg_min_temp_first_4_days)









## TRIAL TO APPLY TO NESTED DATA --------

Trial <- read.csv("Raw_Data/2025_01_09.csv", header = TRUE)
Trial <- Trial %>%
  filter(UID == "6E6FA27D") %>%
  rename("Temp" = "Temperature")


window_width <- 11
test <- 1:(length(Trial$Temp))
n_iterations <- length(Trial$Temp) - 4

keep_or_discard <- character(n_iterations)
sd_val_storage <- numeric(n_iterations)
median_val_storage <- numeric(n_iterations)


for (i in test) {
  # take the values in the window
  window_values <- Trial$Temp[i:(i + window_width - 1)]
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
  window_values <- Trial$Temp[i:(i + window_width - 1)]
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


keep_or_discard_sh <- head(keep_or_discard,-5)
#look at how many observations there are in the kep data frame... that'll be the second number then -4
Trial$keep <- keep_or_discard_sh

#QC - you don't need to run this as part of your script.
summary(as.factor(keep_or_discard)) ## number of keep/discards
hist(as.numeric(Trial$Temp[which(keep_or_discard=="Keep")]))
hist(as.numeric(Trial$Temp[which(keep_or_discard=="Discard")]))
hist(as.numeric(Trial$Temp))

##visual confirmation - THIS WILL MAKE THE PLOTS YOU WANT MEG!
library(ggplot2)
library(lubridate)
Trial$Temp <- as.numeric(Trial$Temp)

names <- unique(Trial$UID)
Trial$Temp <- as.numeric(Trial$Temp)
Trial$TimeStamp <- as.POSIXct(Trial$TimeStamp, format = "%Y/%m/%d %H:%M:%S")

# Extract year from DT
year <- format(Trial$TimeStamp, "%Y")
unique(year)


#RAW

ggplot(data = Trial) +
  geom_line(aes(x = TimeStamp, y = Temp)) +
  scale_y_continuous(limits = c(25,40)) +
  geom_abline(slope = 0, intercept = 36.5, col = "red") 

#CLEAN
ggplot(data = Trial[which(Trial$keep=="Keep"),]) +
  geom_line(aes(x = TimeStamp, y = Temp)) +
  scale_y_continuous(limits = c(25,40)) +
  geom_abline(slope = 0, intercept = 36.5, col = "red")




##SAVE CLEANED DATA
clean_data <- h[which(h$keep=="Keep"),]
PTBlist$Ptcode[6]
write.csv(clean_data, "167F8B96_PTTB6.csv")












