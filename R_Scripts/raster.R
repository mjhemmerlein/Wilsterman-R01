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



# Raster (nested) --------

G3KK1 <- unnested_results %>%
  filter(Female == "K-120824-03") %>%
  filter(keep_or_discard == "Keep")

G3KK1$TimeStamp <- as.POSIXct(G3KK1$TimeStamp, tz = "MST", format = "%Y/%m/%d %H:%M:%S")
G3KK1$startDate  <- as.POSIXct(G3KK1$TimeStamp, tz = "MST", format = "%Y/%m/%d %H:%M:%S")
G3KK1$startDate <- min(G3KK1$TimeStamp, na.rm = TRUE)


##bin data
G3KK1$pTRC <- interval(G3KK1$startDate, G3KK1$TimeStamp)
G3KK1$HoursFromStart <- time_length(G3KK1$pTRC, "hour")
G3KK1$HoursFromStartRounded <- floor(G3KK1$HoursFromStart)
G3KK1$MinsFromStart <- time_length(G3KK1$pTRC, "minute")
G3KK1$MinsFromStartRounded <- floor(G3KK1$MinsFromStart)

G3KK1$MinuteofHour <- format(G3KK1$TimeStamp, "%M")
G3KK1$MinCorr <- NA
G3KK1$MinCorr[which(G3KK1$MinuteofHour<20)] <- 0
G3KK1$MinCorr[which(G3KK1$MinuteofHour>39)] <- 40
G3KK1$MinCorr[which(is.na(G3KK1$MinCorr))] <- 20

G3KK1$uniqueG3KK1 <- paste(G3KK1$HoursFromStartRounded, G3KK1$MinCorr)
length(unique(G3KK1$uniqueG3KK1))

binned1 <- G3KK1 %>% 
  group_by(Female,HoursFromStartRounded,MinCorr) %>%
  summarise(Temp = min(Temp),
            StartDate = min(startDate),
            minsFromStartRounded = min(MinsFromStartRounded),
            TimeStamp = min(TimeStamp))

min(binned1$Temp)
max(binned1$Temp)

##convert to minutes in the day
binned1$altMinsFrom <- (binned1$HoursFromStartRounded*60)+(binned1$MinCorr)

binned1$altSecsFrom <- binned1$altMinsFrom*60
binned1$Time <- as.POSIXct(binned1$altSecsFrom, origin="2025-01-01", tz = "MST")
binned1$TimeCorr <- binned1$Time + hours(6)

binned1$xaxis <- as.Date(binned1$TimeCorr, format='%d%m%y', tz = "MST")

binned1$minuteOfDay <- (as.numeric(format(binned1$TimeCorr, "%H"))*60)+ as.numeric((format(binned1$TimeCorr, "%M")))

ggplot(data = binned1) + 
  geom_raster(aes(x=as.Date(TimeCorr, format='%d%m%y', tz = "MST"), y=minuteOfDay, fill=Temp)) + 
  scale_fill_gradientn(colours = (cet_pal(6, name = "inferno")),
                       na.value = "transparent",
                       breaks=c(35,37,39),labels=c(35,37,39),
                       limits=c(33.9,40))



# RASTER -----

G3KK1 <- unnested_results %>%
  filter(Female == "G3-KK1") %>%
  filter(keep_or_discard == "Keep")

G3KK1$TimeStamp <- as.POSIXct(G3KK1$TimeStamp, tz = "MST", format = "%Y/%m/%d %H:%M:%S")
G3KK1$startDate  <- as.POSIXct(G3KK1$TimeStamp, tz = "MST", format = "%Y/%m/%d %H:%M:%S")
G3KK1$startDate <- min(G3KK1$TimeStamp, na.rm = TRUE)


##bin data
G3KK1$pTRC <- interval(G3KK1$startDate, G3KK1$TimeStamp)
G3KK1$HoursFromStart <- time_length(G3KK1$pTRC, "hour")
G3KK1$HoursFromStartRounded <- floor(G3KK1$HoursFromStart)
G3KK1$MinsFromStart <- time_length(G3KK1$pTRC, "minute")
G3KK1$MinsFromStartRounded <- floor(G3KK1$MinsFromStart)

G3KK1$MinuteofHour <- format(G3KK1$TimeStamp, "%M")
G3KK1$MinCorr <- NA
G3KK1$MinCorr[which(G3KK1$MinuteofHour<20)] <- 0
G3KK1$MinCorr[which(G3KK1$MinuteofHour>39)] <- 40
G3KK1$MinCorr[which(is.na(G3KK1$MinCorr))] <- 20

G3KK1$uniqueG3KK1 <- paste(G3KK1$HoursFromStartRounded, G3KK1$MinCorr)
length(unique(G3KK1$uniqueG3KK1))

binned1 <- G3KK1 %>% 
  group_by(Female,HoursFromStartRounded,MinCorr) %>%
  summarise(Temp = min(Temp),
            StartDate = min(startDate),
            minsFromStartRounded = min(MinsFromStartRounded),
            TimeStamp = min(TimeStamp))

min(binned1$Temp)
max(binned1$Temp)

##convert to minutes in the day
binned1$altMinsFrom <- (binned1$HoursFromStartRounded*60)+(binned1$MinCorr)

binned1$altSecsFrom <- binned1$altMinsFrom*60
binned1$Time <- as.POSIXct(binned1$altSecsFrom, origin="2025-01-01", tz = "MST")
binned1$TimeCorr <- binned1$Time + hours(6)

binned1$xaxis <- as.Date(binned1$TimeCorr, format='%d%m%y', tz = "MST")

binned1$minuteOfDay <- (as.numeric(format(binned1$TimeCorr, "%H"))*60)+ as.numeric((format(binned1$TimeCorr, "%M")))

ggplot(data = binned1) + 
  geom_raster(aes(x=as.Date(TimeCorr, format='%d%m%y', tz = "MST"), y=minuteOfDay, fill=Temp)) + 
  scale_fill_gradientn(colours = (cet_pal(6, name = "inferno")),
                       na.value = "transparent",
                       breaks=c(35,37,39),labels=c(35,37,39),
                       limits=c(33.9,40))
