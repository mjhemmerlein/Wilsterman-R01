# setwd("/Users/kwilst/Documents/Research/PlacentalAtlas/") 

library(dplyr)
library(zoo)

Tb <- read.csv("Raw_Data/tempdata_6.28.23.csv", header= TRUE)

Tb$Tag.ID <- as.factor(Tb$Tag.ID)
Tb$DT <- paste(Tb$Date, Tb$Time)
 
PTBlist <- read.csv("Raw_Data/PTBlist.csv", header= TRUE)
##the above line is where I pulled TagIDs from ... The system sometimes reads nonsense,
##so you really do need a "correct" reference file to work off of
##you can see the "non\sense" lines in the file by running unique(Tb$Tag.ID)

##the best option is to keep an up-to-date file like the PTBList that has Female ID, plate #, and
##Tag.ID in it that can be referenced by the file. You will record Tag.ID when you PIT Tag individuals. 

##subset full datasheet for SINGLE tag ID
h <- filter(Tb, Tag.ID == PTBlist$Ptcode[2])

window_width <- 11

test <- c(1:length(h$Temp))
n_iterations <- length(h$Temp) - 4  
# number of iterations. subtract 9 so that we have 10 values in each window

keep_or_discard <- character(n_iterations)
sd_val_storage <- numeric(n_iterations)
median_val_storage <- numeric(n_iterations)

# Loop through each window - NOT THAT THE SECOND LOOP TAKES A WHILE!
# There is probably a faster way to do this :P
for (i in test) {
  # take the values in the window
  window_values <- h$Temp[i:(i + window_width - 1)]
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
  window_values <- h$Temp[i:(i + window_width - 1)]
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
h$keep <- keep_or_discard_sh

#QC - you don't need to run this as part of your script.
summary(as.factor(keep_or_discard)) ## number of keep/discards
hist(as.numeric(h$Temp[which(keep_or_discard=="Keep")]))
hist(as.numeric(h$Temp[which(keep_or_discard=="Discard")]))
hist(as.numeric(h$Temp))

##visual confirmation - THIS WILL MAKE THE PLOTS YOU WANT MEG!
library(ggplot2)
library(lubridate)
h$Temp <- as.numeric(h$Temp)

names <- unique(h$Tag.ID)
h$Temp <- as.numeric(h$Temp)
h$DT <- as.POSIXct(h$DT, format = "%m/%d/%Y %H:%M:%S")

# Extract year from DT
year <- format(h$DT, "%Y")

# Correct year if it starts with '00'
year <- ifelse(substr(year, 1, 2) == "00", paste0("20", substr(year, 3, 4)), year)

# Update DT with corrected year
h$DT <- as.POSIXct(paste0(year, format(h$DT, "-%m-%d %H:%M:%S")), format = "%Y-%m-%d %H:%M:%S")

# Check if all years are corrected
summary(format(h$DT, "%Y"))

#RAW
FILTER8 <- filter(h, DT > "2023-01-20 18:00:00")
ggplot(data = FILTER8) +
  geom_line(aes(x = DT, y = Temp)) +
  scale_y_continuous(limits = c(25,40)) +
  geom_abline(slope = 0, intercept = 36.5, col = "red") 

#CLEAN
ggplot(data = FILTER8[which(FILTER8$keep=="Keep"),]) +
  geom_line(aes(x = DT, y = Temp)) +
  scale_y_continuous(limits = c(25,40)) +
  geom_abline(slope = 0, intercept = 36.5, col = "red")

##SAVE CLEANED DATA
clean_data <- h[which(h$keep=="Keep"),]
PTBlist$Ptcode[6]
write.csv(clean_data, "167F8B96_PTTB6.csv")



