# Eirini Trypidaki 
# e.trypidaki@creaf.uab.cat

## Quality control meteorological station data ##
#Connect the project to Github
library(usethis)
use_github() # a repository will be created in Github

##Install libraries
install.packages("dplyr")
install.packages("sf")
install.packages("ggplot2")
install.packages('lubridate')
install.packages("tidyr")
install.packages("openxlsx")

##Load libraries
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(tidyr)
library(openxlsx)



#Set working Directory
setwd("XXXXX")

#Load meteorological stations data
csv_directory <- "XXXXX"
csv_files <- list.files(csv_directory,pattern = "\\.csv$", full.names = TRUE)

#Load study Area
StudyArea <- st_read("C:/Users/e.trypidaki/OneDrive - CREAF/Escritorio/Data/EbroLimits/buffered25km.geojson", promote_to_multi = FALSE)

# List all the csv files with meteorological data
# Select variables
read_weather_data <- function(csv_files) {
  data <- lapply(csv_files, function(csv_file) {
    df <- read.csv(csv_file)
    df$Station_Name <- as.character(df$Station_Name)
    df$Source <- basename(csv_file)
    df$Year <- as.numeric(df$Year)
    df <- select(df,"Station_Name","Year","Month","Station_Altitude","Precipitacion.mm",
                 "Tmean.C","Tmin.C","Tmax.C","X","Y","Source")
    return(df)
  })
  data <- bind_rows(data)
  return(data)
}
# Load meteorological data
df <- read_weather_data(csv_files)


# Creating base data frames 
stationID_list <- unique(df$Station_Name)
# Transform Year and Month to YYYYMM date format
df$YYYYMMdate <- as.character(paste0(df$Year, "-", sprintf("%02d", df$Month), "-15"))

ShortSeries <- data.frame(station=character(), initial_date=Date(), end_date=Date(), series_years_length=numeric())
StationHighNApc <- data.frame(station = character(), NApc = numeric())
StationNApc <- data.frame(station = character(), NApc = numeric())

#_______________________________________________________________________________________
                  
#Set up QUALITY THRESHOLDS####
#_______________________________________________________________________________________
THRESHOLD_serieslength <- 1 # minimum length of a series
THRESHOLD_NApc <- 30 # Maximum NA%

for (station in "82") {
  subset_data <- df[df$Station_Name == "A137", ]

  # 1. Series length threshold____________________________________________________
  
  # Find the earliest and oldest dates
  earliest_date <- min(subset_data$YYYYMMdate, na.rm = TRUE)
  oldest_date <- max(subset_data$YYYYMMdate, na.rm = TRUE)
  
  # Calculate the difference in years between the dates
  difference_in_years <- as.numeric(difftime(oldest_date, earliest_date, units = "weeks") / 52.1775)
  
  # Create a sequence of all dates between the minimum and maximum dates for the station
    all_dates <- seq(from = as.Date(earliest_date), to = as.Date(oldest_date), by = "1 month") 
  # Convert all_dates to character format
    all_dates <- as.character(all_dates)
  # Use the complete function to fill in missing dates
    subset_data <- subset_data %>%
      complete(YYYYMMdate = all_dates)
  
  # Check if the difference in years is less than the length Threshold
  if (difference_in_years < THRESHOLD_serieslength) {
    print(paste("Time series for", station, "is shorter than", THRESHOLD_serieslength, "year."))
    info <- c(station, earliest_date, oldest_date, difference_in_years)
    ShortSeries <- rbind(ShortSeries, info)
  } 
  
  # 2.. Number of NA values. Who complete are the series. 
  
  # Calculate the number of NA values in the PREC column
  num_na <- sum(is.na(df$Precipitacion.mm))
  # Calculate the total number of values in the PREC column
  total_values <- length(all_dates)
  
  # Calculate the percentage of NA values
  percent_na <- (num_na / total_values) * 100
  StationNAresult <- c(station, percent_na)
  StationNApc <- rbind(StationNApc, StationNAresult)
  
  # Check if the % of NA values is greater than the Threshold %NA
  if (percent_na > THRESHOLD_NApc) {
    print(paste(station, "have more than", THRESHOLD_NApc, "% NA values."))
    StationHighNApc <- rbind( StationHighNApc,StationNAresult )
  } 
  
}

#_______________________________________________________________________________________

#Save reports (text,excel)####
#_______________________________________________________________________________________

# Text report

# Write report header line
header_line <- "QUALITY CONTROL REPORT"
write(header_line, "Quality Control.txt")

# Write Series length info:
header_line <- paste("List of stations with a time series shorter than the threshold:", THRESHOLD_serieslength)
write(header_line, "Quality Control.txt", append = TRUE)

# Write the extracted data frame to a text file
write.table(ShortSeries, file = "Quality Control.txt", append = TRUE,col.names=!file.exists("Quality Control.txt"))

# Write NA% info
header_line <- paste("List of stations with a NA% higher than the threshold:", THRESHOLD_NApc, "%.")
write(header_line, "Quality Control.txt", append = TRUE)

# Write the extracted data frame to a text file
write.table(StationHighNApc, file = "Quality Control.txt",   append = TRUE,col.names=!file.exists("Quality Control.txt"))

# No need to explicitly return a message, it will be returned automatically
"Extraction complete. Check Quality Control.txt files."

# Excel Report

# Create a new Excel workbook
wb <- createWorkbook()

# Add a worksheet for each section of the report
addWorksheet(wb, "Series Length")
addWorksheet(wb, "High NA Percentage")
addWorksheet(wb, "NA Percentage")

# Write headers to each worksheet
writeData(wb, sheet = "Series Length", x = "QUALITY CONTROL REPORT", startCol = 1, startRow = 1)
writeData(wb, sheet = "Series Length", x = paste("List of stations with a time series shorter than the threshold:", THRESHOLD_serieslength), startCol = 1, startRow = 2)

writeData(wb, sheet = "High NA Percentage", x = "QUALITY CONTROL REPORT", startCol = 1, startRow = 1)
writeData(wb, sheet = "High NA Percentage", x = paste("List of stations with an NA% higher than the threshold:", THRESHOLD_NApc, "%."), startCol = 1, startRow = 2)

writeData(wb, sheet = "NA Percentage", x = "QUALITY CONTROL REPORT", startCol = 1, startRow = 1)
writeData(wb, sheet = "NA Percentage", x = "List of stations with an NA%.", startCol = 1, startRow = 2)

# Write Series length info to Excel
writeData(wb, sheet = "Series Length", x = ShortSeries, startRow = 3)

# Write NA% info to Excel
writeData(wb, sheet = "High NA Percentage", x = StationHighNApc, startRow = 3)
writeData(wb, sheet = "NA Percentage", x = StationNApc, startRow = 3)

# Check if the file exists
if (file.exists("Quality Control.xlsx")) {
  # Delete the existing file
  file.remove("Quality Control.xlsx")
}

# Save the workbook to an Excel file
saveWorkbook(wb, "Quality Control.xlsx")




















#_______________________________________________________________________________________

##### Filter data by year ####
#_______________________________________________________________________________________
### Keep stations before 2000 with 5 or more years of data ##
### After 2000 with 1 or more years of data ##
### Remove Stations where all the values are NA ##
## Precipitation ##
prec_filtered <- prec %>%
  group_by(Station_Name) %>%
  filter(!all(is.na(Precipitacion.mm))) %>%
  filter((Year < 2000 & n_distinct(Year) >= 5) | (Year >= 2000 & n_distinct(Year) >= 1)) %>%
  ungroup()


## Temperature ##
tempe_filtered  <- tempe %>%
  group_by(Station_Name) %>%
  filter(!all(is.na(Tmean.C))) %>%
  filter((Year < 2000 & n_distinct(Year) >= 5) | (Year >= 2000 & n_distinct(Year) >= 1)) %>%
  ungroup()