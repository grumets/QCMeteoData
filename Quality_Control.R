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
#_______________________________________________________________________________________

#Dataframe preprocessing####
#_______________________________________________________________________________________

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
# Transform Year and Month to YYYYMM date format
df$YYYYMMdate <- as.character(paste0(df$Year, "-", sprintf("%02d", df$Month), "-15"))



#_______________________________________________________________________________________
                  
#Set up QUALITY THRESHOLDS####
#_______________________________________________________________________________________

perform_quality_control_variable <- function(df, variable, threshold_series_length, threshold_na_percentage) {
  # Initialize result data frames
  short_series <- data.frame(Station_Name = character(), 
                             Initial_date = character(), 
                             End_date = character(), 
                             Series_years_length = character(),
                             stringsAsFactors = FALSE)
  
  high_na_percentage <- data.frame(Station_Name = character(), NA_percentage = character(),
                                   stringsAsFactors = FALSE)
  
  # Iterate over unique station names
  unique_stations <- unique(df$Station_Name)
  for (station_name in unique_stations) {
    # Subset data for the current station
    subset_data <- df[df$Station_Name == station_name, ]
    
    # Filter data to specific variable
    subset_data <- subset_data %>% filter(!all(is.na(.data[[variable]])))
    
    
    # Check if there are valid dates for the variable
    if (any(!is.na(subset_data$YYYYMMdate))) {
      # Find the earliest and oldest dates
      earliest_date <- min(subset_data$YYYYMMdate, na.rm = TRUE)
      oldest_date <- max(subset_data$YYYYMMdate, na.rm = TRUE)
      
      # Calculate the difference in years between the dates
      difference_in_years <- as.numeric(difftime(oldest_date, earliest_date, units = "weeks") / 52.1775)
      
      # Create a sequence of all the dates between the minimum and maximum dates for the station
      all_dates <- seq(from = as.Date(earliest_date), to = as.Date(oldest_date), by = "1 month") 
      all_dates <- as.character(all_dates)
      
      # Use the complete function to fill in missing dates
      subset_data <- subset_data %>%
        complete(YYYYMMdate = all_dates)
      
      
      # Check if the difference in years is less than the threshold series length
      if (difference_in_years < threshold_series_length) {
        info <- c(Station_Name = station_name, 
                  Initial_date = earliest_date, 
                  End_date = oldest_date, 
                  Series_years_length = difference_in_years)
        short_series <- bind_rows(short_series, info)
      } 
      
      # Calculate the percentage of NA values
      num_na <- sum(is.na(subset_data[[variable]]))
      total_values <- length(all_dates)
      percent_na <- (num_na / total_values) * 100
      
      # Check if the % of NA values is greater than the threshold NA percentage
      if (percent_na > threshold_na_percentage) {
        info <- c(Station_Name = station_name, NA_percentage = percent_na)
        high_na_percentage <- bind_rows(high_na_percentage, info)
      }
    } 
  }
  
  # Return the results as a list
  quality_control_results <- list(ShortSeries = short_series, StationHighNApc = high_na_percentage)
  return(quality_control_results)
}

  
##calculate
quality_control_precipitation<- perform_quality_control_variable(df, "Precipitacion.mm", 1, 30)

#_______________________________________________________________________________________

#Save reports (text,excel)####
#_______________________________________________________________________________________

# Text report
write_report_text <- function(quality_control_results, filename = "Quality_Control_Report.txt") {
  # Open file for writing
  con <- file(filename, "w")
  
  # Write header
  cat("QUALITY CONTROL REPORT\n", file = con)
  
  # Write Series length info
  cat("List of stations with a time series shorter than the threshold:\n", file = con)
  write.table(quality_control_results$ShortSeries, file = con, row.names = FALSE)
  
  # Write NA% info
  cat("\nList of stations with an NA percentage higher than the threshold:\n", file = con)
  write.table(quality_control_results$StationHighNApc, file = con, row.names = FALSE)
  
  # Close the file
  close(con)
}

# Excel report
write_report_excel <- function(quality_control_results, filename = "Quality_Control_Report.xlsx") {
  
  # Check if the file exists
  if (file.exists("Quality Control.xlsx")) {
    # Delete the existing file
    file.remove("Quality Control.xlsx") 
  }
  # Create a new workbook
  wb <- createWorkbook()
  # Add worksheets
  addWorksheet(wb, "ShortSeries")
  addWorksheet(wb, "StationHighNApc")
  
  # Write data to worksheets
  writeData(wb, "ShortSeries", quality_control_results$ShortSeries)
  writeData(wb, "StationHighNApc", quality_control_results$StationHighNApc)
  
  
  # Save the workbook
  saveWorkbook(wb, filename)
}

# Combine both reports
  write_report_text(quality_control_precipitation)
  write_report_excel(quality_control_precipitation)

#_______________________________________________________________________________________

##### Filter data by year ####
#_______________________________________________________________________________________
### Keep stations before 2000 with 5 or more years of data ##
### After 2000 with 1 or more years of data ##
### Remove Stations where all the values are NA ##

filter_year <- function(df, station, variable, year_variable, threshold_year, min_obs_below_threshold, min_obs_above_threshold) {
  df %>%
    group_by({{ station }}) %>%
    filter(!all(is.na({{ variable }}))) %>%
    filter(({{ year_variable }} < threshold_year & n_distinct({{ year_variable }}) >= min_obs_below_threshold) |
             ({{ year_variable }} >= threshold_year & n_distinct({{ year_variable }}) >= min_obs_above_threshold)) %>%
    ungroup()
}

#Filter
prec_filtered <- filter_year(df, Station_Name, Precipitacion.mm, Year, 2000, 5, 1)
tempe_filtered  <- filter_year(df, Station_Name, Tmean.C, Year, 2000, 5, 1)

#_______________________________________________________________________________________
## Statistics ####
## Detection and removal physically impossible observations ####
## Check range of values to detect extremes 
## Precipitation:: Upper mean monthly threshold for 1500mm and negative values ##
## Temperature:: Upper mean monthly threshold for 50 and negative values ##
#_______________________________________________________________________________________
statistics <- function(df, variable) {
  df %>%
    group_by(Station_Name, Month) %>%
    summarise_at(vars({{ variable }}), list(Mean = ~ifelse(all(is.na(.)), NA, mean(., na.rm = TRUE)),
                                   Max = ~ifelse(all(is.na(.)), NA, max(., na.rm = TRUE)),
                                   Min = ~ifelse(all(is.na(.)), NA, min(., na.rm = TRUE)),
                                   SD = ~ifelse(all(is.na(.)), NA, sd(., na.rm = TRUE)),
                                   Range = ~ifelse(all(is.na(.)), NA, max(., na.rm = TRUE) - min(., na.rm = TRUE))))
}

# Calculate summary statistics for precipitation and temperature
prec_statistics <- statistics(prec_filtered, Precipitacion.mm)
tempe_statistics <- statistics(tempe_filtered, Tmean.C)

### Stations with extreme  precipitation values ##
prec_filtered <- prec_filtered %>%
  filter(is.na(Precipitacion.mm) | (Precipitacion.mm >= 0 & Precipitacion.mm <= 1500))
### Stations with extreme temperature values ##
### Stations with Tmean higher than Tmax or smaller that Tmin ##
tempe_filtered <- tempe_filtered %>%
  filter(is.na(Tmean.C) | (Tmean.C > -20 & Tmean.C < 45))%>%
  filter(is.na(Tmin.C) | Tmin.C == 0 | is.na(Tmean.C) | Tmean.C == 0|is.na(Tmax.C) 
         | Tmax.C == 0 |(Tmean.C >= Tmin.C & Tmean.C <= Tmax.C)) %>%
  select("Station_Name", "Year", "Month", "Station_Altitude", "Tmean.C","X","Y", "Source")

# Calculate summary statistics for precipitation and temperature
prec_statistics_filtered <- statistics(prec_filtered, "Precipitacion.mm")
tempe_statistics_filtered <- statistics(tempe_filtered, c("Tmean.C"))

