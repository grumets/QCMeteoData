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
                  
#Set up Quality Thresholf####
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
###ERROR IF THE FILE ALREADY EXIST
  ##DELETE DOESN'T WORK
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

### Function to delete extreme values ##
filter_extreme_precipitation <- function(df, variable,threshold) {
  # Filter extreme precipitation values
  filtered_precip <- df %>%
    filter(is.na({{ variable }}) | ({{ variable }} >= 0 & {{ variable }} <= threshold))
  return(filtered_precip)
}


filter_extreme_temperature <- function(df, Tmean_variable, Tmax_variable, Tmin_variable, temp_min, temp_max) {
  filtered_temperature <- df %>%
    filter(is.na(.data[[Tmean_variable]]) | (.data[[Tmean_variable]] > temp_min & .data[[Tmean_variable]] < temp_max)) %>%
    filter(is.na(.data[[Tmin_variable]]) | .data[[Tmin_variable]] == 0 | is.na(.data[[Tmean_variable]]) | 
             .data[[Tmean_variable]] == 0 | is.na(.data[[Tmax_variable]])  | .data[[Tmax_variable]] == 0 |
             (.data[[Tmean_variable]] >= .data[[Tmin_variable]] & .data[[Tmean_variable]] <= .data[[Tmax_variable]])) 
  return(filtered_temperature)
}
####MORE WORK HERE
####NEEDS ADDITIONS IF TMAX OR TMIN DOESNT EXIST

# Calculate
prec_filtered <- filter_extreme_precipitation(prec_filtered ,Precipitacion.mm,1500)
tempe_filtered <- filter_extreme_temperature(tempe_filtered ,"Tmean.C","Tmax.C","Tmin.C",-20,45)

# Calculate summary statistics for precipitation and temperature
prec_statistics_filtered <- statistics(prec_filtered, Precipitacion.mm)
tempe_statistics_filtered <- statistics(tempe_filtered, Tmean.C)

#_______________________________________________________________________________________
### Selection of Precipitation stations with reliable data####
#### Assign a threshold based on selected stations #
#_______________________________________________________________________________________  
# Filter stations with observation counts greater than 240 (at least 20 years data), after 1990 (more reliable)

reliable_stations <- function(df, variable, variable_year, station_id, X, Y, Altitude, threshold_year, count_threshold) {
  filtered_stations <- df %>%
    mutate(Count_obs = if_else(!is.na({{ variable }}) & {{ variable_year }} > {{ threshold_year }}, 1, 0)) %>%
    group_by({{ station_id }}) %>%
    mutate(Count_obs = sum(Count_obs)) %>%
    filter(Count_obs > {{ count_threshold }}) %>%
    ungroup()
  
  filtered_stations <- filtered_stations %>%
    select({{ station_id }}, {{ X }}, {{ Y }}, {{ Altitude }}) %>%
    distinct() 
  
  return(filtered_stations)
}


# Calculate
prec_stations <- reliable_stations(prec_filtered, Tmean.C,  Year,  Station_Name, X, Y, Station_Altitude,  1990,  240)
tempe_stations <- reliable_stations(tempe_filtered, Tmean.C,  Year,  Station_Name, X, Y, Station_Altitude,  1990,  240)



# Function to calculate differences in distance and altitude
calculate_differences <- function(data) {
  diff_x <- outer(data$X, data$X, "-")
  diff_y <- outer(data$Y, data$Y, "-")
  diff_altitude <- outer(data$Station_Altitude, data$Station_Altitude, "-")
  list(diff_x = diff_x, diff_y = diff_y, diff_altitude = diff_altitude)
}

# Function to find indices of points with the largest distance/difference
find_largest_distance_indices <- function(diff_matrix) {
  indices <- which(diff_matrix == max(diff_matrix, na.rm = TRUE), arr.ind = TRUE)
  unique(indices)
}

# Function to select points based on the largest distance
select_points <- function(data, indices) {
  data[indices, ]
}

# Function to perform the sampling process
perform_sampling <- function(df) {

  # Calculate differences
  differences <- calculate_differences(df)
  
  # Find indices with the largest distance
  indices_x <- find_largest_distance_indices(differences$diff_x)
  indices_y <- find_largest_distance_indices(differences$diff_y)
  indices_altitude <- find_largest_distance_indices(differences$diff_altitude)
  
  # Combine indices
  selected_indices <- unique(c(indices_x, indices_y, indices_altitude))
  
  # Select points based on the indices
  selected_points <- select_points(df, selected_indices)
  
  return(selected_points)
}
####################ERROR!!!
#########Same results
##Selection of points to define the threshold
prec_selected_points <- perform_sampling(prec_stations)
temp_selected_points <- perform_sampling(tempe_stations)                                       
prec_points <- st_as_sf(prec_selected_points, coords = c("X", "Y"), crs = 25830)
temp_points <- st_as_sf(temp_selected_points, coords = c("X", "Y"), crs = 25830)

# Plot the polygon and random points
plot(StudyArea$geometry)
plot(prec_points, add = TRUE, pch = 19, col = "red4")
plot(temp_points, add = TRUE, pch = 19, col = "blue4")


#_______________________________________________________________________________________
### Plot Function for  stations####
#### Assign a threshold based on selected stations #
#### Plot a specific Station for many year for a month
#_______________________________________________________________________________________ 
station_name<- unique(selected_points$Station_Name)

# Plot Temperature for selected stations for all the months #
plot_yearly <- function(data,variable,ylim_min,ylim_max) {
  ggplot(data, aes(x = Year, y = {{ variable }}, color = Station_Name)) +
    geom_point() +  # Plot points
    geom_line() +   # Connect points with lines
    labs(title = "{{ variable }} by Month",
         x = "Year", y = "{{ variable }}") +
    theme_minimal() +  # Minimal theme
    facet_wrap(~Month, scales = "free_y")+
    coord_cartesian(ylim = c({{ ylim_min }},{{ ylim_max }}))
}
####################ERROR!!!
######### more work
plot_yearly(tempe_filtered%>%
                   filter(Station_Name %in% "Alcanar" & Year > 2000), Tmean.C, -10,35)

#___________________________________________________________________________________________________________________________________________

### Detection of outliers ###########
#___________________________________________________________________________________________________________________________________________# Select a specific station with many observations
# Range of values per month per stations bigger than 10 degrees
# Filter out stations and months where the range exceeds 10 degrees Celsius
errorsT  <- tempe_statistics_filtered%>%
  filter(Range >= 10)%>%
  select(Station_Name)%>%
  distinct()
station_name<- unique(sort(errorsT$Station_Name))

# Create groups to check easier
station_groups <- split(unique(station_name), ceiling(seq_along(station_name)/4))

# Plotting specific stations for outlier detection
plot_outlier_detection <- function(data, station_groups) {
  plots <- lapply(station_groups, function(group) {
    plot_temperature(data %>% filter(Station_Name %in% group  & Year > 1990))
  })
  return(plots)
}
plot_outlier_detection(tempe_filtered, station_groups[1])
plot_outlier_detection(tempe_filtered, station_groups[2])
plot_outlier_detection(tempe_filtered, station_groups[3])
plot_outlier_detection(tempe_filtered, station_groups[4])
plot_outlier_detection(tempe_filtered, station_groups[5])
plot_outlier_detection(tempe_filtered, station_groups[6])
plot_outlier_detection(tempe_filtered, station_groups[7])
plot_outlier_detection(tempe_filtered, station_groups[8])
plot_outlier_detection(tempe_filtered, station_groups[9])


