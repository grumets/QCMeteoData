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
    df$Station_ID <- as.character(df$Station_ID)
    df$Source <- basename(csv_file)
    df$Year <- as.numeric(df$Year)
    df <- select(df,"Station_ID","Year","Month","Station_Altitude","Precipitacion.mm",
                 "Tmean.C","Tmin.C","Tmax.C","X","Y","Source")
    return(df)
  })
  data <- bind_rows(data)
  return(data)
}
# Load meteorological data
df <- read_weather_data(csv_files)
# Function to QA_preprocessing raw Meteodata

####ERRORS WITH IFELSE
QA_preprocessing <- function(raw_meteodata_df, Station_ID, Latitude, Longitude, Altitude, Variable, Year, Month) {
  # Create a new data frame with standardized column names and data types
  processed_df <- raw_meteodata_df %>%
    # Rename columns if needed
    rename(
      Station_ID = {{ Station_ID }},
      Latitude = {{ Latitude }},
      Longitude = {{ Longitude }},
      Altitude = {{ Altitude }},
      Variable = {{ Variable }}
    ) %>%
    
    # Check time columns and format
    mutate(
      Year = ifelse({{ Year }} %in% colnames(.), {{ Year }}, NA_real_),
      Month = ifelse({{ Month }} %in% colnames(.), {{ Month }}, NA_integer_),
      Date = ifelse({{ Date}} %in% colnames(.), {{ Date}}, NA_integer_),
      YYYYMMdate = as.Date(paste0(Year, "-", sprintf("%02d", Month), "-15"))
    ) %>%  
    group_by(Station_ID) %>%
    mutate(
      Start_Date = ifelse(
        "station_StartDate" %in% colnames(.),
        as.character(station_StartDate),
        min(YYYYMMdate)
      ),
      End_Date = ifelse(
        "station_EndDate" %in% colnames(.),
        as.character(station_EndDate),
        max(YYYYMMdate)
      )
    )
  # Return the processed data frame
  return(processed_df)
}
 
prec <- QA_preprocessing(df, "Station_ID","X","Y", "Station_Altitude", "Precipitacion.mm","Year", "Month")

####FOR NOW
process_data <- function(raw_df) {
  processed_df <- raw_df %>%
    rename(
      Station_ID = "Station_ID",
      Latitude = "X",
      Longitude = "Y",
      Altitude = "Station_Altitude",
      Precipitation = "Precipitacion.mm",
      Tmean = "Tmean.C",
      Tmin = "Tmin.C",
      Tmax = "Tmax.C"
    ) %>%
    mutate(
      YYYYMMdate = as.character(paste0(Year, "-", sprintf("%02d", Month), "-15"))
    ) %>%
    group_by(Station_ID) %>%
    mutate(
      Start_Date = ifelse(
        "station_StartDate" %in% colnames(.),
        as.character(station_StartDate),
        min(YYYYMMdate)
      ),
      End_Date = ifelse(
        "station_EndDate" %in% colnames(.),
        as.character(station_EndDate),
        max(YYYYMMdate)
      )
    ) %>%
    mutate(
      Start_Date = as.Date(Start_Date),
      End_Date = as.Date(End_Date)
    ) %>%
    ungroup() %>%
    select(
      Station_ID, Year, Month, Altitude, Precipitation, Tmean, Tmin, Tmax,
      Latitude, Longitude, Start_Date, End_Date, YYYYMMdate
    )
  
  return(processed_df)
}

# Calculate
processed_df <- process_data(df)


#_______________________________________________________________________________________
                  
#Length of the time series####
#_______________________________________________________________________________________

QA_serieslenght_shortlist <- function(df, variable, threshold_series_length) {
  # Initialize result data frames
  short_series <- data.frame(Station_ID = character(), 
                             Initial_date = character(), 
                             End_date = character(), 
                             Series_years_length = character(),
                             stringsAsFactors = FALSE)

  # Iterate over unique station names
  unique_stations <- unique(df$Station_ID)
  for (Station_ID in unique_stations) {
    # Subset data for the current station
    subset_data <- df[df$Station_ID == Station_ID, ]
    
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
        info <- c(Station_ID = Station_ID, 
                  Initial_date = earliest_date, 
                  End_date = oldest_date, 
                  Series_years_length = difference_in_years)
        short_series <- bind_rows(short_series, info)
      } 
    }
  }
  # Return the results as a list
  quality_control_results <-  short_series
  return(quality_control_results)
}

##calculate
short_series<- QA_serieslenght_shortlist(processed_df, "Precipitation", 1)

#_______________________________________________________________________________________

#% NA values####
#_______________________________________________________________________________________

QA_NApc_shortlist<- function(df, variable, threshold_na_percentage) {
  # Initialize result data frames
  high_na_percentage <- data.frame(Station_ID = character(), NA_percentage = character(),
                                   stringsAsFactors = FALSE)
  
  # Iterate over unique station names
  unique_stations <- unique(df$Station_ID)
  for (Station_ID in unique_stations) {
    # Subset data for the current station
    subset_data <- df[df$Station_ID == Station_ID, ]
    
    # Filter data to specific variable
    subset_data <- subset_data %>% filter(!all(is.na(.data[[variable]])))
    
    # Check if there are valid dates for the variable
    if (any(!is.na(subset_data$YYYYMMdate))) {
      # Find the earliest and oldest dates
      earliest_date <- min(subset_data$YYYYMMdate, na.rm = TRUE)
      oldest_date <- max(subset_data$YYYYMMdate, na.rm = TRUE)
 
      # Create a sequence of all the dates between the minimum and maximum dates for the station
      all_dates <- seq(from = as.Date(earliest_date), to = as.Date(oldest_date), by = "1 month") 
      all_dates <- as.character(all_dates)
      
      # Use the complete function to fill in missing dates
      subset_data <- subset_data %>%
        complete(YYYYMMdate = all_dates)
      
      # Calculate the percentage of NA values
      num_na <- sum(is.na(subset_data[[variable]]))
      total_values <- length(all_dates)
      percent_na <- (num_na / total_values) * 100
      
      # Check if the % of NA values is greater than the threshold NA percentage
      if (percent_na > threshold_na_percentage) {
        info <- c(Station_ID = Station_ID, NA_percentage = percent_na)
        high_na_percentage <- bind_rows(high_na_percentage, info)
    }
  }
  }
  # Return the results as a list
  StationHighNApc <-  high_na_percentage
  return(StationHighNApc)
}

##calculate
NA_precipitation<- QA_NApc_shortlist(processed_df, "Precipitation", 30)

#_______________________________________________________________________________________

#Save reports (text,excel)####
#_______________________________________________________________________________________
# Function to export report to both text and Excel files
export_report <- function(report_data, text_file_path, excel_file_path, row_names = FALSE) {
  # Export report to text file
  con <- file(text_file_path, "w")
  cat("Quality_Control\n", file = con)
  write.table(report_data, file = con, sep = "\t", row.names = row_names)
  close(con)
  
  # Export report to Excel file
  write.xlsx(report_data, file = excel_file_path, rowNames = row_names)
}

export_report(short_series,  "short_series_report.txt", "short_series_report.xlsx")
export_report(NA_precipitation,  "NA_precipitation_report.txt", "NA_precipitation_report.xlsx")


###NEEDS to include the threshold?


#__________________________________________________________________________
### QA_serieslenght_plot ####
#__________________________________________________________________________
precipitation_df <- processed_df %>%
    rename(Variable = "Precipitation")

####MAYBE IT NEEDS TO BE APPLICABLE FOR ALL THE VARIABLES IN THE SAME FUNCTION?

# Plot Temperature for selected stations for all the months #
QA_serieslenght_plot <- function(df,short_series,ylim_min,ylim_max) {
  # Filter data for shortlisted stations
  shortlisted_stations <- unique(short_series$Station_ID)
  shortlisted_data <- df[df$Station_ID %in% shortlisted_stations, ]
  
  # Plot time series for each shortlisted station
  plots <- list()
  for (Station_ID in shortlisted_stations) {
    station_data <- shortlisted_data[shortlisted_data$Station_ID == Station_ID, ]
    
    # Create plot for precipitation time series
    p <-  ggplot(station_data, aes(x = Month, y = Variable, color = Station_ID)) +
            geom_point() +  
             geom_line(aes(group = 1)) +   
      labs(title = paste("Time Series Plot for Station", Station_ID),
            x = "Month", y =  "Variable") +
            theme_minimal() +
            coord_cartesian(ylim = c({{ ylim_min }},{{ ylim_max }}))
    plots[[Station_ID]] <- p
  }
  
  return(plots)
}

# Generate plots for shortlisted stations
shortlisted_plots <- QA_serieslenght_plot (precipitation_df, short_series,0,250)


# Print individual plots
for (Station_ID in names(shortlisted_plots)) {
  print(shortlisted_plots[[Station_ID]])
}

#__________________________________________________________________________
### QA_serieslenght_clean ####
#__________________________________________________________________________


#__________________________________________________________________________
### QA_NApc_plot ####
#__________________________________________________________________________
precipitation_df <- processed_df %>%
  rename(Variable = "Precipitation")

####I THINK ITS BETTER THE NA TO BE PLOTTED BY YEAR

# Plot Temperature for selected stations for all the months #
QA_NApc_plot <- function(df,highNA,ylim_min,ylim_max) {
  # Filter data for shortlisted stations
  highNA_stations <- unique(highNA$Station_ID)
  highNA_data <- df[df$Station_ID %in% highNA_stations, ]
  
  # Plot time series for each shortlisted station
  plots <- list()
  for (Station_ID in highNA_stations) {
    station_data <- highNA_data[highNA_data$Station_ID == Station_ID, ]
    
    # Create plot for precipitation time series
    p <-  ggplot(station_data, aes(x = Year, y = Variable, color = Station_ID, group = 1)) +
      geom_point() +  
      geom_line() +   
      labs(title = paste("High percentage of NA in Station", Station_ID),
           x = "Year", y =  "Variable") +
      theme_minimal()  +
      facet_wrap(~Month, scales = "free_y")+
      coord_cartesian(ylim = c({{ ylim_min }},{{ ylim_max }}))
    plots[[Station_ID]] <- p
  }
  
  return(plots)
}

# Generate plots for shortlisted stations
High_NA_plots <- QA_NApc_plot(precipitation_df, NA_precipitation,0,350)


# Print individual plots
for (Station_ID in names(High_NA_plots)) {
  print(High_NA_plots[[Station_ID]])
}

#__________________________________________________________________________
### QA_NApc_clean ####
#__________________________________________________________________________



#_______________________________________________________________________________________

##### Removal of extreme values ####
##### QA_yearly_filtering ####
#_______________________________________________________________________________________
QA_yearly_filtering <- function(df, variable, threshold_year, min_obs_below_threshold, min_obs_above_threshold) {
  df %>%
    group_by(Station_ID) %>%
    filter(!all(is.na({{ variable }}))) %>%
    filter((Year < threshold_year & n_distinct(Year) >= min_obs_below_threshold) |
             (Year >= threshold_year & n_distinct(Year) >= min_obs_above_threshold)) %>%
    ungroup()
}

# Calculate
prec_filtered <- QA_yearly_filtering(processed_df, Precipitation,  2000, 5, 1)
tempe_filtered  <- QA_yearly_filtering(processed_df,  Tmean, 2000, 5, 1)

#_______________________________________________________________________________________
## Outliers ####
## Detection and removal physically impossible observations ####
## Check range of values to detect extremes 
## Precipitation:: Upper mean monthly threshold for 1500mm and negative values ##
## Temperature:: Upper mean monthly threshold for 50 and negative values ##
#_______________________________________________________________________________________
QA_outlier_variation <- function(df, variable) {
  df %>%
    group_by(Station_ID, Month) %>%
    summarise_at(vars({{ variable }}), list(Mean = ~ifelse(all(is.na(.)), NA, mean(., na.rm = TRUE)),
                                   Max = ~ifelse(all(is.na(.)), NA, max(., na.rm = TRUE)),
                                   Min = ~ifelse(all(is.na(.)), NA, min(., na.rm = TRUE)),
                                   SD = ~ifelse(all(is.na(.)), NA, sd(., na.rm = TRUE)),
                                   Range = ~ifelse(all(is.na(.)), NA, max(., na.rm = TRUE) - min(., na.rm = TRUE))))
}

# Calculate summary statistics for precipitation and temperature
prec_statistics <- QA_outlier_variation(prec_filtered, Precipitation)
tempe_statistics <- QA_outlier_variation(tempe_filtered, Tmean)

### Function to delete extreme values of precipitation ##
QA_outlier_precipitation <- function(df, variable,threshold) {
  # Filter extreme precipitation values
  filtered <- df %>%
    filter(is.na({{ variable }}) | ({{ variable }} >= 0 & {{ variable }} <= threshold))
  return(filtered)
}
# Calculate
prec_filtered2 <- QA_outlier_precipitation (prec_filtered ,Precipitation,1500)
# Create a dataset containing removed values
removed_valuesP <- anti_join(prec_filtered, prec_filtered2)

### Function to delete extreme values of temperature ##
QA_outlier_temperature <- function(df, Tmean_variable, Tmax_variable, Tmin_variable, temp_min, temp_max) {
  filtered_temperature <- df %>%
    filter(
      # Check if Tmean is within the specified range
      is.na(.data[[Tmean_variable]]) | 
        (.data[[Tmean_variable]] > temp_min & .data[[Tmean_variable]] < temp_max),
      
      # Check if Tmin is within the specified range or if it's missing
      is.na(.data[[Tmin_variable]]) | 
        .data[[Tmin_variable]] == 0 | 
        (.data[[Tmean_variable]] >= .data[[Tmin_variable]]),
      
      # Check if Tmax is within the specified range or if it's missing
      is.na(.data[[Tmax_variable]])  | 
        .data[[Tmax_variable]] == 0 |
        (.data[[Tmean_variable]] <= .data[[Tmax_variable]])
    )
  
  return(filtered_temperature)
}
####MORE WORK HERE!! maybe can be simpler??
# Calculate
tempe_filtered2 <- QA_outlier_temperature(tempe_filtered ,"Tmean", "Tmax","Tmin", -20,45)
# Create a data set containing removed values
removed_valuesT <- anti_join(tempe_filtered, tempe_filtered2)


# Calculate summary statistics for precipitation and temperature
prec_statistics_filtered <- QA_outlier_variation(prec_filtered2, Precipitation)
tempe_statistics_filtered <- QA_outlier_variation(tempe_filtered2, Tmean)

#_______________________________________________________________________________________
        ### QA_reliable_stations ####
        #### Selection of Precipitation stations with reliable data#### 
        #### Assign a threshold based on selected stations #
#_______________________________________________________________________________________  
QA_reliable_stations <- function(df, variable, threshold_year, count_threshold) {
  filtered_stations <- df %>%
    mutate(Count_obs = if_else(!is.na({{ variable }}) & Year > {{ threshold_year }}, 1, 0)) %>%
    group_by(Station_ID) %>%
    mutate(Count_obs = sum(Count_obs)) %>%
    filter(Count_obs > {{ count_threshold }}) %>%
    select(Station_ID, Longitude, Latitude, Altitude) %>%
    distinct() 
  
  return(filtered_stations)
}

# Calculate for observation after 1990 and more than 240 (around 20 years, so more reliable)
prec_stations <- QA_reliable_stations(prec_filtered2, Precipitation,  1990,  240)
tempe_stations <- QA_reliable_stations(tempe_filtered2, Tmean,  1990,  240)

# Function to perform the sampling process for diverse stations in the study area
QA_Stations_sampling <- function(data) {
  
 #Function to calculate distances between X,Y,Altitude
  calculate_differences <- function(data) {
    diff_x <- outer(data$Longitude, data$Longitude, "-")
    diff_y <- outer(data$Latitude, data$Latitude, "-")
    diff_altitude <- outer(data$Altitude, data$Altitude, "-")
    list(diff_x = diff_x, diff_y = diff_y, diff_altitude = diff_altitude)
  }
  # Calculate differences
  differences <- calculate_differences(data)
  
  # Function to find indices of points with the largest distance/difference
  find_largest_distance_indices <- function(diff_matrix) {
    indices <- which(diff_matrix == max(diff_matrix, na.rm = TRUE), arr.ind = TRUE)
    unique(indices)
  }
  
  # Find indices with the largest distance
  indices_x <- find_largest_distance_indices(differences$diff_x)
  indices_y <- find_largest_distance_indices(differences$diff_y)
  indices_altitude <- find_largest_distance_indices(differences$diff_altitude)
  
  # Combine indices
  selected_indices <- unique(c(indices_x, indices_y, indices_altitude))
  
  # Function to select points based on the largest distance
  select_points <- function(data, indices) {
    data[indices, ]
  }
  
  # Select points based on the indices
  selected_points <- select_points(data, selected_indices)
  
  return(selected_points)
}

##Selection of points to define the threshold
temp_selected_points <- QA_Stations_sampling(tempe_stations)
prec_selected_points <- QA_Stations_sampling(prec_stations)

###Transform to st obj
prec_points <- st_as_sf(prec_selected_points, coords = c( "Latitude","Longitude"), crs = 25830)
temp_points <- st_as_sf(temp_selected_points, coords = c("Latitude","Longitude"), crs = 25830)

# Plot the polygon and random points
plot(StudyArea$geometry)
plot(st_geometry(prec_points),add = TRUE,  pch = 19, col = "red4")
plot(st_geometry(temp_points), add = TRUE, pch = 19, col = "blue4")


#_______________________________________________________________________________________
### Plot Function for  stations####
#### Assign a threshold based on selected stations #
#### Plot a specific Station for many year for a month
#_______________________________________________________________________________________ 
Station_ID<- unique(selected_points$Station_ID)

# Plot Temperature for selected stations for all the months #
QA_plot_yearly <- function(df, variable, selected_points, ylim_min, ylim_max) {
  # Filter df for shortlisted stations
  selected_df <- df[df$Station_ID %in% unique(selected_points$Station_ID), ]
  
  # Plot time series for each selected stations
  ggplot(selected_df, aes(x = Year, y = .data[[variable]], color = Station_ID)) +
    geom_point() +  # Plot points
    geom_line() +   # Connect points with lines
    labs(title = paste(variable, "by Month"),
         x = "Year", y = variable) +
    theme_minimal() +  # Minimal theme
    facet_wrap(~Month, scales = "free_y") +
    coord_cartesian(ylim = c(ylim_min, ylim_max))
}

# Generate plots for reliable stations
QA_plot_yearly(prec_filtered2,"Precipitation",prec_selected_points,0,500)
QA_plot_yearly(tempe_filtered2,"Tmean",temp_selected_points,-10,30)


#___________________________________________________________________________________________________________________________________________

### Outliers shortlists ###########
#___________________________________________________________________________________________________________________________________________# Select a specific station with many observations
## For temperature
QA_outlier_shortlist <- function(df_statistics, Range_threshold) {
# Filter out stations, which monthly range exceeds 10 degrees Celsius
    outliers <- df_statistics %>%
      filter(Range >= {{Range_threshold}})%>%
      select(Station_ID,Month,Range) %>%
      distinct()
    
    return(outliers)
}

outliersT <- QA_outlier_shortlist(tempe_statistics_filtered,10)
##Export results in a report
export_report(outliersT,  "outliers_temperature_report.txt", "outliers_temperature_report.xlsx")


# Plot outliers
QA_outlier_plot <- function(df,outliers,ylim_min,ylim_max) {
  # Filter data for shortlisted stations
  outliers_stations <- unique(outliers$Station_ID)
  outliers_data <- df[df$Station_ID %in% outliers_stations, ]
  
  # Plot time series for each shortlisted station
  plots <- list()
  for (Station_ID in outliers_stations) {
    station_data <- outliers_data[outliers_data$Station_ID == Station_ID, ]
    
    # Create plot for precipitation time series
    p <-  ggplot(station_data, aes(x = Year, y = Variable, color = Station_ID, group = 1)) +
      geom_point() +  
      geom_line() +   
      labs(title = paste("Possible outliers in Station", Station_ID),
           x = "Year", y =  "Variable") +
      theme_minimal()  +
      facet_wrap(~Month, scales = "free_y")+
      coord_cartesian(ylim = c({{ ylim_min }},{{ ylim_max }}))
    plots[[Station_ID]] <- p
  }
  
  return(plots)
}

##Rename variable
tempe_filtered2 <- tempe_filtered2 %>%
  rename(Variable = "Tmean") 
# Generate plots for shortlisted stations
Outliers_plots <- QA_outlier_plot(tempe_filtered2, outliersT,-10,35)


# Print individual plots
for (Station_ID in names(Outliers_plots)) {
  print(Outliers_plots[[Station_ID]])
}



