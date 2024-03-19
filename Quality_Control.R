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
#setwd("XXXXX")
setwd("C:\\Users\\e.trypidaki\\OneDrive - CREAF\\Escritorio\\Data\\Quality_Control")

#Load meteorological stations data
#csv_directory <- "XXXXX"
csv_directory <- "C:/Users/e.trypidaki/OneDrive - CREAF/Escritorio/Data/FirstClean/"
csv_files <- list.files(csv_directory,pattern = "\\.csv$", full.names = TRUE)

#Load study Area
StudyArea <- st_read("C:/Users/e.trypidaki/OneDrive - CREAF/Escritorio/Data/EbroLimits/buffered25km.geojson", promote_to_multi = FALSE)

#_______________________________________________________________________________________
### Pre-processing functions ####
#_______________________________________________________________________________________
# List all the csv files with meteorological data
# Select variables
read_weather_data <- function(csv_files) {
  # Read and process each CSV file
  data <- lapply(csv_files, function(csv_file) {
    df <- read.csv(csv_file)
    df$Station_ID <- as.character(df$Station_Name)
    df$Source <- basename(csv_file)
    df$Year <- as.numeric(df$Year)
    df$Month <- as.numeric(df$Month)
    # Select required columns
    df <- select(df, "Station_ID", "Year", "Month", "Station_Altitude", "Precipitacion.mm",
                 "Tmean.C", "Tmin.C", "Tmax.C", "X", "Y", "Source")

    return(df)  # Return the processed dataframe
  })
  
  # Combine all processed dataframes into one
  data <- bind_rows(data)
  
  return(data)
}
# Load meteorological data
df <- read_weather_data(csv_files)

# Function to QA_preprocessing raw Meteodata
QA_preprocessing <- function(raw_meteodata_df, Station_ID,Longitude, Latitude,  Altitude, Precipitation, Tmean, Tmin, Tmax) {
  processed_df <- raw_meteodata_df %>%
    dplyr::rename(
      Station_ID = {{ Station_ID }},
      Longitude = {{ Longitude }},
      Latitude = {{ Latitude }},
      Altitude = {{ Altitude }},
      Precipitation = {{ Precipitation }},
      Tmean = {{ Tmean }},
      Tmin = {{ Tmin }},
      Tmax = {{ Tmax }}
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
processed_df <- QA_preprocessing(df, "Station_ID", "X", "Y", "Station_Altitude", "Precipitacion.mm", "Tmean.C", "Tmin.C", "Tmax.C")

#_______________________________________________________________________________________
### Create functions####
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

# Function to plot monthly for selected stations
QA_plot_yearly <- function(df, variable, selected_points) {
  # Filter df for shortlisted stations
  selected_df <- df[df$Station_ID %in% unique(selected_points$Station_ID), ]
  
  # Get unique stations with their altitudes
  selected_df$StationID_Altitude <- paste(selected_df$Station_ID, ",", selected_df$Altitude,"m")
  
  # Plot time series for each selected station
  p <- ggplot(selected_df, aes(x = Year, y = .data[[variable]], color = StationID_Altitude)) +
    geom_point() +  # Plot points
    geom_line() +   # Connect points with lines
    labs(title = paste(variable, "by Month"),
         x = "Year", y = variable,
         color = "StationID_Altitude") +
    theme_minimal() +  # Minimal theme
    facet_wrap(~Month, scales = "free_y") 
  
  return(p)
}

# Function to plot monthly for selected stations with mean and 2SD
QA_plot_yearly_meansd <- function(df, variable, selected_points) {
  # Filter df for shortlisted stations
  selected_df <- df[df$Station_ID %in% unique(selected_points$Station_ID), ]
  
  # Calculate mean and 2 SD
  mean_value <- mean(selected_df[[variable]], na.rm = TRUE)
  sd_value <- sd(selected_df[[variable]], na.rm = TRUE)
  upper_sd <- mean_value + 2 * sd_value
  lower_sd <- mean_value - 2 * sd_value
  
  # Get unique stations with their altitudes
  selected_df$StationID_Altitude <- paste(selected_df$Station_ID, ",", selected_df$Altitude,"m")
  
  # Plot time series for each selected station
  p <- ggplot(selected_df, aes(x = Year, y = .data[[variable]], color = StationID_Altitude)) +
    geom_point() +  # Plot points
    geom_line() +   # Connect points with lines
    geom_hline(yintercept = mean_value, linetype = "dashed", color = "red4") +  # Add mean line
    geom_hline(yintercept = upper_sd, linetype = "dashed", color = "blue4") +   # Add upper 2 SD line
    geom_hline(yintercept = lower_sd, linetype = "dashed", color = "blue4") +   # Add lower 2 SD line
    labs(title = paste(variable, "by Month"),
         x = "Year", y = variable,
         color = "StationID_Altitude") +
    theme_minimal() +  # Minimal theme
    facet_wrap(~Month, scales = "free_y") +
    ylim(min(lower_sd, min(selected_df[[variable]], na.rm = TRUE)), 
         max(upper_sd, max(selected_df[[variable]], na.rm = TRUE)))  # Set constant y-range
  
  return(p)
}

#_______________________________________________________________________________________
### Length of the time series####
### QA_serieslenght_shortlist #
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
      #Arrange so it count from the first date of data
      subset_data <- arrange(subset_data, YYYYMMdate)
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

##Calculate
short_series<- QA_serieslenght_shortlist(processed_df, "Precipitation", 1)

#Export report
export_report(short_series,  "short_series_report.txt", "short_series_report.xlsx")

#__________________________________________________________________________
#QA_serieslenght_plot#
#__________________________________________________________________________
QA_serieslenght_plot <- function(df, short_series_variable, variable_name) {
  # Filter data for shortlisted stations
  shortlisted_stations <- unique(short_series_variable$Station_ID)
  shortlisted_data <- df[df$Station_ID %in% shortlisted_stations, ]
  
  # Plot time series for each shortlisted station
  plots <- list()
  for (Station_ID in shortlisted_stations) {
    station_data <- shortlisted_data[shortlisted_data$Station_ID == Station_ID, ]
    
    # Create plot for the specified variable time series
    p <- ggplot(station_data, aes(x = Month, y = !!sym(variable_name), color = Station_ID)) +
      geom_point() +  
      geom_line(aes(group = 1)) +   
      labs(title = paste("Time Series Plot for", variable_name, "in Station", Station_ID),
           x = "Month", y = variable_name) +
      theme_minimal() 
    plots[[Station_ID]] <- p
  }
  
  return(plots)
}

# Generate plots for shortlisted stations
shortlisted_plots <- QA_serieslenght_plot(processed_df, short_series, "Precipitation")

# Print individual plots
for (Station_ID in names(shortlisted_plots)) {
  print(shortlisted_plots[[Station_ID]])
}

#__________________________________________________________________________
#QA_serieslenght_clean#
#__________________________________________________________________________
QA_serieslenght_clean <- function(df, stations_to_remove) {
  # Filter out rows corresponding to the specified stations
  cleaned_df <- df[!df$Station_ID %in% stations_to_remove, ]
  
  return(cleaned_df)
}

#Calculate
stations_to_removeP <- c("0002I","1159C")

#Calculate df with short series
cleaned_df1 <- QA_serieslenght_clean(processed_df,stations_to_removeP)
# Create a data set containing removed values
removed_values <- anti_join(processed_df,cleaned_df1)

#_______________________________________________________________________________________
### Percentage of NA values####
#QA_NApc_shortlist#
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

##Calculate
NA_precipitation<- QA_NApc_shortlist(cleaned_df1, "Precipitation", 30)

#Export report
export_report(NA_precipitation,  "NA_precipitation_report.txt", "NA_precipitation_report.xlsx")

#__________________________________________________________________________
#QA_NApc_plot #
#__________________________________________________________________________
QA_NApc_plot <- function(df, highNA_variable, variable_name) {
  # Filter data for shortlisted stations
  highNA_stations <- unique(highNA_variable$Station_ID)
  highNA_data <- df[df$Station_ID %in% highNA_stations, ]
  
  # Plot time series for each shortlisted station
  plots <- list()
  for (Station_ID in highNA_stations) {
    station_data <- highNA_data[highNA_data$Station_ID == Station_ID, ]
    
    # Calculate the range of the y-axis
    y_range <- range(station_data[[variable_name]], na.rm = TRUE)
    
    # Create plot for variables with NA
    p <- ggplot(station_data, aes(x = Year, y = !!sym(variable_name), color = Station_ID, group = 1)) +
      geom_point() +  
      geom_line() +    
      labs(title = paste("High percentage of NA for", variable_name,"in Station", Station_ID),
           x = "Year", y = variable_name) +
      theme_minimal() +
      facet_wrap(~Month, scales = "free_y") +
      ylim(y_range)  
    plots[[Station_ID]] <- p
  }
  return(plots)
}

# Generate plots for shortlisted stations
High_NA_plots <- QA_NApc_plot(cleaned_df1, NA_precipitation, "Precipitation")

# Print individual plots
for (Station_ID in names(High_NA_plots)) {
  print(High_NA_plots[[Station_ID]])
}

#__________________________________________________________________________
#QA_NApc_clean#
#__________________________________________________________________________
QA_NApc_clean <- function(df, stations_to_remove) {
  # Filter out rows corresponding to the specified stations
  cleaned_df <- df[!df$Station_ID %in% stations_to_remove, ]
  
  return(cleaned_df)
}

#Calculate
stations_to_removeP <- c("E092","65138402","9952")

#Calculate df with short series
cleaned_df2 <- QA_NApc_clean(cleaned_df1,stations_to_removeP)

# Create a data set containing removed values
removed_values <- anti_join(cleaned_df1, cleaned_df2)


#_______________________________________________________________________________________
### QA_plots for station count and time periods ####
#_______________________________________________________________________________________
# Function to plot observation count by number of stations against years
QA_obs_plot <- function(df, variable_name) {
  # Group data by year and count the number of stations with observations
  observation_count <- df %>%
    filter(!is.na(.data[[variable_name]])) %>%
    group_by(Year) %>%
    reframe(Station_Count = n_distinct(Station_ID))
  
  # Plot observation count by number of stations against years
  p <- ggplot(observation_count, aes(x = Year, y = Station_Count)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Number of", variable_name, "observations by Number of Stations"),
         x = "Year", y = "Number of Stations") +
    scale_x_continuous(breaks = seq(min(observation_count$Year), max(observation_count$Year), by = 5)) +
    theme_minimal()
  
  return(p)
}

# Calculate and plot for Precipitation
QA_obs_plot(cleaned_df2, "Precipitation")
QA_obs_plot(processed_df, "Tmean")

# Function to plot observation count by number of stations against years
# Include time variable because maybe we want to plot either by year or date, faster by Year
QA_heatmap <- function(df, variable_name, min_year, max_year) {
  # Filter out rows with missing variable values
  df_filtered <- df %>%
    filter(!is.na(.data[[variable_name]]),
           Year > min_year,
           Year < max_year)
  
  # Give numeric value to Station_ID
  df_with_station_number <- df_filtered %>%
    distinct(Station_ID) %>%
    dplyr::mutate(Station_Number = row_number())
  
  # Join Station numbers back to the original dataframe
  df_filtered <- df_filtered %>%
    left_join(df_with_station_number, by = "Station_ID")
  
  # Group data by Station_Number and calculate recording period
  station_recording_periods <- df_filtered %>%
    group_by(Station_Number) %>%
    dplyr::reframe(Start_Date = min(Year), End_Date = max(Year))
 
  # Plot recording periods
  p <- ggplot(station_recording_periods, aes(x = Start_Date, xend = End_Date, y = Station_Number)) +
    geom_segment(linewidth = 1, color = "dodgerblue3", alpha = 0.5) +
    labs(title = paste( "Recording Periods of", variable_name, "Weather Stations"),
         x = "Year",
         y = "Station Number") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  return(p)
}

# Call the function to create the plot
QA_heatmap(cleaned_df2,"Precipitation",1990,2024)
QA_heatmap(processed_df,"Tmean",1990,2024)

### Large data gaps####
#QA_gaps_shortlist#
#_______________________________________________________________________________________
QA_gaps_shortlist <- function(df, variable, threshold_days) {
  # Initialize an empty dataframe to store gaps
  gaps <- data.frame(Station_ID = character(), 
                     Start_Date = character(),
                     End_Date = character(),
                     YYYYMMdate = as.Date(character()),
                     Days_gaps = integer(),
                     stringsAsFactors = FALSE)
  
  # Iterate over unique station names
  unique_stations <- unique(df$Station_ID)
  for (Station_ID in unique_stations) {
    # Subset data for the current station
    subset_data <- df[df$Station_ID == Station_ID, ]
    
    # Filter data to specific variable
    subset_data <- subset_data %>% filter(!is.na(.data[[variable]]))
    
    # Check if there are valid dates for the variable
    if (any(!is.na(subset_data$YYYYMMdate))) {
      subset_data$YYYYMMdate <- as.Date(subset_data$YYYYMMdate)
      
      #Arrange so it count from the first date of data
      subset_data <- arrange(subset_data, YYYYMMdate)
      
      # Calculate daily differences
      subset_data$Days_gaps <- c(0, diff(subset_data$YYYYMMdate))
      
      # Find gaps larger than threshold 
      gaps <- rbind(gaps, subset_data[subset_data$Days_gaps > threshold_days, c("Station_ID", "Start_Date", "End_Date","YYYYMMdate","Days_gaps")])
    }
  }
  
  return(gaps)
}

# Calculate gaps larger than 8 months
gaps_series <- QA_gaps_shortlist(cleaned_df2, "Precipitation", 250)

#Export report
export_report(gaps_series,  "gaps_series_report.txt", "gaps_series_report.xlsx")

#_______________________________________________________________________________________
#QA_gaps_plot#
#_______________________________________________________________________________________
gaps <-cleaned_df2[cleaned_df2$Station_ID %in% unique(gaps_series$Station_ID), ]

## One way to plot all of them together
QA_heatmap(gaps,"Precipitation",2015,2024)

## Second way to plot individually
QA_plot_gaps <- function(df, variable_name) {
  # Filter out rows with missing variable values
  df_filtered <- df %>%
    filter(!is.na(.data[[variable_name]]))
  
  # Group data by Station_Number and calculate recording period
  station_recording_periods <- df_filtered %>%
    group_by(Station_ID) %>%
    summarise(Start_Date = min(Year), End_Date = max(Year))
  
  # Loop through each station and create individual heatmaps
  heatmap_plots <- list()
  for (station_id in station_recording_periods$Station_ID) {
    station_data <- df_filtered[df_filtered$Station_ID == station_id, ]
    
    # Create heatmap for the current station
    heatmap_plot <- ggplot(station_data, aes(x = Month , y =Year , fill = .data[[variable_name]])) +
      geom_tile() +
      scale_fill_gradient(low = "lightblue3", high = "dodgerblue4") +
      labs(title = paste("Time series of", variable_name, "for Station", station_id),
           y = "Year",
           x = "Month",
           fill = variable_name) +
      theme_minimal()
    
    heatmap_plots[[station_id]] <- heatmap_plot
  }
  
  return(heatmap_plots)
}

##Subset to the stations with gaps
gaps_df <- cleaned_df2[cleaned_df2$Station_ID %in% unique(gaps_series$Station_ID), ]

##Plot list
gap_plots_list <- QA_plot_gaps(gaps_df, "Precipitation")

# Print individual heatmaps
for (i in 1:length(gap_plots_list)) {
  print(gap_plots_list[[i]])
}

#_______________________________________________________________________________________
#QA_gaps_clean#
#_______________________________________________________________________________________
QA_gaps_clean <- function(df, stations_to_remove) {
  # Filter out rows corresponding to the specified stations
  cleaned_df <- df[!df$Station_ID %in% stations_to_remove, ]
  
  return(cleaned_df)
}

#Calculate
stations_to_removeP <- c("303","9660","307")

#Calculate df with short series
cleaned_df3 <- QA_gaps_clean(cleaned_df2,stations_to_removeP)

# Create a data set containing removed values
removed_values <- anti_join(cleaned_df2, cleaned_df3)

#_______________________________________________________________________________________
### Filtering by observation before 2000 and after 2000 #####
##### and removal of Stations which ALL values of the variable are NA ##
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
cleaned_df4 <- QA_yearly_filtering(cleaned_df3, Precipitation,  2000, 5, 1)
tempe_filtered  <- QA_yearly_filtering(processed_df,  Tmean, 2000, 5, 1)

# Create a data set containing removed values
removed_valuesP <- anti_join(cleaned_df3,cleaned_df4)
removed_valuesT <- anti_join(processed_df, tempe_filtered)

# Check removed values
removed_valuesP
removed_valuesT


#_______________________________________________________________________________________
### Removal of extreme values ####
# Detection and removal physically impossible observations #
# Check range of values to detect extremes 
# Precipitation:: Upper mean monthly threshold for 1500mm and negative values ##
# Temperature:: Upper mean monthly threshold for 50 and negative values #
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
prec_statistics <- QA_outlier_variation(cleaned_df3, Precipitation)
tempe_statistics <- QA_outlier_variation(tempe_filtered, Tmean)

###Extreme values###

### Function to delete extreme values of precipitation ##
QA_outlier_precipitation <- function(df, variable,threshold) {
  # Filter extreme precipitation values
  filtered <- df %>%
    filter(is.na({{ variable }}) | ({{ variable }} >= 0 & {{ variable }} <= threshold))
  return(filtered)
}

# Calculate for Precipitation
cleaned_df5 <- QA_outlier_precipitation (cleaned_df4 ,Precipitation,1500)

# Create a data set containing removed values and Plot
removed_valuesP <- anti_join(cleaned_df4, cleaned_df5)

export_report(removed_valuesP,  "Extreme_precipitation_values.txt", "Extreme_precipitation_values.xlsx")

### Function to delete extreme values of temperature ##
QA_outlier_temperature <- function(df, Tmean_variable, Tmax_variable = NULL, Tmin_variable = NULL, temp_min, temp_max) {
  # Check if Tmax_variable and Tmin_variable are provided
  if (is.null(Tmax_variable) && is.null(Tmin_variable)) {
    # Filter only based on Tmean_variable
    filtered_temperature <- df %>%
      filter(
        is.na(.data[[Tmean_variable]]) | 
          (.data[[Tmean_variable]] > temp_min & .data[[Tmean_variable]] < temp_max)
      )
  } else {
    # Filter based on all three variables
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
  }
  
  return(filtered_temperature)
}

# Calculate for Temperature
tempe_filtered2 <- QA_outlier_temperature(tempe_filtered ,"Tmean", "Tmax","Tmin",-15,40)

# Create a data set containing removed values
removed_valuesT <- anti_join(tempe_filtered, tempe_filtered2)
export_report(removed_valuesT,  "Extreme_temperature_values.txt", "Extreme_temperature_values.xlsx")


# Calculate summary statistics for filtered precipitation and temperature
prec_statistics_filtered <- QA_outlier_variation(cleaned_df5, Precipitation)
tempe_statistics_filtered <- QA_outlier_variation(tempe_filtered2, Tmean)

#_______________________________________________________________________________________
### QA_reliable_stations ####
#### Selection of Precipitation stations with reliable data # 
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
prec_stations <- QA_reliable_stations(cleaned_df5, Precipitation,  1990,  240)
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
prec_points <- st_as_sf(prec_selected_points, coords = c( "Longitude","Latitude"), crs = 25830)
temp_points <- st_as_sf(temp_selected_points, coords = c("Longitude","Latitude"), crs = 25830)

# Plot the polygon and random points
plot(StudyArea$geometry)
plot(st_geometry(prec_points),add = TRUE,  pch = 19, col = "red4")
plot(st_geometry(temp_points), add = TRUE, pch = 19, col = "blue4")

# Generate plots for reliable stations
QA_plot_yearly(cleaned_df5,"Precipitation",prec_selected_points)
QA_plot_yearly(tempe_filtered2,"Tmean",temp_selected_points)

# Generate plots for reliable stations
QA_plot_yearly_meansd(cleaned_df5,"Precipitation",prec_selected_points)
QA_plot_yearly_meansd(tempe_filtered2,"Tmean",temp_selected_points)

## Range for temperature max 5 degrees for one stations and for many 10
## Range for precipitation around 200 and max 4SD
## General trend
#___________________________________________________________________________________________________________________________________________
### Outliers shortlists #####
### QA_outlier_shortlist#
#___________________________________________________________________________________________________________________________________________# Select a specific station with many observations
QA_outlier_shortlist <- function(df_statistics, Range_threshold) {
  # Filter out stations, which monthly range exceeds 10 degrees Celsius
  outliers <- df_statistics %>%
    filter(Range >= {{Range_threshold}})%>%
    select(Station_ID,Month,Range) %>%
    distinct()
  
  return(outliers)
}

#Calculate
outliersT <- QA_outlier_shortlist(tempe_statistics_filtered,10)
outliersP <- QA_outlier_shortlist(prec_statistics_filtered,500)

##Export results in a report
export_report(outliersT,  "Outliers_temperature_report.txt", "Outliers_temperature_report.xlsx")
export_report(outliersP,  "Outliers_precipitation_report.txt", "Outliers_precipitation_report.xlsx")

#___________________________________________________________________________________________________________________________________________
### QA_outlier_plots#
#___________________________________________________________________________________________________________________________________________# Select a specific station with many observations
## Function to buffer around each suspicious station
# Distance in meters
#_____QA_buffer_stations_______
QA_buffer_stations  <- function(outlier_stations_sf, all_stations, variable, initial_buffer_distance) {
  
  # Convert all_stations to sf object
  all_stations <- all_stations %>% 
    select(Station_ID, Longitude, Latitude) %>%
    distinct()
  
  # Transform to sf obj
  all_stations_sf <- st_as_sf(all_stations, coords = c("Longitude", "Latitude"), crs = 25830)
  
  # Get unique outlier station IDs
  unique_outlier_ids <- unique(outlier_stations_sf$Station_ID)
  
  # Create an empty list to store stations within buffer for each outlier station
  stations_within_buffer <- vector("list", length = length(unique_outlier_ids))
  
  # Iterate over each unique outlier station ID
  for (i in seq_along(unique_outlier_ids)) {
    outlier_id <- unique_outlier_ids[i]
    outlier_station <- all_stations_sf[all_stations_sf$Station_ID == outlier_id, ] 
    buffer_distance <- initial_buffer_distance
    
    # Repeat until at least 3 stations are found within the buffer
    while (TRUE) {
      # Create a buffer around the outlier station
      outlier_buffer <- st_buffer(outlier_station, buffer_distance)
      
      # Find stations that intersect with the buffer
      stations_within <- all_stations_sf[st_intersection(all_stations_sf, outlier_buffer), ]
      
      # Add the list of stations within buffer to the result list
      stations_within_buffer[[i]] <- stations_within
      
      # Check if at least 3 stations are found within the buffer
      if (nrow(stations_within) >= 3) {
        break  # Exit the loop
      } else {
        # Increase the buffer distance
        buffer_distance <- buffer_distance + 500  # Increase by 500 m
      }
    }
  }
  
  return(stations_within_buffer)
}

#Calculate temperature
stations_per_bufferT<- QA_buffer_stations(outliersT,tempe_filtered2,"Tmean",500) 
#Calculate precipitation
stations_per_bufferP<- QA_buffer_stations(outliersP,cleaned_df5,"Precipitation",2000)

## Function to plot for outlier month only
QA_plot_outlier <- function(df, variable, selected_points, month) {
  # Filter df for shortlisted stations and the specific month
  selected_df <- df[df$Station_ID %in% unique(selected_points$Station_ID) & df$Month == month, ]
  
  # Get unique stations with their altitudes
  selected_df$StationID_Altitude <- paste(selected_df$Station_ID, ",", selected_df$Altitude, "m")
  
  # Plot time series for each selected station
  p <- ggplot(selected_df, aes(x = Year, y = .data[[variable]], color = StationID_Altitude)) +
    geom_point() +  # Plot points
    geom_line() +   # Connect points with lines
    labs(title = paste(variable,"for an outlier in the", month,"month, at Station",selected_df$Station_ID),
         x = "Year", y = variable,
         color = "StationID_Altitude") +
    theme_minimal()  # Minimal theme
  
  return(p)
}


# List to store plots for each buffer zone
# Temperature
buffer_plotsT <- list()
for (i in seq_along(stations_per_bufferT)) {
  # Generate plot for the current buffer zone
  plot <- QA_plot_outlier(tempe_filtered2, "Tmean", stations_per_bufferT[[i]], outliersT$Month[i])
  buffer_plotsT[[i]] <- plot
}

#Precipitaiton
buffer_plotsP <- list()
for (i in seq_along(stations_per_bufferP)) {
  # Generate plot for the current buffer zone
  plot <- QA_plot_yearly(cleaned_df5, "Precipitation", stations_per_bufferP[[i]], outliersP$Month[i])
  buffer_plotsP[[i]] <- plot
}

## Function to find stations with similar altitude for each suspicious station
# Altitude in meters
#___QA_altitude_stations_________

QA_altitude_stations <- function(df, outliers_df) {
  # Filter out rows with NA values in the Altitude column
  df_unique <- df[!is.na(df$Altitude), ]
  
  # Arrange by Altitude
  df_unique <- df_unique %>%
    select("Station_ID", "Longitude", "Latitude", "Altitude") %>%
    distinct() %>%
    arrange(Altitude)
  
  # Dataframe with unique outliers Stations
  outlier_stations <- unique(outliers_df$Station_ID)
  
  # Create empty list
  station_results <- list()
  
  # Iterate over each outlier station
  for (station_id in outlier_stations) {
    # Find the index of the outlier station
    outlier_index <- which(df_unique$Station_ID == station_id)
    
    # If the outlier station is the first or last station, set above and below stations to NA
    if (outlier_index == 1) {
      above_station <- NA
      below_station <- df_unique$Station_ID[outlier_index + 1]
    } else if (outlier_index == nrow(df_unique)) {
      above_station <- df_unique$Station_ID[outlier_index - 1]
      below_station <- NA
    } else {
      # Otherwise, set the above and below stations
      above_station <- df_unique$Station_ID[outlier_index - 1]
      below_station <- df_unique$Station_ID[outlier_index + 1]
    }
    # Match with info from original df
    above_station_df <- df_unique[df_unique$Station_ID == above_station,]
    below_station_df <- df_unique[df_unique$Station_ID == below_station,]
    outlier_station_df <- df_unique[df_unique$Station_ID == station_id,]
    # Store the results in the list
    result <- data.frame(
      Station_ID = c(above_station_df$Station_ID, outlier_station_df$Station_ID, below_station_df$Station_ID),
      Longitude = c(above_station_df$Longitude, outlier_station_df$Longitude, below_station_df$Longitude),
      Latitude = c(above_station_df$Latitude, outlier_station_df$Latitude, below_station_df$Latitude),
      Altitude = c(above_station_df$Altitude, outlier_station_df$Altitude, below_station_df$Altitude)
    )
    rownames(result) <- NULL  # Reset row names
    station_results[[station_id]] <- result
  }
  
  return(station_results)
}

# Call the function with your dataframes
stations_per_altitP <- QA_altitude_stations(cleaned_df5, outliersP)

# List to store plots for each outlier
altitude_plotsP <- list()
for (i in seq_along(stations_per_altitP)) {
  # Generate plot for the current buffer zone
  plot <- QA_plot_outlier(prec_filtered2, "Precipitation", stations_per_altitP[[i]], outliersP$Month[i])
  altitude_plotsP[[i]] <- plot
}

#___________________________________________________________________________________________________________________________________________
### QA_outlier_clean#
#___________________________________________________________________________________________________________________________________________# Select a specific station with many observations

QA_outlier_clean <- function(df, outlier_station_ID, variable, outlier_date) {
  # Select measurements that are going to be removed
  outlier_data <- df[df$Station_ID == outlier_station_ID 
                     & df$YYYYMMdate == outlier_date, c("Station_ID", "YYYYMMdate")]
  
  # Set the values of the variable to NA
  df[df$Station_ID == outlier_station_ID & df$YYYYMMdate == outlier_date, variable] <- NA
  
  # Return the dataframe with removed outliers and the outlier data
  return(df)
    
}

# Clean the meteo dataframe
cleaned_df6 <- QA_outlier_clean(prec_filtered2, "1088C", "Precipitation", "2020-10-15")

