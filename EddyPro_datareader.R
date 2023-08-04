read_eddypro_output = function(root, string_identifier, datecol="date", timecol="time", skip=0){
  # This function reads all EddyPro output files in a folder, 
  # converts the timestamps to POSIXct, fills up the timeseries to an equidistant
  # half-hourly (or whatever the timesteps in the provided data are) dataset
  # and converts all columns except date_time, date, time and filename into numeric
  # format
  # Input:
  #       root: string, path to the folder to search for EddyPro output files
  #       string_identifier: string to identify desired files by (e.g. "full_output", "biomet" or "fluxnet")
  #       datecol: string, column which contains the dates, defaults to "date"
  #       timecol: string, column which contains the times, defaults to "time"
  #       skip: int, how many lines in the data file to skip (e.g. typically 1 for full_output files because it 
  #             has an extra header or 0 for biomet as the extra header is not present)
  
  files = get_files_with_searchstring(root, string_identifier)
  if(length(files) == 0){
    print(paste("No data found at path: ",root))
          return()
  }
  df_and_units = read_and_concat_csv_files(files, skip)
  df_and_units$df$date_time = combine_date_time_characters(df_and_units$df, datecol, timecol)
  df_and_units$df = complete_datetime_df(df_and_units$df, "date_time")
  df_and_units$df = convert_to_numeric_except(df_and_units$df, c("date_time", "date", "time", "filename"))
  return(df_and_units)
}

get_files_with_searchstring <- function(directory_path, search_string="full_output") {
  # List all files in the directory 
  # Input:
  #       directory_path: path of directory to search for files
  #       search_string: string identifiert, which files to output, default = "full_output"
  #                       which is an identifiert for full_output files of EddyPro,
  #                       can use e.g. "biomet" to read biomet output files
  all_files <- list.files(directory_path, full.names = TRUE)
  
  # Filter the files that contain the search string
  matching_files <- grep(search_string, all_files, value = TRUE, ignore.case = TRUE)
  
  return(matching_files)
}

read_and_concat_csv_files <- function(file_paths, skip) {
  # Takes a list of paths to csv files and reads them into dataframes, afterwards
  # concatenating them to produce one large dataframe containing the data with
  # correct header and one dataframe containing the units for each header in
  # the dataframe
  # Input: 
  #       file_paths: list of csv file paths, typically derived from function
  #                    get_file_with_string()
  #       skip: int, how many lines to skip while reading data, defaults to 1
  #             as in typical full_output from EddyPro the first line contains
  #             non-neccessary information for processing
  
  require(dplyr)  # Load the dplyr package for data manipulation
  
  # Initialize an empty list to store the data frames
  data_frames <- list()
  browser()
  # Loop through each file path in the provided list
  for (file_path in file_paths) {
    # Read the CSV file into a data frame
    df <- read.csv(file_path, skip=skip)
    units <- df[1,]
    
    units_dict <- setNames(as.list(units), colnames(df))
    df <- df[-1,]
    # Add the data frame to the list
    data_frames <- append(data_frames, list(df))
  }
  
  # Concatenate all data frames in the list into a single data frame
  concatenated_df <- bind_rows(data_frames)

  return(list("df"=concatenated_df, "units"=units_dict))
}

library(lubridate)

combine_date_time_characters <- function(data, date_col, time_col, datetime_format = "%Y-%m-%d %H:%M") {
  # combines the data and time columns in a typical EddyPro output file to a 
  # POISXct formatted date_time column
  
  # Combine date and time columns using paste
  combined_datetime <- paste(data[[date_col]], data[[time_col]])
  combined_datetime = as.POSIXct(combined_datetime, format="%Y-%m-%d %H:%M", tz="GMT")

  # Add the combined POSIXct column to the data frame
  data$datetime_combined <- combined_datetime
  
  return(combined_datetime)
}

complete_datetime_df <- function(data, datetime_col) {
  # checks the date_time column in the dataframe containing the EddyPro 
  # data and fills the dataframe to contain a full half-hourly equidistant timestep
  # dataset
  
  # Load the tidyr package
  require(tidyr)
  # Generate a sequence of complete datetime values with the same time step as the original data
  complete_datetime <- seq(from = min(data[[datetime_col]], na.rm=TRUE), to = max(data[[datetime_col]], na.rm=TRUE), by = diff(data[[datetime_col]])[1])
  
  # Convert the complete_datetime to a data frame
  complete_df <- data.frame(datetime = complete_datetime)
  colnames(complete_df) <- datetime_col
  # Left join the original data frame with the complete data frame
  result_df <- left_join(complete_df, data, by = datetime_col)
  
  return(result_df)
}

convert_to_numeric_except <- function(data, except_cols) {
  # As often the original data is read in as characters, this function
  # converts all columns except specified ones to numeric format
  # Input:
  #         data: dataframe containing the EddyPro data
  #         except_cols: cols not to convert to numeric format
  # Identify the columns to be converted to numeric
  cols_to_convert <- setdiff(names(data), except_cols)
  
  # Convert the selected columns to numeric
  data[cols_to_convert] <- lapply(data[cols_to_convert], as.numeric)
  
  return(data)
}