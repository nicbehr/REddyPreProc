remove_absolute_limits = function(df, column, upper_limit, lower_limit){
  # removes absolute limits (exclusive) from dataframe
  # Values are replaced by NA
  
  if(column %not in% colnames(df)){
    print(paste(column, " not found in dataframe"))
    return()
  }
  df[df[column] > upper_limit] = NA
  df[df[column] < lower_limit] = NA
  return(df)
}


remove_absolute_limits_day_night = function(df, column, upper_limit_day, lower_limit_day, upper_limit_night, lower_limit_night, lat, lon){
  # removes absolute limits in a specified column by replacing with NA
  # day/night is computed based on latitude and longitude, the date_time column and a specified
  # timezone (make sure to set the right timezone (TODO!))
  # Input:
  #       df: dataframe containing the specified column and a date_time column
  #       column: column to remove data from
  #       upper_limit_day: upper limit above which data is replace with NA during day time
  #       lower_limit_day: lower limit below which data is replace with NA during day time
  #       upper_limit_night: upper limit above which data is replace with NA during night time
  #       lower_limit_night: lower limit below which data is replace with NA during night time
  #       lat: latitude of the station in decimal degrees
  #       lon: longitude of the station in decimal degrees
  # Output:
  #       df: dataframe containing all of the input dataframe but values below/above
  #           the threshold in the specified column replaced  with NA
  if(!column %in% colnames(df)){
    print(paste(column, " not found in dataframe"))
    return()
  }
  df["is_day"] = photobiology::is_daytime(date=df$date_time, tz="GMT", geocode = tibble::tibble(lat=lat, lon=lon))
  df[df["is_day"] == TRUE & df[column] > upper_limit_day & !is.na(df[column]), column] = NA
  df[df["is_day"] == TRUE & df[column] < lower_limit_day & !is.na(df[column]), column] = NA
  
  df[df["is_day"] == FALSE & df[column] > upper_limit_night & !is.na(df[column]), column] = NA
  df[df["is_day"] == FALSE & df[column] < lower_limit_night & !is.na(df[column]), column] = NA

  return(df)
}

remove_partial_days = function(df){
  #remove first day to get full days
  last_day = tail(unique(as.Date(df$date_time)), n=1)
  if(nrow(df[as.Date(df$date_time) == last_day,]) < 48){
    midnight_row = head(df[as.Date(df$date_time) == last_day,], n=1)
    df = subset(df, as.Date(df$date_time) != last_day)
    df = rbind(df, midnight_row)
  }
  
  #remove last day to get full days
  first_day = head(unique(as.Date(df$date_time)), n=1)
  if(nrow(df[as.Date(df$date_time) == first_day,]) < 48){
    df = subset(df, as.Date(df$date_time) != first_day)
    df = df[-c(1),]
  }
}


get_data_slice = function(dataframe, startdate, enddate){
  # Gets a slice of data between certain timestamps from the dataset
  # depending on two datetimes (inclusively, meaning defined datetimes are included)
  # if no startdate or enddate is specified, removes slice from specified date to
  # start/end
  
  if(startdate == ""){
    startdate = dataframe$date_time[1]
  }
  if(enddate == ""){
    enddate = tail(data$date_time, n=1)
  }
  
  dataframe_sliced <- dataframe[dataframe$date_time >= startdate,]
  dataframe_sliced <- dataframe[dataframe$date_time <= enddate,]
  
  return(dataframe_sliced)
}

change_datetime_in_slice = function(dataframe, startdate, enddate, seconds_to_add){
  # adds or subtracts (if negative values are passed for seconds_to_add) seconds
  # to/from the date_time column in a dataframe
  if(startdate == ""){
    startdate = dataframe$date_time[1]
  }
  if(enddate == ""){
    enddate = tail(data$date_time, n=1)
  }
  data %>% 
  filter(date_time >= startdate & date_time <= enddate) %>% 
  mutate(date_time = date_time + seconds_to_add)
  return(data)
}


# Removes fields for LE, H, CO2 and H2O for given flags
# argument flags must be vector of e.g. c(1,2)
remove_qc = function(df, flags){
# TODO: Make variables to remove argument (needs to split to get qc_...)
  for (flag in flags){
    df$NEE[df$qc_co2_flux == flag] = NA
    df$H[df$qc_H == flag] = NA
    df$LE[df$qc_LE == flag] = NA
    df$h2o_flux[df$qc_h2o_flux == flag] = NA
  }
  return(df)
}