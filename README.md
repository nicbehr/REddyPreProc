# REddyPreProc
A package of convenience functions to read EddyPro output files into R. It makes the process of reading the output csv files into R to further process them with REddyProC easier. I deemed it useful as many people just starting out working with EddyCovariance and having to work with REddyProC don't have much experience with R and exercises such as formatting timestamps etc. can be tedious in the beginning, it is however very much recommended to look through the code and understand it. It is written in very simple code and separeted into several functions for the single steps of processing, which makes it rather easy to read. A general understanding of such processes is neccessary, to process the data to ones own needs and being able to customize your date pipeline.

This package includes steps for 
- timestamp formatting
- removal of quality flagged data
- absolute limits and outlier removal
- timeseries slicing and manipulation and more

# Modules
## EddyPro_datareader
simple convenience function to read EddyPro output data into R as dataframe  
- gets all files containing a searchstring in a specified folder, e.g. all files containing "full_output" in the output folder of EddyPro
- merges the "date" and "time" columns into a date_time column in POSIXct format
- fills the timeseries missing timestamps with NA values to create a full equidistant dataframe
- converts all columns except for "date", "time", "date_time" and "filename" into numeric format

## EC_filtering
some functions for filtering the data such as removing a certain portion of the data (or getting a certain timeslice of the data), removing absolute limits from a column, removing absolute limits separately for day and night time (based on the package "photobiology" implementation of day/night cycles (https://www.rdocumentation.org/packages/photobiology/versions/0.10.17/topics/day_night), removing quality flagged data

## EC_despiking
contains some despiking functions typically used for EddyCovariance.  
Implemented methods are 
- spike removal based on distance from the mean in number of standard deviations in a running window
- Median absolute deviation (MAD) based removal of spikes as in the oneflux pipeline (work in progress)


## Example
```{r}
root = "<path_to_your_data>"
sitename = "<your_sitename>"
lat = <lat> # required for removal of day/night time absolute limits separetly
long = <long> # required for removal of day/night time absolute limits separetly

# First loading the data and doing some early preparation:
source("<path_toscript>\\EddyPro_datareader.r")
data_and_units = read_eddypro_output(root, "full_output", "date", "time", skip=1)
data = data_and_units$df

biometdata_and_units = read_eddypro_output(root, "biomet", "date", "time", skip=0)
biometdata = biometdata_and_units$df
biometdata = biometdata[biometdata$date_time %in% data$date_time,]

# Time slice removal, absolute
source("<path_toscript>\\EC_filtering.r")

# remove unwanted data e.g. when it was tested in the lab or during setup of the station
# providing no enddate gets everything to the end of the available data
data = get_data_slice(data, startdate = "2022-10-01 00:30:00", enddate="")

# for example fix first 2 weeks of wrong time format (subtract 1 hour bc. GMT+2 was active for DST, then it was changed)
# providing no startdate lets it start at the first available datapoint
data = change_datetime_in_slice(data, startdate="", enddate='2022-10-17 11:00:00', seconds_to_add = 60*60)

# remove qc 2 data
data = remove_qc(data, c(2))

# remove hard limits for day and night separately
data = remove_absolute_limits_day_night(data, "co2_flux", 40, -40, 30, -5, lat, lon)
data = remove_absolute_limits_day_night(data, "LE", 800, -40, 107, -52, lat, lon)

# despike
source("<path_toscript>\\EC_despiking.R")
data$co2_flux_despiked_stdve_3days =  unname(unlist(despiking_bystdev_cnt(data$co2_flux, width=3*48, multiplier = 4)[1]))
data$co2_flux_despiked_stdve_9days =  unname(unlist(despiking_bystdev_cnt(data$co2_flux, width=9*48, multiplier = 4)[1]))
data$co2_flux_despiked_stdve_14days =  unname(unlist(despiking_bystdev_cnt(data$co2_flux, width=14*48, multiplier = 4)[1]))
data$co2_flux_despiked_stdve_30days =  unname(unlist(despiking_bystdev_cnt(data$co2_flux, width=30*48, multiplier = 4)[1]))
```
