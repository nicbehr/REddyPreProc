# ================================================
# ===============despiking function ==============
# ================================================
#hard flag removal is done outside the despiking function

despiking_bystdev_cnt <- function(flux,width=96,multiplier=3){    #with one month window we use lots of good data. use only 2 days (96 steps)
  
  # Purpose: This program detects spikes in half-hourly fluxes and removes them. 
  #          First monthly means and standard deviations are calculated (running mean) 
  #          and all values > mean+multiplier*stdev or < mean-multiplier*stdev are removed. This procedure 
  #          is repeated (iteration) until no outlier can be found anymore (using a while loop).
  # Input: flux = variable that should be despiked
  #        width = window with for rolling mean and rolling standard deviation (default = 1440; number of half hours per 30 days)
  #        multiplier = value defining by how many standard deviations off the mean a value is considered an outlier (default = 3)
  # Output: cleaned_flux: despiked input variable filling the first and (width/2) number of values by the first and last calculated value, respectively. NAs are ignored (na.rm=TRUE).
  #         spike_cnt: number of spikes that were removed.
  # Dependencies: Package caTools (install.packages("caTools"))
  # Refrence: Rogiers, N., Eugster, W., Furger, M., & Siegwolf, R. (2004). Effect of land management on ecosystem carbon fluxes at a subalpine grassland site in the Swiss Alps. Theoretical and Applied Climatology, 80(2-4), 187-203. http://doi.org/10.1007/s00704-004-0099-7
  
  library(caTools)
  
  cnt.org <- sum(is.finite(flux))       # needed to determine percentage that was removed compared to finite data
  cnt.org.all <- length(flux)   # needed to determine percentage that was removed compared to all hlaf hours
  i <-0                                 # needed to initially start the iteration
  len.lim.up  <-0                       # needed to initially start the iteration
  len.lim.low  <-0                      # needed to initially start the iteration
  spike_count <-0                       # needed to initially start the iteration
  while(len.lim.up>0 | len.lim.low>0 | i==0){
    i<-i+1                              # needed to initially start the iteration
    # Calculate running mean and standard deviation (30 days default)
    fluxmean <- runmean(flux, width, endrule="constant")
    fluxstdev <- runsd(flux, width, endrule="constant")
    
    # Remove spikes
    lim.up <- flux>fluxmean+multiplier*fluxstdev
    lim.low <- flux<fluxmean-multiplier*fluxstdev
    
    flux[lim.up] <- NA
    flux[lim.low] <- NA
    
    len.lim.up <- length(which(lim.up))
    len.lim.low <- length(which(lim.low))
    
    spike_count <- spike_count+len.lim.up+len.lim.low
  }
  
  cat(paste("  Percentage of data removed due to despiking (compared to all finite data):", format(spike_count/cnt.org*100, digits=4, trim=TRUE),"%\n"))
  cat(paste("  Percentage of data removed due to despiking (compared to all data - finite and infinite):", format(spike_count/cnt.org.all*100, digits=4, trim=TRUE),"%\n"))
  cat(paste("  Percentage of finite data after despiking (compared to all data - finite and infinite):", format(sum(is.finite(flux))/cnt.org.all*100, digits=4, trim=TRUE),"%\n"))
  output <- list(flux,spike_count)
  names(output) <-c("cleaned.flux","spike.count")
  return(output)
}

despiking_MAD_papaple_day_night_separate = function(df, fluxvar, z){
  # UNFINISHED - Work in progress
  # Purpose: This program detects spikes in half-hourly fluxes and removes them. 
  #          Spikes are detected for day and night times separately. The procedure
  #           is the MAD (median absolute deviation) method as used in the oneflux
  #           pipeline (Papaple et. al 2006)
  # Input: df = dataframe including the flux
  #        fluxvar = string with the column name of the variable to detect spikes in
  #       z = multiplier of the median deviation above which a point is detected as
  #           outlier
  # Output: cleaned_flux: despiked input variable filling the first and (width/2) number of values by the first and last calculated value, respectively. NAs are ignored (na.rm=TRUE).
  # Refrence: https://bg.copernicus.org/articles/3/571/2006/bg-3-571-2006.pdf
  
  isday = df$is_day
  despiked_flux = df
  day_flux = df[df$is_day,]
  night_flux = df[!df$is_day,]

  day_flux_outliers = detect_spikes_MAD_papaple(day_flux, fluxvar, z)
  night_flux_outliers = detect_spikes_MAD_papaple(night_flux, fluxvar, z)
  
  despiked_flux[despiked_flux$date_time %in% day_flux_outliers, fluxvar] = NA
  despiked_flux[despiked_flux$date_time %in% night_flux_outliers, fluxvar] = NA  
  
  return(despiked_flux[[fluxvar]])
}

detect_spikes_MAD_papaple = function(df, fluxvar, z){
  # This is an implementation of Dario Papales MAD based algorithm as in
  # https://bg.copernicus.org/articles/3/571/2006/bg-3-571-2006.pdf
  # for each datapoint the difference between the point and the points directly
  # before and after is calculated
  # The value is flagged as an outlier if the difference between the value
  # and its neighbors is more or less than median of the differences Md plus or minus
  # the median of the absolute difference minus the median of the differences
  fluxes = df[fluxvar]
  d = c()
  outliers = c()
  for(i in 1:nrow(fluxes)){
    flux = fluxes[i, fluxvar]
    if ((i == 1) || (i == nrow(fluxes))){
      d = append(d, 0)
    }else{
      di = (flux - fluxes[i-1, fluxvar]) - (fluxes[i+1, fluxvar] - flux)
      d = append(d, di)
    }
  }
  Md = median(d, na.rm=TRUE)
  MAD = median(abs(d-Md), na.rm=TRUE)
  df$outliers <- abs(d - Md) > (z * MAD / 0.6745)
  idc = which(df$outliers)
  return(df[idc,"date_time"])
}
