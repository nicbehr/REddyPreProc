## Author: Nicolas Behrens
## Contains functions for plotting of processed EC data

library(ggplot2)
library(scales)
library(grid)
library(ggrepel)

# Plots two datasets with OLS regression line and r² coefficient
plot_correlation = function(data, x, y){
  
  model = lm(data[,x] ~ data[,y])
  model_stat = summary(model)
  r2 = model_stat$r.squared
  r2_plot = round(r2, digits=3)
  # Create a text
  grob <- grobTree(textGrob("Test", x=0.5,  y=.5, hjust=0,
                            gp=gpar(col="black", fontsize=13, fontface="italic")))
  # Plot
  
  p = ggplot(data,aes_string(x, y)) +
    coord_cartesian(expand = FALSE)+
    geom_point(size = .8)+
    #stat_summary(fun.data= mean_cl_normal) + 
    geom_smooth(method='lm', color="red") +
    annotate("label", x = Inf, y = Inf, label=paste('rsq: ', r2_plot), vjust = 2, hjust = 2, fill="grey")+
    theme_bw()
  p + annotation_custom(grob)
  
  ggsave(paste('./plots/correlations/corr_',x,'_',y,'.png'), p)
  # Pearson correlation
  #text(paste("Correlation:", round(cor(x, y), 2)), x = 25, y = 95)
  
}

## plots continuous fingerprint of the dataset for a given variable
plot_fingerprint = function(df, var, unit,output_folder){
  
  df$date <- as.Date(df$date_time)
  df$year <- as.numeric(format(df$date_time,"%Y"))
  df$month <- as.numeric(format(df$date_time,"%m"))
  df$hour <- as.numeric(format(df$date_time,"%H"))
  df$DOY  <- format(df$date_time, "%j")   #dont put as.numeric here because you need the Julian date in 3 digits (001 instead of 1)
  
  df$yearday = as.numeric(paste(df$year, df$DOY , sep="")) 

  # colnames(df)= c("hour", "day","yearday","NEE")
  
  df$label= as.character(df$yearday)   #needs to be a discrete axis - hence convert to string
  
    z = ggplot(df, aes(hour, date))   
    z = z + geom_raster(aes_string(fill=var), interpolate=FALSE)+ 
      scale_fill_gradientn(name=paste(var," [", unit,"]", sep=""),
                           colours=colorRampPalette(c("#00007F", 
                                                      "blue", "#007FFF", "cyan", "#7FFF7F", 
                                                      "yellow", "#FF7F00", "red", "#7F0000"))(50)) +
      xlab("Hour of day") + ylab("Date")+
      xlim(0,24)+
      coord_cartesian(expand = FALSE)+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position="bottom")+
      scale_x_continuous(breaks = seq(0, 24, by = 1)) + scale_y_date(date_labels = "%y-%m-%d", date_breaks = '1 week')
    z
    ggsave(paste(output_folder,'fingerprint\\',var, "_fingerprint.png"), z, height = 10 , width = 7)
    
}