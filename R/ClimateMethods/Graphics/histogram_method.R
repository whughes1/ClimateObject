##############################################################################
# HISTOGRAM
#' @title Get histogram from a dataframe
#' @name histogram
#' @author Rafael Posada 2015 (SASSCAL/DWD)
# 
#' @description 
#' Allows plotting the data in a histogram plot.
#  
#' @return It returns a histogram plot.

climate$methods(histogram = function(data_list = list()){
  #######################################################################
  # CLIMATE DATA OBJS
  climate_data_objs = get_climate_data_objects(data_list)
  for(data_obj in climate_data_objs) {
    data_name = data_obj$get_meta(data_name_label)
    #####################################################################
    # FIND OUT WHICH VARIABLE TO PLOT
    # Rain
    var_label=list()
    if (data_obj$is_present(rain_label)){
      var_label[[rain_label]]=rain_label
    }
    # Minimum Temperature
    if (data_obj$is_present(temp_min_label)) {
      var_label[[temp_min_label]]=temp_min_label
    }
    # Maximum Temperature
    if (data_obj$is_present(temp_max_label)){
      var_label[[temp_max_label]]=temp_max_label
    }
    # Wind speed 
    if (data_obj$is_present(wind_speed_label)){
      var_label[[wind_speed_label]]=wind_speed_label
    }
    # Wind direction
    if (data_obj$is_present(wind_direction_label)) {
      var_label[[wind_direction_label]]=wind_direction_label
    }
    
    # Create one plot for each variable
    date_col = data_obj$getvname(date_label)
    station_id_col = data_obj$getvname(station_label)
    # Get the data frames for analysis
    curr_data_list = data_obj$get_data_for_analysis(data_list)
      
    for (i in var_label){
      # Get the variable 
      var_col = data_obj$getvname(as.character(i))
      # loop for computing 
      for( curr_data in curr_data_list ) {
        # Begin and end dates of dataset
        first.date <- curr_data[[date_col]][1]
        last.date <- curr_data[[date_col]][nrow(curr_data)]
        
        #define graphic working directory
        mainDir<-getwd()
        
        tperiod <- paste(first.date,"-",last.date, sep="")
        station_id <- unique(curr_data[[station_id_col]])
        
        y <- curr_data[[date_col]]
        x <- curr_data[[var_col]]

        #####################################################
        # GET THE MAX & MIN LIMITS
        n.std <- 4
        std.dev <- sd(x,na.rm=TRUE)
        mean <- mean(x,na.rm=TRUE)
        xaxis.max <- round(mean+(n.std*std.dev),digits=2)
        xaxis.min <- round(mean-(n.std*std.dev),digits=2)
        if (xaxis.min<0 & grepl("temp",var_col)==FALSE){
          xaxis.min=0
        }
        
        ####################################################
        # SEQUENCE OF THE HISTOGRAM
        # maximum value for the sequence
        seq.max <- max(x,na.rm=TRUE)
        # minimum value for the sequence
        seq.min <- min(x,na.rm=TRUE)
        # sequence
        seq.interval <- round((abs(seq.max)-abs(seq.min))/100,digits=0)
        if (seq.interval==0){
          seq.interval <- round((abs(seq.max)-abs(seq.min))/100,digits=1)
          if (seq.interval==0){
            seq.interval <- round((abs(seq.max)-abs(seq.min))/100,digits=2)
          }
        }
        sequence <- seq(from=seq.min,to=seq.max,by=seq.interval)
        
        
        #####################################################
        # REMOVE DATA ABOVE LIMIT
        # Remove the data from "x" that are above the limit in order
        # to be able to make a histogram. If there are outsiders,
        # the function "hist" cannot be used
        id00 <- which(x<max(sequence) & x>min(sequence))
        y3 <- y[id00]
        x3 <- x[id00]
        
        
        ####################################################################
        # PLOT - HISTOGRAM
        Sys.sleep(0.1) # this is to avoid an error that pops up when trying 
        # to plot two plots at a time (something to do with
        # rversion in grDevices)
        
        # x and y labels
        xlabel <- paste("Intervals [from ",seq.min," to ",
                        seq.max,"; by ",seq.interval,"]",sep="")
        ylabel <- "Number of cases"

        # Histogram
        hist(x3,breaks=sequence,xlim=c(seq.min,seq.max),
             main="",
             xlab=xlabel,
             ylab=ylabel,cex.lab=.8,
             cex.axis=.8)
        
        ###########################################################
        # TITLES AND LEGEND
        # Title
        tit<- paste("Station id: ",station_id," - ",var_col)
        subtit <- paste(first.date,"-",last.date)
        title(main=paste(tit," (",subtit,")"),cex.main=0.8,line =3)
        
        # Legend
        # Total number of cases for the given time inverval
        tt.total <- paste("Total Cases: ",length(x))
        # Number of cases above the maximum limit
        above.max <- which(x>xaxis.max)
        above.percent <- round((length(above.max)/length(x))*100,2)
        tt.above <- paste("Cases > ",
                          xaxis.max,": ",
                          length(above.max),
                          " (",above.percent,"%)",     
                          sep="")
        
        # Number of cases below the minimum limit
        below.min <- which(x<xaxis.min)
        below.percent <- round((length(below.min)/length(x))*100,2)
        tt.below <- paste("Cases < ",
                          xaxis.min,": ",
                          length(below.min),
                          " (",below.percent,"%)",
                          sep="")
        
        # Number of NA cases 
        na.cases <- which(is.na(x))
        na.percent <- round((length(na.cases)/length(x))*100,2)
        tt.na <- paste("'NA'-Cases: ",
                       length(na.cases)," (",na.percent,"%)",sep="")
        
        #Write down the legend 
        mtext(paste(tt.total,"\n",
                    tt.above,"\n",
                    tt.below,"\n",
                    tt.na),
              cex=.6*par("cex"),
              side=3,
              line=0)
        setwd(mainDir)
      }
    }
  }
})