# Suite of functions to create met input dfs for Qual2Kw; parameters include:
# air temp, dew point T, cloud cover, wind speed, and direct solar radition.
# Function for writing to csv file as well.

# __________________________________________________________________________----
# Temperature - Dry Bulb ----
t_air_q2k <- function(strD = NULL, endD = NULL, shp = NULL, dir = NULL,
                      tmxt = NULL, tmnt = NULL) {
  
  # Function to create an hourly time series of air temperature data for each
  # Qual-2Kw reach using gridded daily PRISM max/min air temperature data
  # interpolates a sinusoidal time series based on specific timing of max/min
  # temperatures. Arguements:
  # strD = start date of desired time series data
  # endD = end date of desired time series data
  # shp = shape file of basins--specify full path name, not the object itself
  # dir = directory of the prism data
  # tmxt = vector of time of maximum temperature (using minmax_time())
  # tmnt = vector of time of minimum temperature (using minmax_time())

  library(raster); library(dplyr)
  
  # Load the shapefile
  shp <- shapefile(shp)
  
  # Create time series vectors for both 
  dtes <- seq(as.POSIXct(strD, '%Y-%m-%d', tz = 'America/Los_Angeles'),
              as.POSIXct(endD, '%Y-%m-%d', tz = 'America/Los_Angeles'), 86400)
  
  # List the folders and files for min/max T
  if (substr(dir, nchar(dir), nchar(dir)) != '/') {paste0(dir, '/')}
  
  # List grids for interrogation
  rfmn <- paste0(dir, 'tmin/Oregon_tmin_', format(dtes, '%Y%m%d'), '.asc')
  
  rfmx <- paste0(dir, 'tmax/Oregon_tmax_', format(dtes, '%Y%m%d'), '.asc')
  
  mnmx <- list(min = data.frame(matrix(ncol = length(shp), nrow = 0)),
               max = data.frame(matrix(ncol = length(shp), nrow = 0)))
  
  for (i in 1 : length(rfmn)) {
    
    rast <- list(min = raster(rfmn[i]), max = raster(rfmx[i]))
    
    for (j in 1 : 2) {mnmx[[j]] = rbind(mnmx[[j]], extract(rast[[j]], shp))}
    
  }
  
  names(mnmx[[1]]) <- names(mnmx[[2]]) <- paste0('B', 1 : length(mnmx[[1]]))
  
  # Set up the data frame for the time series data
  dtes <- seq(as.POSIXct(strD, '%Y-%m-%d', tz = 'America/Los_Angeles'),
              as.POSIXct(endD, '%Y-%m-%d', tz = 'America/Los_Angeles'), 3600)
  
  airT <- data.frame(dates = dtes, matrix(ncol = length(shp), nrow = length(dtes)))
  
  for (i in 1 : length(tmxt)) {
    
    # Special case where initial temp is not defined
    if (i == 1) {
      
      temp <- airT[which(airT$dates <= tmnt[i]), ]
      
      for (j in 2 : length(temp)) {
        
        # Calculate a sinusoidal time series between the daily max/min
        temp[, j] <- calc_sin(mnmx[['min']][i, j - 1], mnmx[['min']][i, j - 1], temp$dates)
        
      }
      
      airT[which(airT$dates <= tmnt[i]), ] <- temp
      
    }
    
    # Calculate the AM to PM time series
    temp <- airT[which(airT$dates >= tmnt[i] & airT$dates <= tmxt[i]), ]
    
    for (j in 2 : length(temp)) {
      
      temp[, j] <- calc_sin(mnmx[['max']][i, j - 1], mnmx[['min']][i, j - 1],
                            temp$dates)
      
    }
    
    airT[which(airT$dates >= tmnt[i] & airT$dates <= tmxt[i]), ] <- temp
    
    # Calculate the PM to AM time series
    if (i != length(tmxt)) {
      
      temp <- airT[which(airT$dates >= tmxt[i] & airT$dates <= tmnt[i + 1]), ]
      
      for (j in 2 : length(temp)) {
        
        temp[, j] <- calc_sin(mnmx[['min']][i + 1, j - 1],
                              mnmx[['max']][i, j - 1], temp$dates)
        
      }
      
      airT[which(airT$dates >= tmxt[i] & airT$dates <= tmnt[i + 1]), ] <- temp
      
    } else { # Special case where last temp not defined
      
      temp <- airT[which(airT$dates >= tmxt[i]), ]
      
      for (j in 2 : length(temp)) {
        
        temp[, j] <- calc_sin(mnmx[['max']][i, j - 1], mnmx[['max']][i, j - 1],
                              temp$dates)
        
      }
      
      airT[which(airT$dates >= tmxt[i]), ] <- temp
      
    }
  }

  # Remove the indirect input columns
  airT <- airT[, c(1, 4, 7, 8, 9, 12, 13, 14, 15, 16, 17)]
  
  row.names(airT) <- airT$date; airT <- airT[, -1]; airT <- data.frame(t(airT))
  
  return(airT)
  
}

# __________________________________________________________________________----
# Temperature - Dew Point ----
t_dwpnt_q2k <- function(dpts = NULL, strD = NULL, endD = NULL, shp = NULL,
                        dir = NULL, tmxt = NULL, tmnt = NULL) {
  
  # Function to create an hourly time series of dewpoint temperature data 4 each
  # Qual-2Kw reach using gridded daily PRISM mean dewpoint temperature data
  # interpolates a sinusoidal time series based on specific timing of max/min
  # temperatures. Arguements:
  # dpts = dew point time series to be adjusted per basin
  # strD = start date of desired time series data
  # endD = end date of desired time series data
  # shp = shape file of basins--specify full path name, not the object itself
  # dir = directory of the prism data
  # tmxt = vector of time of maximum dewpoint temperature (using minmax_time())
  # tmnt = vector of time of minimum dewpoint temperature (using minmax_time())

  library(raster); library(dplyr)
  
  # Load the shapefile
  shp <- shapefile(shp)
  
  # Create time series vectors for both 
  dtes <- seq(as.POSIXct(strD, '%Y-%m-%d', tz = 'America/Los_Angeles'),
              as.POSIXct(endD, '%Y-%m-%d', tz = 'America/Los_Angeles'), 86400)
  
  # Need to shift the dtes forward for the PRISM interrogation because mean daily 
  # PRISM data for any given day are defined for the period ending at 7:00AM on
  # that day, rather than at the end of the day! So...to get the approximate mean
  # mean daily PRISM data for a given day, go get the PRISM data for the previous day.
  dte2 <- dtes + 86400
  
  # List the folders and files for min/max T
  if (substr(dir, nchar(dir), nchar(dir)) != '/') {dir <- paste0(dir, '/')}
  
  for (i in unique(year(dtes))) {
    # Use the shifted PRISM dates (dte2)
    rfdp <- paste0(dir, 'yr_', i, '/PRISM_tdmean_stable_4kmD1_',
                   format(dte2, '%Y%m%d'), '_bil.asc')  
  }
  
  # Set up data frame shell for the basin specific mean dewpoint T (with correct dates)
  bmdp <- data.frame(date = dtes, matrix(data = 0, ncol = length(shp), nrow = length(dtes)))
  
  # Populate data frame
  for (i in 1 : length(rfdp)) {
    
    rast <- raster(rfdp[i])
    
    bmdp[i, 2 : (length(shp) + 1)] = extract(rast, shp)
    
  }
  
  # Rename columns
  names(bmdp) <- c('date', paste0('B', 1 : length(shp)))
  
  # Rename columns of dewpoint time series
  names(dpts) <- c('date', 'tdpC')
  
  # 1) Calculate mean daily T_dwp for the dewpoint time series
  mdwp <- aggregate(dpts$tdpC, by = list(floor_date(dpts$date, unit = 'day')),
                    FUN = mean)  
  
  # 2) Subtract mean daily T_dwp @ basin from T_dwp @ Nwp (mean dailys)
  for (i in 1 : nrow(bmdp)) {
    bmdp[i, 2 : (length(shp) + 1)] <- mdwp[i, 2] - bmdp[i, 2 : (length(shp) + 1)]
  }
  
  # 2) Subtract that difference from the T_dwp @ Nwp (time series) for each basin
  # Set up the data frame for the time series data
  dtes <- seq(as.POSIXct(strD, '%Y-%m-%d', tz = 'America/Los_Angeles'),
              as.POSIXct(endD, '%Y-%m-%d', tz = 'America/Los_Angeles'), 3600)
  
  dwpT <- data.frame(datetime = dtes,
                     date = floor_date(dtes, unit = 'day'),
                     tdpC = dpts$tdpC)
  
  dwpT <- merge(dwpT, bmdp, by.x = 'date', by.y = 'date', all.x = T)
  
  for (i in 4 : length(dwpT)) {dwpT[, i] <- dwpT[, 3] - dwpT[, i]}
  
  dwpT <- dwpT[, -c(1, 3)]
  
  # Remove indirect input columns and transpose
  dwpT <- dwpT[, c(1, 4, 7, 8, 9, 12, 13, 14, 15, 16, 17)]
  
  row.names(dwpT) <- dwpT$date; dwpT <- dwpT[, -1]; dwpT <- data.frame(t(dwpT))
  
  return(dwpT)
  
}

# __________________________________________________________________________----
# Cloud Cover ----
cloud_q2k <- function(x = NULL, strD = NULL, endD = NULL) {
  
  # This function interrogates hardwired met data file and returns cloud
  # cover (fraction - 0 = clear, 1 = totally cloudy)
  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  mDat <- x[which(x$time >= dtes[1] & x$time <= dtes[2]), c(1, 9)]
  
  mDat <- data.frame(date = mDat$time,
                     B3   = mDat$sky_nwp / 8, B6   = mDat$sky_nwp / 8,
                     B7   = mDat$sky_nwp / 8, B8   = mDat$sky_nwp / 8,
                     B11  = mDat$sky_nwp / 8, B12  = mDat$sky_nwp / 8,
                     B13  = mDat$sky_nwp / 8, B14  = mDat$sky_nwp / 8,
                     B15  = mDat$sky_nwp / 8, B16  = mDat$sky_nwp / 8)
  
  # Transpose and set col names to row 1
  row.names(mDat) <- mDat$date; mDat <- mDat[, -1]; mDat <- data.frame(t(mDat))

  return(mDat)

}

# __________________________________________________________________________----
# Wind Speed ----
wind_q2k <- function(x = NULL, strD = NULL, endD = NULL) {
  
  # This function interrogates hardwired met data file and returns wind speed
  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  mDat <- x[which(x$time >= dtes[1] & x$time <= dtes[2]), c(1, 7)]
  
  mDat <- data.frame(date = mDat$time,
                     B3   = mDat$usp_nwp, B6   = mDat$usp_nwp,
                     B7   = mDat$usp_nwp, B8   = mDat$usp_nwp,
                     B11  = mDat$usp_nwp, B12  = mDat$usp_nwp,
                     B13  = mDat$usp_nwp, B14  = mDat$usp_nwp,
                     B15  = mDat$usp_nwp, B16  = mDat$usp_nwp)
  
  # Transpose and set col names to row 1
  row.names(mDat) <- mDat$date; mDat <- mDat[, -1]; mDat <- data.frame(t(mDat))
  
  return(mDat)

}  

# __________________________________________________________________________----
# Direct Solar Radiation ----
solar_q2k <- function(strD = NULL, endD = NULL) {

  source('C:/Users/rshojin/Desktop/006_scripts/github/Met_Functions/solar2par.R')
  
  # lat/lon of basin centroids
  basn <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxyge',
                          'n/Middle_Siletz_River_1710020405/004_gis/001_data/001',
                          '_shape/siletz_catchments_HSPF_latlon.csv'))
  
  basn <- basn[basn$HSPF_Bas %in% c(3, 6, 7, 8, 11, 12, 13, 14, 15, 16), c(5, 11, 12)]
  
  tz = 'America/Los_Angeles'
  
  dtes <- as.POSIXct(c('2017-07-17', '2017-07-22'), '%Y-%m-%d', tz = tz)
  
  solr <- data.frame(date = seq(dtes[1], dtes[length(dtes)], 3600))
  
  solr <- cbind(solr, matrix(data = 0, nrow = nrow(solr), ncol = nrow(basn)))
  
  names(solr)[2 : length(solr)] <- paste0('basn_', basn$HSPF_Bas)
  
  for (i in 1 : nrow(basn)) {
    
    tmp <- solar2par(strDate = dtes[1], endDate = dtes[length(dtes)],
                     lat = basn[i, 2], lon = basn[i, 3], pres = 1013)
    
    solr[, i + 1] <- tmp$solar
    
  }
  
  row.names(solr) <- solr$date; solr <- solr[, -1]; solr <- data.frame(t(solr))

  return(solr)  

}

# __________________________________________________________________________----
# Supporting functions ----
minmax_time <- function(mDat = NULL) {
  
  # Specify a dataframe of date/times and variable values; this function returns
  # a data frame of the times of maximum and minimum values for each day
  
  library(dplyr); library(lubridate)
  
  names(mDat) <- c('time', 'val')
  
  # Create the dates and AM/PM
  mDat <- mDat %>% mutate(date = floor_date(x = mDat$time, unit = 'day'),
                          ampm = ifelse(hour(mDat$time) < 12, 0, 1))
  
  mins <- aggregate(mDat[which(mDat$ampm == 0), 2], FUN = 'min', na.rm = T,
                    by = list(mDat[which(mDat$ampm == 0), 3]))
  
  maxs <- aggregate(mDat[which(mDat$ampm == 1), 2], FUN = 'max', na.rm = T,
                    by = list(mDat[which(mDat$ampm == 1), 3]))
  
  maxs <- rbind(maxs, mins[nrow(mins),])
  
  mnmx <- data.frame(date = mins$Group.1, min = mins$x, max = maxs$x)
  
  tmnx <- data.frame(date = 0, min = 0, max = 0); tmnx <- tmnx[-1, ]
  
  for (i in 1 : nrow(mnmx)) {
    
    temp <- mDat[mDat$date %in% mnmx[i, 1], ]
    
    cond <- list(min = which(temp$val == mnmx[i, 2] & temp$ampm == 0),
                 max = which(temp$val == mnmx[i, 3] & temp$ampm == 1))
    
    tmnx <- rbind(tmnx,
                  data.frame(date = mnmx[i, 1],
                             min = round_date(mean(temp[cond[[1]], 1]), 'hour'),
                             max = round_date(mean(temp[cond[[2]], 1]), 'hour')))
    
  }
  
  tmnx <- tmnx[-nrow(tmnx), ]
  
  return(tmnx)
  
}

calc_sin <- function(t1 = NULL, t2 = NULL, x = NULL) {
  
  # Function for creating a sinusoidal signal between a daily max and min value
  # Additional arguement includes the time series between the max & min
  
  x <- pi * (as.numeric(x - min(x)) / 3600) / (length(x) - 1)
  
  y <- (t2 - t1) / 2 * cos(x) + (t2 + t1) / 2; return(y)
  
}

plot_temps <- function(df = NULL) {
  
  # Input a data frame (wide) of temperatures by basin, plots to window and
  # returns a plot object
  
  library(reshape2); library(ggplot2)
  
  names(df) <- c('dates', paste0('B', names(df)[2 : length(df)]))
  
  y <- melt(data = df, id.vars = 'dates', variable.name = 'basn', value.name = 'tmpC')
  
  plot <- ggplot(data = y, aes(x = dates, y = tmpC, color = basn)) + geom_line()
  
  return(plot)
  
}

