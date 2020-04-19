# Suite of functions to create met input dfs for Qual2Kw; parameters include:
# air temp, dew point T, cloud cover, wind speed, and direct solar radition.
# Function for writing to csv file as well.

# __________________________________________________________________________----
# Temperature - Dry Bulb ----
t_air_q2k <- function(strD = NULL, endD = NULL, hBsn = NULL, q2k = F,
                      nday = NULL) {
  
  # Read the T_air data
  airT <- readRDS("C:/siletz_tmdl/01_inputs/02_q2k/RData/T_air_2004_2017.RData")

  names(airT)[1] <- 'date'
  
  # Strip to dates (strD, endD)
  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  airT <- airT[which(airT$date >= dtes[1] & airT$date <= dtes[2]), ]
  
  # Strip to basins (hBsn)
  airT <- airT[, which(names(airT) %in% c('date', paste0('X', hBsn)))]
  
  # Add warm up days if nday = real number
  if (!is.null(nday)) {airT <- add_warm_up(df = airT, nday = nday)}
  
  # Transpose if for Q2K boundary file
  if (q2k) {
    row.names(airT) <- airT$date
    airT <- airT[, -1]
    airT <- data.frame(t(airT))
  }

  return(airT)
  
}

# __________________________________________________________________________----
# Temperature - Dew Point ----
t_dwp_q2k <- function(strD = NULL, endD = NULL, hBsn = NULL, q2k = F,
                      nday = NULL) {
  
  # Read the T_air data
  dwpT <- readRDS("C:/siletz_tmdl/01_inputs/02_q2k/RData/T_dwp_2004_2017.RData")
  
  names(airT)[1] <- 'date'
  
  # Strip to dates (strD, endD)
  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  dwpT <- dwpT[which(dwpT$date >= dtes[1] & dwpT$date <= dtes[2]), ]
  
  # Strip to basins (hBsn)
  dwpT <- dwpT[, which(names(dwpT) %in% c('date', paste0('X', hBsn)))]
  
  # Add warm up days if nday = real number
  if (!is.null(nday)) {dwpT <- add_warm_up(df = dwpT, nday = nday)}
  
  # Transpose if for Q2K boundary file
  if (q2k) {
    row.names(dwpT) <- dwpT$date
    dwpT <- dwpT[, -1]
    dwpT <- data.frame(t(dwpT))
  }
  
  return(dwpT)
  
}

# __________________________________________________________________________----
# Cloud Cover ----
cloud_q2k <- function(x = NULL, strD = NULL, endD = NULL, nday = NULL) {
  
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
  
  # Add warm up days if nday = real number
  if (!is.null(nday)) {mDat <- add_warm_up(df = mDat, nday = nday)}
  
  # Remove duplicate rows
  mDat <- change_dups(mDat)
  
  # Fill NAs
  for (i in 2 : length(mDat)) {mDat[which(is.nan(mDat[, i])), i] <- NA} 
  
  mDat <- mDat %>% tidyr::fill(everything())
  
  # Transpose and set col names to row 1
  row.names(mDat) <- mDat$date; mDat <- mDat[, -1]; mDat <- data.frame(t(mDat))

  return(mDat)

}

# __________________________________________________________________________----
# Wind Speed ----
wind_q2k <- function(x = NULL, strD = NULL, endD = NULL, nday = NULL) {
  
  # This function interrogates hardwired met data file and returns wind speed
  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  mDat <- x[which(x$time >= dtes[1] & x$time <= dtes[2]), c(1, 7)] # Newport
  
  mDat <- data.frame(date = mDat$time,
                     B3   = mDat$usp_nwp, B6   = mDat$usp_nwp,
                     B7   = mDat$usp_nwp, B8   = mDat$usp_nwp,
                     B11  = mDat$usp_nwp, B12  = mDat$usp_nwp,
                     B13  = mDat$usp_nwp, B14  = mDat$usp_nwp,
                     B15  = mDat$usp_nwp, B16  = mDat$usp_nwp)
  
  # Add warm up days if nday = real number
  if (!is.null(nday)) {mDat <- add_warm_up(df = mDat, nday = nday)}
  
  # Remove duplicate datetimes
  mDat <- change_dups(mDat)
  
  # fill NAs
  for (i in 2 : length(mDat)) {mDat[which(is.nan(mDat[, i])), i] <- NA} 
  
  mDat <- mDat %>% tidyr::fill(everything())

  # Transpose and set col names to row 1
  row.names(mDat) <- mDat$date; mDat <- mDat[, -1]; mDat <- data.frame(t(mDat))

  return(mDat)

}  

# __________________________________________________________________________----
# Supporting functions ----
minmax_time <- function(mDat = NULL) {
  
  # Specify a dataframe of date/times and variable values; this function returns
  # a data frame of the times of maximum and minimum values for each day

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
  # Additional arguement includes the time series (x) between the max & min
  
  x <- pi * (as.numeric(x - min(x)) / 3600) / (length(x) - 1)
  
  y <- (t2 - t1) / 2 * cos(x) + (t2 + t1) / 2;
  
  return(y)
  
}

add_warm_up <- function(df = NULL, nday = NULL) {
  
  # Rename the dates column for consistency
  names(df)[1] <- 'date'
  
  # Chunk out the first day
  temp <- df[1 : 24, ]
  
  # Create the data frame of repeating first days
  for (i in 1 : (nday - 1)) {temp <- rbind(temp, df[1 : 24, ])}
  
  df <- rbind(temp, df)
  
  # Recalculate the date/times of each data-frame
  df$date <- seq(df$date[1] - nday * 86400, df$date[nrow(df)], 3600)
  
  return(df)

}

change_dups <- function(df = NULL) {

  df$dchar <- as.character(df$date)
  
  # Add 30 minutes to this time step, to keep it but not duplicate the time.
  df[which(duplicated(df$dchar)), 1] <- df[which(duplicated(df$dchar)), 1] + (60 * 30)
  
  df <- df[, -length(df)]

  return(df)

}

