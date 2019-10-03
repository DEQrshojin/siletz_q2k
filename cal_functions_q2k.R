
# CREATE PEST OBS OBJECT ----
obs4PEST <- function(strD = NULL, endD = NULL) {

  # This function creates an output file for use in PEST based on the
  # amalgamation of data from various sources. Input the start and end
  # dates for the output file; Also creates an output dataframe that
  # is to be used in collated the model output data file.
  
  # ________________________________________________________________________----
  # Libraries, Functions and Objects----
  library(reshape2); library(dplyr)

  # Function to add a leading 0 if a number is less than 10
  addZ <- function(v) {ifelse(v < 10, paste0(0, v), as.character(v))}
  
  # ________________________________________________________________________----
  # Load And Process Data----  
  # Load observation data
  d <- load_obs(strD, endD); d <- d[2 : 5] # Don't need flow data so adios!

  # Load reach and station links
  rows <- read.csv('D:/siletz_q2k/05_calib/rhdr.csv', stringsAsFactors = F)
  
  # ________________________________________________________________________----
  # Create A Master Keys Of Obs Id List----
  # Exclude grabs, but make one of all continuous obs, and one of daily mean rear
  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  # dts1 are dates/times for continuous data, dts2 for reaeration
  dts1 <- seq(dtes[1], dtes[2], 3600); dts2 <- seq(dtes[1], dtes[2], 86400)
  
  nmes <- c('tmp', 'doc', 'phX'); obID <- data.frame(stringsAsFactors = F)

  # Continuous parameters
  for (i in 1 : 3) {
    
    for (j in 1 : 10) {

      # The key in order includes: reach#, parm, month, day, hour
      temp <- data.frame(date = dts1, q2kR = addZ(rep(j, length(dts1)))) %>%
              mutate(obID = paste0('r', q2kR, nmes[i], addZ(month(date)),
                                   addZ(day(date)), addZ(hour(date))))
      
      obID <- rbind(obID, temp)

    }
  }
  
  # Reaeration
  for (i in 1 : 10) {
    
    temp <- data.frame(date = dts2, q2kR = addZ(rep(i, length(dts2)))) %>%
            mutate(obID = paste0('r', q2kR, 'rea', addZ(month(date)),
                                 addZ(day(date)), 12)) # Add 12 for midday (avg)
    
    obID <- rbind(obID, temp)
    
  }
  
  obID$indx <- 1 : nrow(obID); max <- nrow(obID)
  
  # ________________________________________________________________________----  
  # Process LSWCD Data----
  # Remove non-calibration stations, including confluence (cause it's used as BC)
  stns <- c(38944, 36367, 38300, 10391, 37848, 38918, 38928, 11246, 37396)

  lswc <- d[['lswc']][which(d[['lswc']]$STAID %in% stns),
                      c(length(d[['lswc']]), 3, 5, 7)]
  
  # Switch out station ID for reach number
  lswc <- merge(lswc, rows[, c(1, 3)], by.x = 'STAID', by.y = 'sta', all.x = T,
                all.y = F)

  lswc$q2kR <- ifelse(lswc$q2kR < 10, addZ(lswc$q2kR), as.character(lswc$q2kR))
  
  # Create a column for the time-hour
  lswc$hour <- floor_date(lswc$date, 'hour')
  
  # Create the lswc data data frame for population with PEST obs data
  l <- data.frame(stringsAsFactors = F)

  for (i in 3 : 4) {

    temp <- aggregate(lswc[, i], by = list(lswc$q2kR, lswc$hour), mean, na.rm = T)
    
    names(temp) <- c('q2kR', 'date', 'val')

    # The key in order includes: reach#, parm, month, day, hour
    temp$obID <- paste0('r', temp$q2kR, nmes[i - 2], addZ(month(temp$date)),
                        addZ(day(temp$date)), addZ(hour(temp$date)))
    
    # Bind the two DFs for a linear (long) df of lswc data
    l <- rbind(l, temp) # NOTE: l has zome NAs in it.
    
  }

  # ________________________________________________________________________----
  # DEQ Continuous Data----
  deqC <- d[['deqC']][which(d[['deqC']]$pH >= 6.5), c(13, 8, 3)]
  
  # Tidy up
  deqC$STAID <- as.numeric(substr(x = deqC$SITENAME, start = 1, stop = 5))

  # Pull in reach number
  deqC <- merge(deqC, rows[, c(1, 3)], by.x = 'STAID', by.y = 'sta', all.x = T,
                all.y = F)
    
  deqC$q2kR <- addZ(deqC$q2kR)
  
  deqC$date <- floor_date(deqC$date, 'hour')
  
  deqC <- aggregate(deqC[, 3], by = list(deqC$q2kR, deqC$date), mean, na.rm = T)
  
  names(deqC) <- c('q2kR', 'date', 'val')

  # The key in order includes: reach#, parm, month, day, hour
  deqC$obID <- paste0('r', deqC$q2kR, nmes[3], addZ(month(deqC$date)),
                      addZ(day(deqC$date)), addZ(hour(deqC$date)))

  l <- rbind(l, deqC)

  # ________________________________________________________________________----
  # StreamMetabolizer Reaeration Data----
  k600 <- d[['k600']]; k600 <- k600[-which(k600$STA == '37848'), ] # STA 37848 = bad data!
  
  # Simplify data and rename station 29287 to 10391 (but really it's 29287)
  k600$STA <- ifelse(k600$STA == '29287', '10391', k600$STA)
  
  # Merge to incorporate reach number
  k600 <- merge(k600, rows[, c(1, 3)], by.x = 'STA', by.y = 'sta', all.x = T,
                all.y = F)
  
  k600$q2kR <- addZ(k600$q2kR)
  
  # The key in order includes: reach#, dummy parm (XXX), month, day
  k600$obID <- paste0('r', k600$q2kR, 'rea', addZ(month(k600$date)),
                      addZ(day(k600$date)), 12)
  
  k600 <- k600[, c(4, 2, 3, 5)]; names(k600) <- c('q2kR', 'date', 'val', 'obID')
  
  l <- rbind(l, k600)

  # ________________________________________________________________________----
  # Merge, Record and Clean Data ----
  obID <- merge(obID, l, by.x = 'obID', by.y = 'obID', all.x = T)
  
  # Clean up and rename columns
  obID <- obID[, c(1 : 4, 7)]; names(obID) <- c('obID', 'date', 'q2kR', 'indx', 'val')
  
  # Reorder to original
  obID <- obID[order(obID$indx), ]
  
  # record index of NA, NaN, Inf and -Inf for when extracting model data
  obNA <- obID[which(is.na(obID$val)), ]
  
  obID <- obID[-which(is.na(obID$val)), ]

  # ________________________________________________________________________----
  # DEQ Grab Data----
  # Pull out Date, station, NO3, TP, TOC, CBOD and Chl a in that order 
  deqG <- d[['deqG']][, c(23, 2, 9, 10, 17, 18, 11, 12, 5, 6, 21, 22)]
  
  # Fix stations: 29287 -> 10391; 38919 -> 29287; remove 38928 and 38930
  deqG$STAID <- ifelse(deqG$STAID == 29287, 10391,
                       ifelse(deqG$STAID == 38919, 10391, deqG$STAID))
  
  deqG <- deqG[-which(deqG$STAID == 38929 | deqG$STAID == 38930), ]
  
  # Bring in the reach number
  deqG <- merge(deqG, rows[, c(1, 3)], by.x = 'STAID', by.y = 'sta', all.x = T,
                all.y = F)
  
  deqG$q2kR <- addZ(deqG$q2kR)
  
  # Replace non-detects (PAR_nd = -1) with NA
  for (i in seq(3, 11, 2)) {deqG[, i] <- ifelse(deqG[, i + 1] == -1, NA, deqG[, i])}
  
  # Remove the ND columns
  deqG <- deqG[, -c(1, seq(4, 12, 2))]

  # Cast as long
  deqG <- melt(deqG, id.vars = c('date', 'q2kR'), value.name = 'val',
               variable.name = 'par')

  deqG <- deqG[complete.cases(deqG), ] # 65 measurements with melt before obID

  # Fix parameter names - combine with new names for the obs ID column
  fixP <- data.frame(old = c('NOx_r', 'TP_r', 'TOC_r', 'CBOD_r', 'CHLA_r'),
                     new = c('nox', 'tpX', 'toc', 'bod', 'cha'),
                     ord = c(1, 2, 3, 4, 5))
  
  deqG <- merge(deqG, fixP, by.x = 'par', by.y = 'old')
  
  # The key in order includes: reach#, parm, month, day, hour
  deqG$obID <- paste0('r', deqG$q2kR, deqG$new, addZ(month(deqG$date)),
                      addZ(day(deqG$date)), addZ(hour(deqG$date)))

  deqG$date <- round_date(deqG$date, 'hour')
  
  # reorder by parameter, reach then date
  deqG <- deqG %>% arrange(ord, q2kR, date)

  # Create index
  deqG$indx <- (max + 1) : (max + nrow(deqG))  
  
  # obID column order obID, date, reach, indx, val
  deqG <- deqG[, c(7, 2, 3, 8, 4)]
  
  names(deqG) <- c('obID', 'date', 'q2kR', 'indx', 'val')

  obID <- rbind(obID, deqG)
  
  # Add the group, package up and return
  obID$grp <- substr(x = obID$obID, start = 4, stop = 6)

  return(list(obs = obID, obNA = obNA))

}

# CREATE QUAL2KW OBSERVATIONS OUTPUTS ----
obs4q2k <- function(date = NULL, fOut = NULL) {

  # This function creates files (.csv) that are used for calibration of Qual2Kw
  # They output for one day specified in the arguements. Additional arguement 
  # includes the output file path

  library(lubridate); library(streamMetabolizer); library(reshape2)
  
  `%notin%` <- Negate(`%in%`)
  
  # ________________________________________________________________________----
  # Initialize Env Vars ----
  date <- as.POSIXct(date, '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  dOut <- as.character(paste0('_', year(date),
                              ifelse(month(date) < 10, '0', ''), month(date),
                              ifelse(day(date) < 10, '0', ''), day(date)))
  
  # If specifying an output file path, check output path last character = '/'
  if (!is.null(fOut)) {
    
    if (substr(fOut, nchar(fOut), nchar(fOut)) != '/') {fOut <- paste0(fOut, '/')}
    
  }

  # directory for the input data  
  pth1 <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle_',
                 'Siletz_River_1710020405/001_data/')
  
  # Read the file with reach information
  rows <- read.csv('D:/siletz_q2k/05_calib/rhdr.csv', stringsAsFactors = F)
  
  # ________________________________________________________________________----
  # Set Up Output DFs ----
  # header data; the diel headers are the same as the mean/min/max WQ headers
  hdr <- list(hydr = c('dist', 'flow', 'dpth', 'velc', 'wdth', 'rear'),
              temp = c('dist', 'tAvg', 'tMin', 'tMax'),
              wqmn = c('rchN', 'dist', 'date', 'temp', 'cond', 'iss', 'do_c', 'bods',
                       'bodf', 'orgN', 'nh4', 'no3', 'orgP', 'po4', 'phyt', 'toc',
                       'path', 'genC', 'alk', 'tic', 'co2', 'hco3', 'co3', 'pH'))
  
  hdr[[4]] <- hdr[[5]] <- hdr[[6]] <- hdr[[3]]
  
  names(hdr)[4 : 6] <- c('wqmi', 'wqmx', 'diel')
  
  # date-time column for the diel data
  ts <- seq(date, date + days(1) - 3600, 3600)
  
  out <- list()
  
  # control for the number of rows in each output data frame (11 for all but diel [24])
  outL <- c(rep(11, 5), 24)
  
  for (i in 1 : 6) {
    
    out[[i]] <- data.frame(matrix(data = 0, nrow = outL[i], ncol = length(hdr[[i]]),
                                  dimnames = list(1 : outL[i], hdr[[i]])))
    
    # Set the hydraulic and temperature tables up
    if (i %in% 1 : 2) {out[[i]]$dist <- rows$dst}
    
    # Set the mean/min/max tables up
    if (i %in% 3 : 5) {out[[i]][, 1 : 2] <- rows[, 3 : 4]}
    
    # Set the diel data tables up
    if (i == 6) {
      
      out[[i]]$date <- ts # Set the date/time column same for all stations
      
      # Create a temporary df for binding rows
      temp <- out[[i]]
      
      # Populate the initial data - reach number and distance
      out[[i]]$rchN <- rows[1, 3]; out[[i]]$dist <- rows[1, 4]
      
      for (j in 2 : nrow(rows)) {
        
        temp$rchN <- rows[j, 3]; temp$dist <- rows[j, 4]
        
        out[[i]] <- rbind(out[[i]], temp)
        
      }
    }
  }
  
  names(out) <- c('hydr', 'temp', 'wqav', 'wqmi', 'wqmx', 'diel')
  
  # ________________________________________________________________________----
  # Load Data ----
  
  # PERHAPS SWITCH TO THE FUNCTION IF NECESSARY LATER ON
  # Modeled flow data
  hspf <- readRDS('D:/siletz/outputs/calib_20190611/rchQLC_NOx.RData')
  
  # Stream metabolizer data
  k600 <- readRDS(paste0(pth1, 'metabolism/Metab_Output_MLE_all_wDepth.RData'))
  
  # LSWCD data
  lswc <- read.csv(paste0(pth1, 'wq_data/Monitoring 2017/LSWCD/Lincoln_SWCD_SI',
                          'LETZ RIVER_06292017-01052018/siletz_volmon_cont_data.csv'),
                   stringsAsFactors = F)
  
  # DEQ continuous
  deqC <- read.csv(paste0(pth1, 'wq_data/deq_cont_2017.csv'), stringsAsFactors = F)
  
  # DEQ grabs
  deqG <- read.csv(paste0(pth1, 'wq_data/Monitoring 2017/DEQ/Grab_Samples/deq_g',
                          'rabs_2017.csv'), stringsAsFactors = F)
  
  # ________________________________________________________________________----
  # Modeled Flow Data ----
  hspf <- hspf[[1]]
  
  # Create a column of the dates (no time)
  hspf$Date2 <- floor_date(hspf$Date, 'day')
  
  # Isolate the data for just that date
  hspf <- hspf[which(hspf$Date2 == date),
               c(1, 2, 3, 6, 7, 8, 11, 12, 13, 14, 15, 16) + 1]
  
  # Calculate the daily mean
  hspf <- colMeans(hspf)
  
  # Populate the output data frame
  out[[1]]$flow <- c(hspf[1] + hspf[2], hspf[3 : 12])
  
  # Stream Metabolism ----
  # Simplify data and rename station 29287 to 10391 (but really it's 29287)
  if (month(date) < 9) {k600 <- k600[[1]]} else {k600 <- k600[[2]]}
  
  names(k600)[4] = '10391'
  
  tmpK <- data.frame(sta = rep(0, length(k600)), k600 = rep(0, length(k600)))
  
  for (i in 1 : length(k600)) {
    
    if (typeof(k600[[i]]) == 'character') {x <- NA} else {
      x <- k600[[i]]@fit[which(k600[[i]]@fit$date == as.Date(date)), 8]
    }
    
    tmpK[i, ] <- c(names(k600)[i], x)
    
  }
  
  # Merge to associate K600 with proper station
  tmpK <- merge(rows, tmpK, by.x = 'sta', by.y = 'sta', all.x = T, all.y = F)
  
  # Reorder from US -> DS
  tmpK <- tmpK[order(tmpK$q2kR), ]
  
  out[[1]]$rear <- as.numeric(tmpK$k600)
  
  # LSWCD Data ----
  lswc$DATE.TIME <- as.POSIXct(lswc$DATE.TIME, '%m/%d/%Y %H:%M', tz = 'America/Los_Angeles')
  
  # Weed out stations
  lswc <- lswc[lswc$STAID %in% rows$sta, ]
  
  # Isolate data on that date and necessary columns
  lswc$date <- floor_date(lswc$DATE.TIME, 'day'); lswc <- lswc[lswc$date %in% date, c(10, 3, 5, 7, 1)]
  
  # Iterate through temp and do 
  for (i in 3 : 4) {
    
    tmp2 <- list(mean = aggregate(lswc[, i], by = list(lswc$STAID), FUN = 'mean', na.rm = T),
                 min = aggregate(lswc[, i], by = list(lswc$STAID), FUN = 'min', na.rm = T),
                 max = aggregate(lswc[, i], by = list(lswc$STAID), FUN = 'max', na.rm = T))
    
    tmp2 <- cbind(tmp2[['mean']], tmp2[['min']][, 2], tmp2[['max']][, 2])
    
    names(tmp2) <- c('sta', 'avg', 'min', 'max')
    
    tmp2 <- merge(rows, tmp2, by.x = 'sta', by.y = 'sta', all.x = T, all.y = F)
    
    tmp2 <- tmp2[order(tmp2$q2kR), 3 : length(tmp2)]
    
    # Write temperature in two places (even though it's not used on the)
    if (i == 3) {out[['temp']][, 2 : 4] <- tmp2[, 3 : 5]}
    
    if (i == 4) {for (j in 3 : 5) {out[[j]][, 7] <- tmp2[, j]}}
    
  }
  
  # Continuous Deq Data ----
  # Dates
  deqC$DATE <- as.POSIXct(paste0(deqC$DATE, ' ', deqC$TIME1), '%m/%d/%Y %H:%M',
                          tz = 'America/Los_Angeles')
  
  # SiteName
  deqC$SITENAME <- substr(x = deqC$SITENAME, start = 1, stop = 5)
  
  # Isolate cond and pH and rename columns
  deqC <- deqC[, c(1, 3, 5, 8)]; names(deqC) <- c('date', 'site', 'cond', 'pH')
  
  # ISOLATE CAL DATE
  deqC$cldt <- floor_date(deqC$date, 'day')
  
  deqC <- deqC[which(deqC$cldt == date), ]
  
  deqC[which(deqC$cond < 50), 3] = NA
  
  pCol <- c(5, 24)
  
  for (i in 3 : 4) {
    
    tmp3 <- list(mean = aggregate(deqC[, i], by = list(deqC$site), FUN = 'mean', na.rm = T),
                 min = aggregate(deqC[, i], by = list(deqC$site), FUN = 'min', na.rm = T),
                 max = aggregate(deqC[, i], by = list(deqC$site), FUN = 'max', na.rm = T))
    
    tmp3 <- cbind(tmp3[['mean']], tmp3[['min']][, 2],tmp3[['max']][, 2])
    
    names(tmp3) <- c('sta', 'avg', 'min', 'max')
    
    tmp3 <- merge(rows, tmp3, by.x = 'sta', by.y = 'sta', all.x = T, all.y = F)
    
    tmp3 <- tmp3[order(tmp3$q2kR), 3 : length(tmp3)]
    
    # Populate the wq output dataframes (cond = col 5, pH = col 24)
    for (j in 3 : 5) {out[[j]][, pCol[i - 2]] <- tmp3[, j]}
    
  }
  
  # DEQ Grab Data ----
  deqG$DATE <- as.POSIXct(deqG$DATE, '%Y-%m-%d %H:%M', tz = 'America/Los_Angeles')
  
  deqG$DATE2 <- floor_date(deqG$DATE, 'day')
  
  # Made a mistake with Chl a data, multiply by 1000 to get back to micrograms
  deqG$CHLA_r <- deqG$CHLA_r * 1000
  
  # Rearrange the columns
  deqG <- deqG[which(deqG$DATE2 == date), c(length(deqG), 2 : (length(deqG) - 1))]
  
  # Rearrange columns so that values in front, nd in back
  # In this order with     TP, PO4, TOC, TKN, NH3, NO3, BOD, CHLA (remove TSS)
  deqG <- deqG[, c(1, 2, c(17,  13,  11,  15,   7,   9,   5,   21),
                   c(17,  13,  11,  15,   7,   9,   5,   21) + 1)]  
  
  # Imput non-detects
  # Create df of coefficients (Q-TP, Q-PO4, TP-TOC, TP-TKN)
  coef <- data.frame(m = c(1.034e-5, 7.267e-7, 2.8, -1.0),
                     b = c(0, 0.00452, 0.7217, 0.150),
                     col = c(19, 19, 3, 3))
  
  # Bring in flow data
  rows$hspf <- c(hspf[1] + hspf[2], hspf[3 : length(hspf)])
  
  # Convert flow to cfs
  rows$hspf <- rows$hspf * 35.314666213
  
  # Combine the reach data with the grab data
  deqG <- merge(deqG, rows[, c(1, 5)], by.x = 'STAID', by.y = 'sta')
  
  # Iterate through the parameters that have censor methods, but bypass TP
  # imputation -- results in negative organic P values
  
  # Impute ND relationships
  # c:/users/rshojin/desktop/006_scripts/github/siletz_do/impute_nondetects.r
  # TP and PO4 from Q
  # Q-TP , Normal-Normal, m = 1.03423e-05 (p << 0.05) -- Intercept not significant 
  # Q-PO4, Normal-Normal, m = 7.2666e-7 (p << 0.05), 0.00452 (p << 0.05)   
  # TOC and TKN from TP
  # These are from basin_wide_carbon_nitrogen_analysis using total phosphorus
  # TP-TOC, Normal-Normal, m = 2.8 (p = 0.0003), b = 0.7217 (p = 8e-9)
  # TP-TKN, Normal-Normal, m = -1.0 (p = 0.0084), b = 0.1500 (p < 5e-13)
  # NH3 from TP
  # Using a logistic regression of TP-NH3, if TP < 0.03442412, then NH3 = 0.005
  # Else, NH3 = 0.01 for all NH3 < MRL; using basin_wide_NH3_binomial.R script

  for (i in 4 : 6) {
    
    # create a temp df with indc var & resp var and d/nd
    tmp4 <- deqG[, c(coef[i - 2, 3], i, i + 8)]
    
    # Impute the ND
    tmp4$impV <- ifelse(tmp4[, 3] == -1,coef[i - 2, 1] * tmp4[, 1] + coef[i - 2, 2],
                        tmp4[, 2])
    
    # Correct NAs due to indicator var ND (i.e., use 1/2 MDL)
    deqG[, i] <- ifelse(is.na(tmp4[, 1]) & !is.na(tmp4[, 2]), tmp4[, 2], tmp4[, 4])
    
  }
  
  # Assign the ammonium non-detects
  deqG[, 7] <- ifelse(!is.na(deqG[, 7]) & deqG$TP_r < 0.0344, 0.005, 0.10)
  
  tmp4 <- list(mean = aggregate(deqG[, 3 : 10], by = list(deqG$STAID),
                                FUN = 'mean', na.rm = T),
               min = aggregate(deqG[, 3 : 10], by = list(deqG$STAID),
                               FUN = 'min', na.rm = T),
               max = aggregate(deqG[, 3 : 10], by = list(deqG$STAID),
                               FUN = 'max', na.rm = T))
  
  # Replace Inf and -Inf with NA
  for (i in 2 : 3) {
    
    for (j in 2 : length(tmp4[['mean']])) {
      
      tmp4[[i]][, j] <- ifelse(tmp4[[i]][, j] == Inf, NA, tmp4[[i]][, j])
      
      tmp4[[i]][, j] <- ifelse(tmp4[[i]][, j] == -Inf, NA, tmp4[[i]][, j])
      
    }
  }
  
  # 
  rchC <- tmp4[[1]]$q2kR
  
  # The 'FROM' columns: OrgP, PO4, TOC, OrgN, NH3, NOx, CBOD, CHLA
  mCol <- c(13, 14, 16, 10, 11, 12, 9, 15)
  
  # Columns to convert to micrograms
  cCol <- c(3, 4, 6, 7, 8)
  
  for (i in 1 : 3) {
    
    # Calculate organic P
    tmp4[[i]]$TP_r <- tmp4[[i]]$TP_r - tmp4[[i]]$PO4_r
    
    tmp4[[i]]$TP_r <- ifelse(tmp4[[i]]$TP_r < 0, NA, tmp4[[i]]$TP_r)
    
    # Calculate organic N
    tmp4[[i]]$TKN_r <- tmp4[[i]]$TKN_r - tmp4[[i]]$NH3_r
    
    tmp4[[i]]$TKN_r <- ifelse(tmp4[[i]]$TKN_r < 0, NA, tmp4[[i]]$TKN_r)
    
    names(tmp4[[i]])[2] <- 'OrgP'; names(tmp4[[i]])[5] <- 'OrgN'
    
    tmp4[[i]] <- merge(rows[, c(3, 1)], tmp4[[i]], by.x = 'sta', by.y = 'Group.1',
                       all.x = F, all.y = T)
    
    for (j in 3 : length(tmp4[[i]])) {
      
      # Convert to micrograms here
      if (j %in% cCol) {tmp4[[i]][, j] <- tmp4[[i]][, j] * 1000}
      
      # Merge the output and temp dfs to match reach information      
      tmp5 <- merge(out[[i + 2]][, 1 : 2], tmp4[[i]][, c(2, j)],
                    by.x = 'rchN', by.y = 'q2kR', all.x = T, all.y = T)
      
      out[[i + 2]][, mCol[j - 2]] <- tmp5[, 3]
      
    }
  }
  
  # Extract Diel Data ----
  # Only provide diel data for DO, temp, and pH
  # use LSWCD data for DO and temp and DEQ data for cond and pH
  # Par: temp, do_c, cond,   pH
  # Col:    4,    7,    5,   24
  vCol <- c(4, 5, 7, 24)
  
  # Merge lswc and deqC with rows to associate station with Reach #
  lswc <- merge(lswc, rows[, c(1, 3)], by.x = 'STAID', by.y = 'sta', all.x = T,all.y = F)
  
  deqC <- merge(deqC, rows[, c(1, 3)], by.x = 'site', by.y = 'sta', all.x = T, all.y = F)
  
  # Add a mintes columns and weed out entries where minutes <> 0
  lswc$min <- minute(lswc$DATE.TIME); lswc <- lswc[which(lswc$min == 0), ]
  
  deqC$min <- minute(deqC$date); deqC <- deqC[which(deqC$min == 0), ]
  
  # List of the basin names
  actC <- 0 : 10
  
  # Create a list for the data to be used in the actual calibration
  tLst <- list()
  
  for (i in 1 : 2) {
    
    # Cast to wide and combine the tables  
    tmpL <- dcast(lswc[, c(5, i + 2, 6)], DATE.TIME ~ q2kR,
                  value.var = names(lswc)[i + 2],  fun.aggregate = mean) # lswc
    tmpQ <- dcast(deqC[, c(2, i + 2, 6)], date ~ q2kR,
                  value.var = names(deqC)[i + 2], fun.aggregate = mean) # cont DEQ
    
    # Merge both with the time series to get a complete 24 hours
    tmpL <- merge(data.frame(ts), tmpL, by.x = 'ts', by.y = 'DATE.TIME', all = T)
    tmpQ <- merge(data.frame(ts), tmpQ, by.x = 'ts', by.y = 'date', all = T)
    
    # Figure out which stations do not have data
    mbnL <- actC[which(actC %notin% as.numeric(names(tmpL)[2 : length(tmpL)]))]
    mbnQ <- actC[which(actC %notin% as.numeric(names(tmpQ)[2 : length(tmpQ)]))]
    
    # Make dummy columns with NA daat
    filL <- data.frame(matrix(data = NA, nrow = 24, ncol = length(mbnL)))
    filQ <- data.frame(matrix(data = NA, nrow = 24, ncol = length(mbnQ)))
    
    # Rename to the names of the reaches no in the data
    names(filL) <- mbnL; names(filQ) <- mbnQ
    
    # column bind the data
    tmpL <- cbind(tmpL, filL); tmpQ <- cbind(tmpQ, filQ)
    
    # Order on reach
    tmpL <- tmpL[, c(1, order(as.numeric(names(tmpL)[2 : length(tmpL)])) + 1)]
    tmpQ <- tmpQ[, c(1, order(as.numeric(names(tmpQ)[2 : length(tmpQ)])) + 1)]
    
    # Extract the diel data for the calibration metrics
    # Delete Column 2 (HW) and rows 2, 3, 5, 6, 8, 9...(hours not div by 3)
    # if i = 1, item = 1, if i = 2, item = 2
    tLst[[i]]     <- tmpL[which(hour(tmpL$ts) %% 3 == 0), - 2]
    
    # if i = 1, item = 3, if i = 2, item = 4
    tLst[[i + 2]] <- tmpQ[which(hour(tmpQ$ts) %% 3 == 0), - 2]
    
    # Cast to long
    tmpL <- melt(data = tmpL, id.vars = 'ts', variable.name = 'rchN',
                 value.name = names(lswc)[i + 2])
    tmpQ <- melt(data = tmpQ, id.vars = 'ts', variable.name = 'rchN',
                 value.name = names(deqC)[i + 2])
    
    # Add the diel data to the output list df
    out[['diel']][, vCol[2 * i - 1]] <- tmpL[, 3]
    out[['diel']][, vCol[2 * i]] <- tmpQ[, 3]
    
  }
  
  # Collate the calibration data from tLst
  names(tLst) <- c('temp', 'do_c', 'cond', 'pH')
  
  # Rename the hour rows to the PAR_Hour and transpose
  for (i in 1 : 4) {
    
    row.names(tLst[[i]]) <- paste0(names(tLst)[i], '_', hour(tLst[[i]]$ts))
    
    tLst[[i]] <- tLst[[i]][, -1]; tLst[[i]] <- t(tLst[[i]])
    
  }
  
  out[['cal']] <- cbind(data.frame(tLst[[1]]), data.frame(tLst[[2]]),
                        data.frame(tLst[[3]]), data.frame(tLst[[4]]))
  
  # ________________________________________________________________________----
  # Output Data 2 CSV ----
  # Replace Inf and -Inf with NA, replace 0 with NA
  stCl <- c(2, 2, 3, 3, 3, 4, 1)
  
  for (i in 1 : length(out)) {
    
    for (j in stCl[i] : length(out[[i]])) {
      
      out[[i]][, j] <- ifelse(out[[i]][, j] == 0, NA, out[[i]][, j])
      
    }
    
    if (!is.null(fOut)) {write.csv(x = out[[i]], row.names = F,
                                   file = paste0(fOut, names(out)[i], dOut, '.csv'))}
    
  }
  
  return(out)

}

# LOAD OBS DATA 4 Q2KW CAL ----
load_obs <- function(strD = NULL, endD = NULL) {

  # Returns a list of 5 data frames with the following observation data:
  # 1) Reach flows per basin from HSPF
  # 2) Reaeration rates for each station based on StreamMetabolizer data
  # 3) Lincoln County SWcD data which includes continuous temp and DO
  # 4) DEQ continuous data which includes pH and conductivity
  # 5) DEQ grab sample data which includes N, P, C, chl and CBOD

  library(lubridate)
    
  tz <- 'America/Los_Angeles'
  
  pth1 <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle',
                 '_Siletz_River_1710020405/001_data/')
  
  d <- list(hspf = readRDS('D:/siletz/outputs/calib_20190611/rchQLC_NOx.RData'),
            k600 = readRDS(paste0(pth1, 'metabolism/Metab_Output_MLE_all_wDepth.RData')),
            lswc = read.csv(paste0(pth1, 'wq_data/Monitoring 2017/LSWCD/Lincoln',
                                   '_SWCD_SILETZ RIVER_06292017-01052018/siletz',
                                   '_volmon_cont_data.csv'), stringsAsFactors = F),
            deqC = read.csv(paste0(pth1, 'wq_data/deq_cont_2017.csv'),
                             stringsAsFactors = F),
            deqG = read.csv(paste0(pth1, 'wq_data/Monitoring 2017/DEQ/Grab_Samp',
                                   'les/deq_grabs_2017.csv'), stringsAsFactors = F))

  # Isolate the flows only from HSPF
  d[['hspf']] <- d[['hspf']]$reach_flows
  
  # Isolate the list for the DO standard period
  if (!is.null(strD)) {
    
    # Isolate the correct period (cold water or spawning)
    if (!is.POSIXct(strD)) {strD <- as.POSIXct(strD, '%Y-%m-%d', tz = tz)}
    if (!is.POSIXct(endD)) {endD <- as.POSIXct(endD, '%Y-%m-%d', tz = tz)}
    
    co <- as.POSIXct('2017-09-01', '%Y-%m-%d', tz = tz)
    
    if (strD < co) {
      d[['k600']] <- d[['k600']][[1]]
    } else {
      d[['k600']] <- d[['k600']][[2]]
    }
    
    # Create a data frame of daily mean reaeration
    d$k600 <- d$k600[which(d$k600 != 'No Data')]

    temp <- data.frame(matrix(data = 0, nrow = 0,
                              ncol = length(d[['k600']][[1]]@fit) + 1))
    
    names(temp) <- c(names(d[[2]][[1]]@fit), 'STA')
    
    for (i in 1 : length(d$k600)) {

      # Add a column to each named STN
      d[[2]][[i]]@fit$STA <- names(d[[2]])[i]

      # Append the reaeration DF with station ID to the temp
      temp <- rbind(temp, d[['k600']][[i]]@fit)

    }

    d$k600 <- temp[which(complete.cases(temp$K600.daily)), c(1, 8, 17)]
    
  }
  
  # Coerce dates
  names(d[['hspf']])[1] <- 'date'
  
  d[['k600']]$date <- as.POSIXct(d[['k600']]$date, '%Y-%m-%d', tz = tz)
  
  d[['k600']]$date <- floor_date(d[['k600']]$date, 'day')
  
  d[['lswc']]$date <- as.POSIXct(d[['lswc']]$DATE.TIME, '%m/%d/%Y %H:%M', tz = tz)
  
  d[['deqC']]$date <- as.POSIXct(paste0(d[['deqC']]$DATE, " ", d[['deqC']]$TIME1),
                                 '%m/%d/%Y %H:%M:%S', tz = tz)
  
  d[['deqG']]$date <- as.POSIXct(d[['deqG']]$DATE, '%Y-%m-%d %H:%M', tz = tz)

  # Trim dates to specified start and end dates
  for (i in 1 : 5) {d[[i]] <- d[[i]][which(d[[i]]$date >= strD & d[[i]]$date <= endD), ]}

  return(d)

}
  
# ADD OBJ FUN (PEST) WEIGHTS TO OBS ----
add_weights <- function(df = NULL) {

  # This function adds observation objective function weights to a dataframe
  # of observations with IDs, groups and values based on the group weights
  # hardcoded in the data frame relWgt below
  
  library(dplyr)

  # CHANGE ME __________________________________________________________________
  # Change me if the groups or the objective function group weights change    
  relWgt <- data.frame(grp = c("tmp", "doc", "phX", "rea", "nox",
                               "tpX", "toc", "bod", "cha"),
                       rel = c(   27,    45,    18,     1,     2,
                                   2,     2,     1,     1),
                       stringsAsFactors = F) %>%
            mutate(num = rep(0, length(grp)))
  # CHANGE ME __________________________________________________________________

  for (i in 1 : nrow(relWgt)) {
    relWgt[i, 3] <- length(which(df$grp == unique(relWgt$grp)[i]))
  }
  
  relWgt <- relWgt %>% mutate(wgt = rel / num)
  
  # Merge the tables to assign a weights and reorder per the index
  df <- merge(df, relWgt[, c(1, 4)], all = T); df <- df %>% arrange(indx)
  
  # Rearrange columns to .pst order
  df <- df[, c(2, 6, 7, 1)]
  
  return(df)
  
}

# FORMAT OBS AND OUTPUT TO FILE FOR .PST ----
output_obs <- function(df = NULL, fOut = NULL) {

  # This function takes a data frame of observations (with ID, Grp, and Val) and 
  # formats it to output for inclusion in the pest control (.pst) file
  # Assumes a ID length of 12 and a group length of 3
  
  library(dplyr)

  df$val <- format(df$val, digits = 9, scientific = T)
  
  df$wgt <- format(df$wgt, digits = 9, scientific = T)

  if (is.null(fOut)) {return(df)} else {
    
    write.table(df, fOut, quote = F, row.names = F, col.names = F)  
    
  }
}


