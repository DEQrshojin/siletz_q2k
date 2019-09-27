# This function creates files (.csv) that are used for calibration of Qual2Kw
# They output for one day specified in the arguements. Additional arguement 
# includes the output file path

calib_data <- function(date = NULL, fOut = NULL) {

  library(lubridate); library(streamMetabolizer)
  
  `%notin%` <- Negate(`%in%`)
  
  # ________________________________________________________________________----
  # INITIALIZE ENV VARS ----
  date <- as.POSIXct(date, '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  dOut <- as.character(paste0('_', year(date),
                              ifelse(month(date) < 10, '0', ''), month(date),
                              ifelse(day(date) < 10, '0', ''), day(date)))

  # Check output path last character = '/'
  if (substr(fOut, nchar(fOut), nchar(fOut)) != '/') {fOut <- paste0(fOut, '/')}
  
  # directory for the input data  
  pth1 <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle_',
                 'Siletz_River_1710020405/001_data/')

  # Read the file with reach information
  rows <- read.csv(paste0(fOut, 'rhdr.csv'), stringsAsFactors = F)

  # ________________________________________________________________________----
  # SET UP OUTPUT DFs ----
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
  # LOAD DATA ----
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
  # EXTRACT DAILY STATISTICS ----
  # MODELED FLOW DATA ----
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

  # STREAM METABOLISM ----
  # Simplify data and rename station 29287 to 10391 (but really it's 29287)
  k600 <- k600[[1]]; names(k600)[4] = '10391'
  
  tmpK <- data.frame(sta = rep(0, length(k600)), k600 = rep(0, length(k600)))
  
  for (i in 1 : length(k600)) {
    
    if (typeof(k600[[i]]) == 'character') {
      x <- NA
    } else {
      x <- k600[[i]]@fit[which(k600[[i]]@fit$date == as.Date(date)), 8]
    }
    
    tmpK[i, ] <- c(names(k600)[i], x)
    
  }
  
  # Merge to associate K600 with proper station
  tmpK <- merge(rows, tmpK, by.x = 'sta', by.y = 'sta', all.x = T, all.y = F)

  # Reorder from US -> DS
  tmpK <- tmpK[order(tmpK$q2kR), ]
  
  out[[1]]$rear <- as.numeric(tmpK$k600)

  # LSWCD DATA ----
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

  # CONTINUOUS DEQ DATA ----
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

  # DEQ GRAB DATA ----
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

  deqG <- merge(deqG, rows[, c(1, 5)], by.x = 'STAID', by.y = 'sta')

  # Iterate through the parameters that have censor methods
  for (i in 3 : 6) {

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

  tmp4 <- list(mean = aggregate(deqG[, 3 : 10], by = list(deqG$STAID), FUN = 'mean', na.rm = T),
               min = aggregate(deqG[, 3 : 10], by = list(deqG$STAID), FUN = 'min', na.rm = T),
               max = aggregate(deqG[, 3 : 10], by = list(deqG$STAID), FUN = 'max', na.rm = T))
  
  rchC <- tmp4[[1]]$q2kR
  
  # The 'FROM' columns: OrgP, PO4, TOC, OrgN, NH3, NOx, CBOD, CHLA
  mCol <- c(13, 14, 16, 10, 11, 12, 9, 15)
  
  # Columns to convert to micrograms
  cCol <- c(3, 4, 6, 7, 8)

  for (i in 1 : 3) {

    # Calculate organic N and P 
    tmp4[[i]]$TP_r <- tmp4[[i]]$TP_r - tmp4[[i]]$PO4_r; names(tmp4[[i]])[2] <- 'OrgP'
    
    tmp4[[i]]$TKN_r <- tmp4[[i]]$TKN_r - tmp4[[i]]$NH3_r; names(tmp4[[i]])[5] <- 'OrgN'
        
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

  # ________________________________________________________________________----
  # EXTRACT DIEL DATA ----
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
  # OUPUT DATA ----
  # Replace Inf and -Inf with NA, replace 0 with NA
  stCl <- c(2, 2, 3, 3, 3, 4, 1)

  for (i in 1 : length(out)) {
    
    for (j in stCl[i] : length(out[[i]])) {
      
      out[[i]][, j] <- ifelse(out[[i]][, j] == 0, NA, out[[i]][, j])
      
    }
    
    write.csv(x = out[[i]], file = paste0(fOut, names(out)[i], dOut, '.csv'),
              row.names = F)
    
  }
}

# Impute ND relationships ----
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
