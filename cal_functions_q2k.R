# ________________________________________________________________________----
# CORE OUTPUT FUNCTIONS

# CREATE PEST OBS OBJECT ----
obs4PEST <- function(strD = NULL, endD = NULL) {
  
  # This function creates an observations object (list) for use in PEST based 
  # on the amalgamation of data from various sources. The object contains
  # observation data including, the unique ID code, date/time, model reach,
  # obs value, and PEST observation group.
  # Input the start and end dates for the output file; Also creates an output
  # dataframe that is to be used in collated the model output data file.
  
  # ________________________________________________________________________----
  # Libraries, Functions and Objects----
  suppressMessages(library(reshape2)); suppressMessages(library(dplyr))
  
  # ________________________________________________________________________----
  # Load And Process Data----  
  # Load observation data
  d <- load_obs(strD, endD); d <- d[2 : 5] # Don't need flow data so adios!
  
  # Load reach and station links
  rows <- read.csv('D:/siletz_q2k/05_calib/rhdr.csv', stringsAsFactors = F)
  
  # ________________________________________________________________________----
  # Create A Master Keys Of Obs Id List----
  obID <- create_key(strD = strD, endD = endD)
  
  max <- nrow(obID)
  
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
  
  nmes <- unique(substr(obID$obID, 4, 6))
  
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
  
  # Oops! Convert chlorophyll a back to ug/L (from mg/L)
  deqG[, 11] <- deqG[, 11] * 1000
  
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

# FORMAT OBS AND OUTPUT TO FILE FOR .PST ----
output_obs <- function(df = NULL, fOut = NULL) {
  
  # This function takes a data frame of observations (with ID, Grp, and Val) and 
  # formats it to output for inclusion in the pest control (.pst) file
  # Assumes a ID length of 12 and a group length of 3
  
  suppressMessages(library(dplyr))
  
  df$val <- format(df$val, digits = 9, scientific = T)
  
  df$wgt <- format(df$wgt, digits = 9, scientific = T)
  
  if (is.null(fOut)) {return(df)} else {
    
    write.table(df, fOut, quote = F, row.names = F, col.names = F)  
    
  }
}

# CREATE PEST INSTRUCTION FILE ----
ins4PEST <- function(obID = NULL, iOut = NULL) {
  
  # This function also creates the instruction file that PEST uses to access the
  # model data in the .ins file. Specify a vector of observation IDs (master key)
  obID <- append('pif $', paste0('l1 [', obID, ']14:29'))
  
  write.table(x = obID, file = iOut, quote = F, row.names = F, col.names = F)
  
}

# CREATE PEST MOD OBJECT ----
mod4PEST <- function(mOut = NULL, obID = NULL, strD = NULL, fOut = NULL) {
  
  # This function:
  # 1) Reads the Qual2Kw output file
  # 2) Processes the data
  # 3) Preps the data for output
  # 4) Writes the data to file to be read by the PEST instruction file
  # Specify: output file directory (mOut) and the observations IDs (obID) 
  
  # ________________________________________________________________________----  
  # Load and process model data ----
  suppressMessages(library(dplyr))
  suppressMessages(library(reshape2))
  suppressMessages(library(lubridate))
    
  dt <- read_q2k_out(mOut)

  dt <- dt[, c(  1,   3,   4,   7,  24,  59,  12,  31,  35,   9,  15)]
  
  names(dt) <- c('rch', 'tme', 'tmp', 'doc', 'phX', 'rea', 'nox', 'tpX', 'toc',
                 'bod', 'cha')  

  # Convert nitrate and phosphate from ug/L to mg/L
  for (i in 7 : 8) {dt[, i] <- dt[, i] / 1000}

  # Add a leading 0 to the reaches
  dt$rch <- addZ(dt$rch)
  
  # Time - convert from days to seconds and convert to POSIXct
  dt$tme <- as.POSIXct(dt$tme * 86400, origin = strD, tz = 'America/Los_Angeles') +
            hours(7)
  
  # Melt to long
  dt <- melt(dt, id.vars = c('tme', 'rch'), variable.name = 'par', value.name = 'val')

  # Separate reaeration for aggregation
  dtR <- dt[which(dt$par == 'rea'), ]; dtE <- dt[-which(dt$par == 'rea'), ]

  # Make the time into the units to which the data will be aggregated
  dtR$tme <- floor_date(dtR$tme, 'day'); dtE$tme <- floor_date(dtE$tme, 'hour')

  # Aggregate to mean hourly values for all but reaeration
  dtE <- aggregate(dtE$val, by = list(dtE$tme, dtE$rch, dtE$par),
                   'mean', na.rm = T)

  # Aggregate to mean daily values for reaeration
  dtR <- aggregate(dtR$val, by = list(dtR$tme, dtR$rch, dtR$par),
                   'mean', na.rm = T)

  dtR$Group.1 <- dtR$Group.1 + hours(12)
  
  dt <- rbind(dtE, dtR); names(dt) <- c('date', 'q2kR', 'par', 'val')
  
  # Create the mdID for merging with obID
  dt$mdID <- paste0('r', dt$q2kR, dt$par, addZ(month(dt$date)),
                    addZ(day(dt$date)), addZ(hour(dt$date)))
  
  obID <- data.frame(obID = obID)
  
  # Merge the two obID with the mdID
  dt <- merge(obID, dt, by.x = 'obID', by.y = 'mdID', all.x = T, all.y = F)
  
  # Create a df to order after merging
  # For all WQ
  # ord <- data.frame(par = c('tmp', 'doc', 'phX', 'rea', 'nox', 'tpX', 'toc',
  #                           'bod', 'cha'), ord = c(1, 2, 3, 4, 5, 6, 7, 8, 9))
  
  # For temperature only
  ord <- data.frame(par = c('tmp', 'rea'), ord = c(1, 2))
  
  dt <- merge(dt, ord)

  dt <- dt %>% arrange(ord, q2kR, date)
  
  # ________________________________________________________________________----
  # CREATE THE OUTPUT (.out) FILE
  mOut <- paste0(dt$obID, '  ', format(dt$val, digits = 10, scientific = T))
  
  write.table(mOut, fOut, quote = F, row.names = F, col.names = F)

}

# WATER QUALITY CALIBRATION FUNCTIONS ----
cal_supp <- function(strD = NULL, mOut = NULL, oOut = NULL, nDir = NULL) {
  
  # functions to assess and plot measured and modeled Qual2Kw
  # Arguments include the path to the folder with modeled output files and
  # observation files

  suppressMessages(library(ggplot2)); suppressMessages(library(lubridate))
  
  # ________________________________________________________________________----    
  # Load and organize the data ----
  # Bring in station reach match
  rows <- read.csv('D:/siletz_q2k/05_calib/rhdr.csv', stringsAsFactors = F)
  
  # Model data first!
  mOut <- read_q2k_out(mOut)
  
  mOut <- mOut[, c(  1,   3,   4,   7,  24,  59,  12,  31,  35,   9,  15)]
  
  names(mOut) <- c('rch', 'tme', 'tmp', 'doc', 'phX', 'rea', 'nox', 'tpX', 'toc',
                   'bod', 'cha')
  
  # Convert nitrate and phosphate from ug/L to mg/L
  for (i in 7 : 8) {mOut[, i] <- mOut[, i] / 1000}
  
  # Add a leading 0 to the reaches
  mOut$rch <- addZ(mOut$rch)
  
  # Time - convert from days to seconds and convert to POSIXct
  mOut$tme <- as.POSIXct(mOut$tme * 86400, origin = strD, tz = 'America/Los_Angeles') +
              hours(7)
  
  # Melt to long
  mOut <- melt(mOut, id.vars = c('tme', 'rch'), variable.name = 'par', value.name = 'val')
  
  mOut$par <- as.character(mOut$par)
  
  # Observations - already bringing in a formated df, but just change the names
  names(oOut) <- c('tme', 'rch', 'par', 'val')

  # Create a column in both for source (model/observation); bind the tables
  mOut$src <- 'mod'; oOut$src <- 'obs'; dt <- rbind(mOut, oOut)
  
  # Fix TP and pH for Obs -- switch x w/ X 
  dt$par <- ifelse(dt$par == 'tpx', 'tpX', ifelse(dt$par == 'phx', 'phX', dt$par))

  # Parameter switch
  pars <- data.frame(pr1 = c('tmp', 'doc', 'phX', 'rea', 'nox', 'tpX', 'toc', 'bod', 'cha'),
                     pr2 = c('Temperature', 'Dissolved Oxygen', 'pH', 'Reaeration',
                             'Nitrate', 'Total Phosphorus', 'Total Organic Carbon',
                             'Carbon BOD', 'Chlorophyll a'),
                     unt = c('oC', 'mg/L', 'su', '/day', 'mg/L', 'mg/L', 'mg/L',
                             'mg/L', 'ug/L'))
  
  pars <- pars[order(pars$pr1), ]

  # Create three different columns: mean/min/max for longitudinal plots
  dd <- dt; dd$dte <- floor_date(dd$tme, 'day')
  
  fncs <- c('mean', 'min', 'max'); ds <- list()
  
  for (i in 1 : length(fncs)) {
    
    ds[[i]] <- aggregate(dd$val, by <- list(dd$rch, dd$dte, dd$par, dd$src),
                         fncs[i], na.rm = T)
    
    ds[[i]]$stt <- fncs[i]
    
  }

  dd <- rbind(ds[[1]], ds[[2]], ds[[3]])
  
  names(dd) <- c('rch', 'dte', 'par', 'src', 'val', 'stt')

  # Bring in river mile for plotting
  rows$q2kR <- addZ(rows$q2kR)
  
  dt <- merge(dt, rows[, c(3, 4, 1)], by.x = 'rch', by.y = 'q2kR', all.x = T,
              all.y = T)
  
  dt <- dt[order(dt$par), ]
  
  dd <- merge(dd, rows[, c(3, 4, 1)], by.x = 'rch', by.y = 'q2kR', all.x = T,
              all.y = T)
  
  # Remove last day (2017-07-22)
  dd <- dd[which(dd$dte != max(dd$dte)), ]; dd <- dd[order(dd$par), ]

  # ________________________________________________________________________----    
  # Plot! ----
  for (i in 1 : length(unique(dt$par))) {

    # Time series graphs (facet station)
    datM <- dt[which(dt$par == unique(dt$par)[i] & dt$src == 'mod'), ]
    datO <- dt[which(dt$par == unique(dt$par)[i] & dt$src == 'obs'), ]

    plt1 <- ggplot(dat = datM, aes(x = tme, y = val)) +
            geom_line(color = 'darkblue', size = 1.1) +
            ylab(paste0(pars[i, 2], ' (', pars[i, 3], ')')) +
            theme_bw() + theme(axis.title.x = element_blank()) +
            facet_wrap(. ~ rch, ncol = 2, labeller = label_both) +
            geom_point(data = datO, aes(x = tme, y = val),
                       color = 'darkred', stroke = 1.1, shape = 5, size = 0.9)

    ggsave(filename = paste0('ts_', pars[i, 1], '.jpg'), plot = plt1, width = 15,
           height = 10, path = nDir, units = 'in', dpi = 300)

    # Longitudinal graphs (facet day) x = rch, y = val, facet = dte, group = stt
    ddM <- dd[which(dd$par == unique(dd$par)[i] & dd$src == 'mod'), ]
    ddO <- dd[which(dd$par == unique(dd$par)[i] & dd$src == 'obs'), ]

    plt2 <- ggplot(dat = ddM, aes(x = dst, y = val, group = stt)) +
            geom_line(color = 'darkblue', size = 1.1) +
            ylab(paste0(pars[i, 2], ' (', pars[i, 3], ')')) +
            theme_bw() + facet_wrap(. ~ dte, ncol = 2) +
            geom_point(data = ddO, aes(x = dst, y = val, group = stt),
                       color = 'darkred', stroke = 1.2, shape = 5, size = 1.1)

    ggsave(filename = paste0('long_', pars[i, 1], '.jpg'), plot = plt2, width = 15,
           height = 10, path = nDir, units = 'in', dpi = 300)

  }

  # ________________________________________________________________________----    
  # Calibration metrics ----
  
  
}

# ________________________________________________________________________----
# SUPPLEMENTAL OUTPUT FUNCTIONS

# LOAD OBS DATA 4 Q2KW CAL (PEST OR AUTOCAL) ----
load_obs <- function(strD = NULL, endD = NULL) {

  # Returns a list of 5 data frames with the following observation data:
  # 1) Reach flows per basin from HSPF
  # 2) Reaeration rates for each station based on StreamMetabolizer data
  # 3) Lincoln County SWcD data which includes continuous temp and DO
  # 4) DEQ continuous data which includes pH and conductivity
  # 5) DEQ grab sample data which includes N, P, C, chl and CBOD
  
  suppressMessages(library(lubridate)); suppressMessages(library(streamMetabolizer))
    
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
  
  suppressMessages(library(dplyr))

  # CHANGE ME __________________________________________________________________
  # Change me if the groups or the objective function group weights change    
  relWgt <- data.frame(grp = c("tmp", "doc", "phX", "rea", "nox",
                               "tpX", "toc", "bod", "cha"),
                       rel = c(   27,    45,    18,     1,     2,
                                   2,     1,     1,     2),
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

# CREATE MASTER KEY ----
create_key <- function(strD = NULL, endD = NULL) {
  
  # This function creates a master observation ID key set for DO, temperature
  # pH, and reaeration based on specified start and end dates. Assumes DO, temp
  # and pH are all continuous (hourly) and reaeration is daily (mean)

  # make one of all continuous obs, and one of daily mean rear
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
  
  obID$indx <- 1 : nrow(obID)
  
  return(obID)
  
}

# ADD A LEADING ZERO ----
addZ <- function(v) {
  
  # Function to add a leading 0 to a vector of #s if a number is less than 10
  # This is for vectors of minutes, hours, days, and months where there are only 
  # one or two digits

  ifelse(v < 10, paste0(0, v), as.character(v))
  
}

# READ Q2K OUTPUT ----
read_q2k_out <- function(mOut) {
  
  # Function that reads 

  if (substr(mOut, nchar(mOut), nchar(mOut)) != '/') {mOut <- paste0(mOut, '/')}
  
  mOut <- paste0(mOut, 'dynamic_MC', c('a', 'b'), '.txt')
  
  # READ IN THE DATA
  dt <- list(f1 = readLines(mOut[1]), f2 = readLines(mOut[2]))
  
  for (i in 1 : 2) {
    
    # Split into columns  
    dt[[i]] <- data.frame(do.call("rbind", strsplit(dt[[i]], "\\s+")),
                          stringsAsFactors = F)
    
    # Remove the first row and rename the columns to row 1
    dt[[i]] <- dt[[i]][, -1]; names(dt[[i]]) <- dt[[i]][1, ]
    
    # Remove rows 1 & 2
    dt[[i]] <- dt[[i]][-(1 : 2), ]
    
    # Make values numeric
    dt[[i]] <- data.frame(apply(dt[[i]], MARGIN = 2, FUN = as.numeric),
                          stringsAsFactors = F)
  }
  
  # Combine into one DF and keep the necessary columns and rename
  dt <- cbind(dt[[1]], dt[[2]])
  
  return(dt)

}

# REMOVE OBS ----
remove_obs <- function(dtes = NULL, parm = NULL, q2kR = NULL, obID = NULL) {
  
  # Removes observations from the observation object and creates a third df
  # in the object that contains the removed entries. Arguements:
  # 1) vector of date/times for removal
  # 2) parameter for removal (only do one parameter at a time)
  # 3) reach (only do one at a time)
  # 4) observations object
  
  library(lubridate)
  
  if (!is.POSIXct(dtes)) {
    dtes <- as.POSIXct(dtes, '%Y-%m-%d %H:%M', tz = 'America/Los_Angeles')
  }
  
  # Parse out the entries for the dates, parameter, and reach condition
  cond <- which(obID[['obs']]$date %in% dtes & obID[['obs']]$q2kR %in% q2kR &
                obID[['obs']]$grp %in% parm)
  
  # Check to see if the removed df is empty, if so, create, if not, append
  if (!is.null(obID[['obRM']])) {
    
    obID[['obRM']] <- rbind(obID[['obRM']], obID[['obs']][cond, ])
    
  } else {
  
    obID[['obRM']] <- obID[['obs']][cond, ]
    
  }

  # Remove the entries from the observation data frame
  obID[['obs']] <- obID[['obs']][-cond, ]
  
  return(obID)
  
}

# CREATE OUPUT PLOTS ----
run_plots <- function(nDir = NULL, mOut = NULL) {
  
  # This function creates output plots (time series and longitudinal) for most
  # recent run an output files in the PEST folder (mOut). It creates a directory 
  # basedon directory name variable (nDir), then outputs to that directory. 
  # Highly customized and needs abstraction for broader use.
  
  nDir <- paste0('D:/siletz_q2k/06_figures', '/', nDir)
  
  if (dir.exists(nDir)) {
    
    cat('Oops! Directory already exists. Please try again.')
    
    break
    
  } else {
    
    dir.create(nDir)  
    
  }
  
  # RUN SUPPLEMENTAL CALIBRATION SCRIPTS ----
  strD <- '2017-07-17'; endD <- '2017-07-22'
  
  obs_CW <- obs4PEST(strD, endD)
  
  # REMOVE OBSERVATIONS ----
  dtes <- c('2017-07-20 15:00', '2017-07-20 16:00', '2017-07-20 17:00')
  
  obs_CW <- remove_obs(dtes = dtes, parm = 'phX', q2kR = '05', obID = obs_CW)
  
  dtes <- c('2017-07-20 17:00', '2017-07-20 18:00')
  
  obs_CW <- remove_obs(dtes = dtes, parm = 'phX', q2kR = '09', obID = obs_CW)
  
  # Reduce the data frame and plot!
  oOut <- obs_CW[['obs']][, c(2, 3, 6, 5)]
  
  x <- cal_supp(strD, mOut, oOut, nDir)
  
}

# Q-V-W HEATSOURCE RATING CURVE ----
rating_curves <- function(df = NULL, par = NULL, file = NULL, trns = NULL) {
  
  # Arguements: data frame of velocity and flow data
  # Outputs: list of intercept, slope, coefficient of determination
  # Optional variables include:
  # 1) filename (with path) for a graph output of RG
  # 2) transformation of variables.
  # Transformation options include
  # 1) 'none' for no transformation
  # 2) 'NL' for a log (base 10) transformation of the response variable
  # 3) 'LN' for a log (base 10) transformation of the indicator variable
  # 4) 'LL' for a log (base 10) transformation of both variables
  # Assumes data frame where flow is first variable, and velocity the second
  
  library(ggplot2)
  
  names(df) <- c('q', 'var') # Rename for convenience using specified parameter
  
  # If a transformation is specified (trns) then transform the data
  if (!is.null(trns)) {

    if (trns == 'none') { # No transformation
      
      rg <- summary(lm(df$var ~ df$q))
      
      tFun <- function(a, b, x) a * x + b
      
    } else if (trns == 'NL') { # Normal (Ind) - Log (Resp)
      
      rg <- summary(lm(log10(df$var) ~ df$q))
      
      tFun <- function(a, b, x) 10^(a * x + b)
      
    } else if (trns == 'LN') { # Log (Ind) - Normal (Resp)
      
      rg <- summary(lm(df$var ~ log10(df$q)))
      
      tFun <- function(a, b, x) a * log10(x) + b
      
    } else if (trns == 'LL') { # Both Log
      
      rg <- summary(lm(log10(df$var) ~ log10(df$q)))
      
      tFun <- function(a, b, x) 10^b * x^a
      
    } else if (trns == 'NS') { # Normal (Ind) - SQRT (Resp)
      
      rg <- summary(lm(sqrt(df$var) ~ df$q))
      
      tFun <- function(a, b, x) (a^2 * x^2) + (2 * a * b * x) + b^2
      
    } else if (trns == 'SN') { # SQRT (Ind) - Normal (Resp)
      
      rg <- summary(lm(df$var ~ sqrt(df$q)))
      
      tFun <- function(a, b, x) a * sqrt(x) + b
      
    } else if (trns == 'SS') { # Both SQRT
      
      rg <- summary(lm(sqrt(df$var) ~ sqrt(df$q)))
      
      tFun <- function(a, b, x) (a^2 * x) + (2 * a * b * sqrt(x)) + b^2
      
    } 

    else {print('Please enter a valid transformation.'); break}
  }  

  if (!is.null(file)) {

    x <- seq(min(df$q) * 0.98, max(df$q) * 1.02,
             (max(df$q) * 1.02 - min(df$q) * 0.98) / 100)

    line <- data.frame(x = x, y = tFun(rg[[4]][2, 1], rg[[4]][1, 1], x))

    plot <- ggplot(df, aes(x = q, y = var)) + theme_bw() +
            geom_point(size = 1.5, shape = 23, color = 'darkred',
                       stroke = 1.0, fill = 'yellow') + 
            geom_line(data = line, aes(x = x, y = y))

    ggsave(filename = file, plot = plot, width = 16, height = 9, units = 'in')
    
  }

  # Return the trasnformation linear function, coefficients, and R2 value  
  ls <- list(parameter = par,
             tFun = tFun,
             coef = c(a = rg[[4]][2, 1], b = rg[[4]][1, 1]),
             r2   = rg[["adj.r.squared"]])
  
  return(ls)

}
