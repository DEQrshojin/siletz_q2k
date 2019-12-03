# Suite of functions to create boundary condition input dfs for Qual2Kw for
# reach 5 and lateral inflows 6 for steady state analyses. Piggy-backs on the
# functions created for the dynamic simulations.

# __________________________________________________________________________----
# HSPF BC INPUTS ----
hspf_q2k_ss <- function(sOut = NULL, dir = NULL) {
  
  source('D:/siletz/scripts/R/utilities.R')
  
  dir <- iDir
  
  pars <- c('NOx', 'NH3', 'TKN', 'PO4', 'TP', 'OrC')     # HSPF parameters
  
  cPut <- c(12, 11, 10, 14, 13, 16)     # HSPF -> Q2K columns
  
  f <- list(r = paste0(dir, '/rchQLC_', pars, '.RData'), # 
            l = paste0(dir, '/latQLC_', pars, '.RData'))
  
  for (i in 1 : length(f[[1]])) {
  
    # Read in the reach and lateral inflow data
    qlc <- list(r = readRDS(f[[1]][i]), l = lQLC <- readRDS(f[[2]][i]))
    
    qlc[[1]][[3]] <- NULL; names(qlc[[1]]) <- names(qlc[[2]])
    
    # Reduce each data frame down to the specified dates
    for (j in 1 : 2) {   # Iterate first on source (lateral or reach)
      for (k in 1 : 3) { # Then iterate on the componenet (q, load, or conc)
        qlc[[j]][[k]] <- reduce_qlc(strDte = sOut[[1]]$date[1],
                                    endDte = sOut[[1]]$date[nrow(sOut[[1]])],
                                    df2Red = qlc[[j]][[k]])
      }
    }

    # Populate flow first
    if (i == 1) {for (j in 1 : 2) {sOut[[j]]$qIn_cms <- qlc[[j]][[1]][, j + 5]}}

    # Populate parameters and convert to ug/L
    for (j in 1 : 2) {
      if (i != length(f[[1]])) {
        sOut[[j]][, cPut[i]] <- qlc[[j]][[3]][, j + 5] * 1000
      } else {
        sOut[[j]][, cPut[i]] <- qlc[[j]][[3]][, j + 5]
      }
    }
  }
  
  # Calculate organic N & P
  for (i in 1 : 2) {
    
    sOut[[i]]$orn_ugL <- ifelse(sOut[[i]]$orn_ugL - sOut[[i]]$nh3_ugL < 0,
                                0, sOut[[i]]$orn_ugL - sOut[[i]]$nh3_ugL)
    
    sOut[[i]]$orp_ugL <- ifelse(sOut[[i]]$orp_ugL - sOut[[i]]$po4_ugL < 0,
                                0, sOut[[i]]$orp_ugL - sOut[[i]]$po4_ugL)
    
  }
  
  return(sOut)
    
}

# __________________________________________________________________________----
# PROCESS LSWCD DATA ----
lswcd_q2k_ss <- function(sOut = NULL, dir = NULL) {
  
  # Reads the LSWCD data and returns the Q2K boundary condition object with
  # DO and temperature boundaries.
  # Pass the BC object (sOut) and location of the HSPF data (.RData) files (dir)
  
  library(dplyr)
  
  lswc <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxyge',
                          'n/Middle_Siletz_River_1710020405/001_data/wq_data/Mon',
                          'itoring 2017/LSWCD/Lincoln_SWCD_SILETZ RIVER_06292017',
                          '-01052018/siletz_volmon_cont_data.csv'))
  
  lswc$DATE.TIME <- as.POSIXct(lswc$DATE.TIME, '%m/%d/%Y %H:%M',
                               tz = 'America/Los_Angeles')
  
  # Import the regression parameters, relate all stations to Moonshine station
  regs <- read.csv("D:/siletz_q2k/02_input/ctsi/summary_NN.csv")
  
  # Re-order based on reach order
  regs <- regs %>% arrange(rch)
  
  # Isolate the Moonshine station data -- these data will form the basis of the
  # other temperature boundary conditions, including the headwaters.
  moon <- lswc[which(lswc$STAID == 37396 & lswc$DATE.TIME %in% cOut[[1]]$date), ]
  
  # Perform the correlations
  sOut[[1]]$tmp_dgC <- regF(m = regs$m[4], b = regs$b[4], x = moon$TEMP_C)
  
  sOut[[1]]$do_mgL <- do_sat(temp = sOut[[1]]$tmp_dgC, elev = regs$elv[4])
    
  sOut[[2]]$tmp_dgC <- sOut[[1]]$tmp_dgC; sOut[[2]]$do_mgL <- sOut[[1]]$do_mgL
    
  return(sOut)
  
}

# __________________________________________________________________________----
# PROCESS DEQ CONT DATA ----
deq_cont_q2k_ss <- function(sOut = NULL) {
  
  library(reshape2); library(lubridate); library(ggplot2)
  
  tz = 'America/Los_Angeles'
  
  # LOAD DATA
  deqC <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxy',
                          'gen/Middle_Siletz_River_1710020405/001_data/wq_data',
                          '/deq_cont_2017.csv'), stringsAsFactors = F)
  
  deqC$datetime <- as.POSIXct(paste0(deqC$DATE, ' ', deqC$TIME1), '%m/%d/%Y %H:%M',
                              tz = tz)
  
  # Parse out the moonshine data
  deqC <- deqC[which(deqC$SITENAME == '37396-SiletzMoon'), ]
  
  # Create a season break date
  strD <- min(cOut[['HW']]$date); endD <- max(cOut[['HW']]$date)
  
  coDt <- as.POSIXct('2017-09-01', '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  if (strD < coDt) {
    deqC <- deqC[which(deqC$datetime < coDt), ]
  } else {
    deqC <- deqC[which(deqC$datetime >= coDt), ]
  } 
  
  # _____________________________________________________________________________
  # CREATE pH TIMESERIES
  # Reshape the data and create the daily periodicity values
  pHdt <- dcast(data = deqC[ , c(13, 3, 8)], formula = datetime ~ SITENAME,
                value.var = 'pH', fun.aggregate = mean)
  
  # FOR COLD-WATER Remove data before 7/17 16:00 and after 7/20 14:30
  if (strD < coDt) {pHdt <- pHdt[-c(293 : nrow(pHdt)), ]}
  
  t1 <- data.frame(date = seq(strD, endD, 900))
  
  pHdt <- merge(t1, pHdt, by.x = 'date', by.y = 'datetime', all.x = T)
  
  # Add new columns to contain the sinusoidal regression; rename
  pHdt <- cbind(pHdt, matrix(data = 0, nrow = nrow(pHdt), ncol = 1))  
  
  names(pHdt)[3] <- paste0('reg_', names(pHdt)[2])
  
  # Add a column for periodicity
  pHdt$per <- (hour(pHdt$date) + minute(pHdt$date) / 60) / 24
  
  # Calculate the sinusoidal coefficients and the lines of best fit
  c <- lm(pHdt[, 2] ~ sin(2 * pi * pHdt$per) + cos(2 * pi * pHdt$per) +
                      sin(4 * pi * pHdt$per) + cos(4 * pi * pHdt$per) +
                      sin(6 * pi * pHdt$per) + cos(6 * pi * pHdt$per))
  
  pHdt[, 3] <- c[[1]][1] + c[[1]][2] * sin(2 * pi * pHdt$per) +
                           c[[1]][3] * cos(2 * pi * pHdt$per) +
                           c[[1]][4] * sin(4 * pi * pHdt$per) +
                           c[[1]][5] * cos(4 * pi * pHdt$per) +
                           c[[1]][6] * sin(6 * pi * pHdt$per) +
                           c[[1]][7] * cos(6 * pi * pHdt$per)

  # Assign pH by basin (geographical proxy): ODEQ-37396 (MOON) - B03, B06
  # First reduce to hourly
  ts <- seq(strD, endD, 3600)
  
  pHdt <- pHdt[which(pHdt$date %in% ts), c(1, 3)]
  
  sOut[[1]][, 20] <- sOut[[2]][, 20] <- pHdt[, 2]
  
  # _____________________________________________________________________________
  # CREATE CONDUCTIVITY TIME SERIES
  cdDt <- dcast(data = deqC[ , c(13, 3, 5)], formula = datetime ~ SITENAME,
                value.var = 'COND', fun.aggregate = mean)

  # Assign Cond as per the pH
  sOut[[1]][, 5] <- sOut[[2]][, 5] <- mean(cdDt[1 : 292, 2], na.rm = T)

  # ASSIGN ALKALINITY (@ 20 mg/L CaCO3)
  sOut[[1]][, 19] <- sOut[[2]][, 19] <- 20
  
  # No phytoplankton from tribs
  
  # CBOD fast - Set tribs, HW, and IC = 0.1 mg/L. No better information!!!
  sOut[[1]][, 9] <- sOut[[2]][, 9] <- 0.1 * 1.46

  return(sOut)
  
}

# __________________________________________________________________________----
# PROCESS DEQ GRAB DATA ----
# No meaningful grab data for Reach 5 and Lateral 6

# __________________________________________________________________________----
# WRITE BCs TO CSVs ----
write_bcs_q2k_ss <- function(sOut = NULL, oPth = NULL, sveRDS = NULL,
                             addSfx = NULL) {
  
  # Function to output the Rch5/Lat6 bc object elements to csv files for input
  # to Q2K. Arguements include the bc object, output directory, optional name
  # to save bc object as RDS (default = NO), and an optional suffix to add
  
  # Add forward slash to file path if not given
  if (substr(oPth, nchar(oPth), nchar(oPth)) != '/') {oPth <- paste0(oPth, '/')}

  # Make Inflow Boundary Conditions 
  sOut[[1]]$Src <- 'R05'; sOut[[2]]$Src <- 'L06'
  
  sOut[[1]] <- sOut[[1]][, c(21, 1 : 20)]; sOut[[2]] <- sOut[[2]][, c(21, 1 : 20)]
  
  ssBC <- rbind(sOut[[1]], sOut[[2]])

  # WRITE TO CSV 
  if (is.null(addSfx)) {
    oFil <- paste0(oPth, 'add_R5L6_', '.csv')  
  } else {
    oFil <- paste0(oPth, 'add_R5L6_', addSfx, '.csv')  
  }
  
  # Write Inflow BCs
  write.csv(x = ssBC, file = oFil, row.names = F)
  
  if (!is.null(sveRDS)) {
    saveRDS(object = sOut, file = paste0(oPth, 'add_R5L6_', sveRDS, '.RData'))
  }

}

# __________________________________________________________________________----
# RECOMBINE STEADY STATE CSVs ----
recombine_ss_bsc <- function(dir = NULL) {

  library(dplyr)
  
  options(stringsAsFactors = F)
  
  # Search the file names of the files in dir
  fils <- list.files(path = dir)
  
  # Filter for .csv only and files with inflows_ and add_R5L6
  fils <- fils[which(substr(fils, 1, 8) == 'inflows_' |
                     substr(fils, 1, 8) == 'add_R5L6' &
                     substr(fils, nchar(fils) - 5, nchar(fils)) != '.RData')]

  # Check the end of the directory for forward slash
  if (substr(dir, nchar(dir), nchar(dir)) != '/') {dir <- paste0(dir, '/')}

  # Read the files
  bc <- read.csv(paste0(dir, fils[2])); ad <- read.csv(paste0(dir, fils[1]))
  
  # Remove HSPF B06 from bc -- to be replaced by Sunshine and Lat6 flows
  bc <- bc[-which(bc$Reach == 6), ]
  
  # Basin partitions for point source (streams), remainder as diffuse runoff
  #                          B03,  B07,  B08,  B11,  B12,  B13,  B14,  B15,  B16 
  part <- data.frame(bsn = c(  3,    7,    8,   11,   12,   13,   14,   15,   16),
                     ps = c(0.00, 0.65, 1.00, 0.58, 0.00, 0.00, 0.00, 0.64, 0.83),
                     stringsAsFactors = F) %>% mutate(nps = 1 - ps)
  
  psbc <- bc; npbc <- bc
  
  for (i in 1 : nrow(part)) {
    
    # Partition out the flows based on fraction of PS/NPS
    psbc[which(psbc$Reach == part$bsn[i]), 4] <-
      psbc[which(psbc$Reach == part$bsn[i]), 4] * part$ps[i]
    
    npbc[which(npbc$Reach == part$bsn[i]), 4] <- 
      npbc[which(npbc$Reach == part$bsn[i]), 4] * part$nps[i]

    # Cull reaches for which the PS/NPS contribution is 0 (base on the part DF)
    if (part$ps[i] == 0) {
      psbc <- psbc[-which(psbc$Reach == part$bsn[i]), ]
    } else if (part$ps[i] == 1) {
      npbc <- npbc[-which(npbc$Reach == part$bsn[i]), ]
    }
    
  }
  
  # Add back in Sunshine Creek and Basin 6 Lateral flows
  ad$Src <- as.numeric(substr(ad$Src, 3, 3)); names(ad)[1] <- 'Reach'
  
  psbc <- rbind(psbc, ad[which(ad$Reach == 5), ])
  
  npbc <- rbind(npbc, ad[which(ad$Reach == 6), ])
  
  # Now order these by ascending reach number
  psbc <- psbc %>% arrange(Reach, date); npbc <- npbc %>% arrange(Reach, date)
  
  # Process both PS and NPS sources differently. Start with NPS (daily averages)
  npbc <- npbc[, c(1, 3 : 21)] %>% group_by(Reach) %>% summarize_all(funs(mean))
  
  write.csv(npbc, "D:/siletz_q2k/02_input/ss_wSTP_20191126/nps_bcs.csv", row.names = F)
  
  # Process PS Need to find the daily mean, range/2, and the time of max
  # Mean
  psMn <- psbc[, c(1, 3 : 21)] %>% group_by(Reach) %>% summarize_all(funs(mean))
  
  # Range/2
  psMi <- psbc[, c(1, 3 : 21)] %>% group_by(Reach) %>% summarize_all(funs(min))
  
  psMx <- psbc[, c(1, 3 : 21)] %>% group_by(Reach) %>% summarize_all(funs(max))
  
  psR2 <- psMx; for (i in 2 : length(psR2)) {psR2[, i] <- (psMx[, i] - psMi[, i]) / 2}
  
  # Time of max
  rch <- unique(psbc$Reach)
  
  psTx <- psR2; psTx[, 1] <- rch; psTx[, 2 : length(psTx)] <- NA
  
  for (i in 1 : length(rch)) {
    
    tmp1 <- psbc[which(psbc$Reach == rch[i]), ]
    
    for (j in 3 : length(tmp1)) {
      
      # Find the index of the time of the max, including duplicate times
      tInx <- as.integer(median(which(tmp1[, j] == max(tmp1[, j]))))

      psTx[i, j - 1] <- tmp1[tInx, 2]
      
    }
  }
  
  # Rename the columns
  names(psMn)[2 : length(psMn)] <- paste0('Avg_', names(psMn)[2 : length(psMn)])
  
  names(psR2)[2 : length(psR2)] <- paste0('Rg2_', names(psR2)[2 : length(psR2)])
  
  names(psTx)[2 : length(psTx)] <- paste0('TMx_', names(psTx)[2 : length(psTx)])
  
  # Now piece them all together
  psOp <- data.frame(rch = psTx$Reach)
  
  for (i in 2 : length(psTx)) {

    psOp <- data.frame(psOp, psMn[, i], psR2[, i], psTx[, i])
    
  }
  
  write.csv(psOp, "D:/siletz_q2k/02_input/ss_wSTP_20191126/ps_bcs.csv", row.names = F)

}

# Function to modify the boundar condition object for rch5/lat6 bcs
mod_bcs_ss <- function(cOut) {cOut <- list(R05 = cOut[[2]], L06 = cOut[[3]])}

