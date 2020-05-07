# Suite of functions to create boundary condition input dfs for Qual2Kw
# Pulls from several sets of disparate data

# __________________________________________________________________________----
# HSPF BC INPUTS ----
hspf_q2k <- function(cOut = NULL, dir = NULL, nme = NULL) {
  
  # Synopsis
  # Converts HSPF outputs to Qual2Kw inputs. Inputs include:
  # cOut = bc object (list) used to create bcs (by init_bcs()) for qual2kw
  # strD = start date of input data
  # endD = end date of input data (ends on hour 0 of the specified day)
  # dir = directory where the HSPF data are

  strD <- cOut[['HW']]$date[1]; endD <- cOut[['HW']]$date[nrow(cOut[['HW']])]
  
  chrDts <- as.character(c(strD, endD)) # Save as character dates for warning log

  # Set the parameter list for HSPF outputs ____________________________________
  par2 = c('Q', 'TKN', 'NH3', 'NOx', 'TP', 'PO4', 'OrC')

  # Create a vector of nutrient destination for initial conditions
  iCol <- list(from = c(B03 = 4, B06 = 7, B07 = 8, B08 = 9, B11 = 12,
                        B12 = 13, B13 = 14, B14 = 15, B15 = 16, B16 = 17),
               to   = c(TKN = 8, NH3 = 9, NOx = 10, TP = 11, PO4 = 12, OrC = 14))
  
  # Create a list of the basins from which to pull either lateral or reach data
  cBas <- list(HW  = list(L = c(1, 2), R = NULL), # Lat from B1 & B2
               B03 = list(L = 3, R = NULL),       # Lat from B3
               B06 = list(L = 6, R = 5),          # Lat from B6 and Rch from B5 
               B07 = list(L = 7, R = NULL),       # Lat from B7
               B08 = list(L = 8, R = 10),         # Lat from B8 and Rch from B10
               B11 = list(L = 11, R = NULL),      # Lat from B11
               B12 = list(L = 12, R = NULL),      # Lat from B12
               B13 = list(L = 13, R = NULL),      # Lat from B13
               B14 = list(L = 14, R = NULL),      # Lat from B14
               B15 = list(L = 15, R = NULL),      # Lat from B15
               B16 = list(L = 16, R = NULL),      # Lat from B16
               TW  = list(L = NULL, R = 16))      # Rch from B16
  
  # Create a vector of column # where specific constituent data should go
  cPut <- c(Q = 2, TKN = 9, NH3 = 10, NOx = 11, TP = 12, PO4 = 13, OrC = 15)
  
  cPut <- data.frame(rbind(HW = cPut, B03 = cPut + 1, B06 = cPut + 1,
                           B07 = cPut + 1, B08 = cPut + 1, B11 = cPut + 1,
                           B12 = cPut + 1, B13 = cPut + 1, B14 = cPut + 1,
                           B15 = cPut + 1, B16 = cPut + 1, TW = cPut - 1))
  
  # Loop through each constituent and extract the TS ___________________________
  for (i in 1 : length(par2)) {
    
    # Read in the HSPF output file _____________________________________________
    # What to do if Q is the only parameter selected
    if (par2[i] == 'Q') {var <- par2[i + 1]} else {var <- par2[i]}
    
    # Reach output = rchO; Lateral output = latO
    qlc <- list(lat = readRDS(paste0(dir, '/', nme, '_latQLC_', var, '.RData')),
                rch = readRDS(paste0(dir, '/', nme, '_rchQLC_', var, '.RData')))

    # Remove the reach concentration (Use the outflow concentration)
    qlc[['rch']][[3]] <- NULL
    
    # Pull out the time series data ____________________________________________
    # Reduce the flow/load/conc (qlc) data to the model timeframe
    for (j in 1 : 3) {
      for (k in 1 : 2) {
        qlc[[k]][[j]] <- reduce_qlc(strDte = chrDts[1], endDte = chrDts[2],
                                    df2Red = qlc[[k]][[j]])
      }
    }
    
    # Rename the elements in both lat and rch flows    
    names(qlc[['lat']]) <- names(qlc[['rch']]) <- c('flow', 'load', 'conc')
    
    # Transfer the data from the HSPF out file to the q2k input ________________
    # Identify the column INTO which data will go for HW, BCs, TW and InitC
    colP <- cPut[, which(names(cPut) == par2[i])]
    
    colQ <- cPut[, which(names(cPut) == 'Q')]
    
    # Iterate through boundary conditions list
    for (m in 1 : length(colP)) { # e.g., m = 1 is headwaters
      
      # Process reach and lateral flows 
      for (g in 1 : 2) { # g = 1 = lateral; g = 2 = U/S reach flows
        
        # Check for the existence of reach or lateral flows
        if (length(cBas[[m]][[g]]) != 0) {
          
          # Iterate through each lateral or U/S reach inflow
          for (n in 1 : length(cBas[[m]][[g]])) { # m = # of lat inflow sources
            
            if (par2[i] == 'Q') {elem <- 1} else {elem <- 2}
            
            if (m != length(cBas)) { # Sum the flows/loads for HW and inflows
              
              cOut[[m]][, colP[m]] <- cOut[[m]][, colP[m]] +
                                      qlc[[g]][[elem]][, cBas[[m]][[g]][n] + 1]
              
            } # Conditional to sum if inflows exist
          }   # n - lateral and upstream inflows
        }     # Conditional check for upstream and lateral inflows 
      }       # g - processing reach and lateral inflows
      
      # Calculate the concentration: C = L / (Q * 3.6)
      if (par2[i] != 'Q') { # Skip flows
        
        if (m != length(cBas)) { # Convert interim HW/lat loads to concentration
          
          cOut[[m]][, colP[m]] <- cOut[[m]][, colP[m]] / (cOut[[m]][, colQ[m]] * 3.6)
          
        } else { # Process for tailwater concentrations as concentrations
          
          cOut[[m]][, colP[m]] <- qlc[['rch']][['conc']][, cBas[[m]][[g]][n] + 1]
          
        }
        
        # If par2[i] = nutrient (not TOC) then convert from mg/L to ug/L
        if (par2[i] == 'TKN' | par2[i] == 'NH3' | par2[i] == 'NOx' | 
            par2[i] == 'TP'  | par2[i] == 'PO4') {
          
          cOut[[m]][, colP[m]] <- cOut[[m]][, colP[m]] * 1000
          
        }
      }
      
      # Process Organic N & P (subtract species from totals) ___________________
      checks <- list(N = c('NH3', 'TKN'), P = c('PO4', 'TP'))
      
      for (t in 1 : 2) { # Iterate through N then P to calculate Org N & P
        
        if (par2[i] == checks[[t]][1] & any(par2 == checks[[t]][2])) {
          
          col1 <- cPut[, which(names(cPut) == checks[[t]][2])][m]
          
          col2 <- cPut[, which(names(cPut) == checks[[t]][1])][m]
          
          cOut[[m]][, col1] <- cOut[[m]][, col1] - cOut[[m]][, col2]
          
          # Check and warn for negative concentrations
          if (any(cOut[[m]][, col1] < 0)) {
            
            print(paste0('Negative (-) organic ' , names(checks)[t],
                         ' concentration of ',
                         round(cOut[[m]][which(cOut[[m]][, col1] < 0), col1], 1),
                         ' ug/L in ', names(cBas)[m], ' at timestep: ',
                         cOut[[m]][which(cOut[[m]][, col1] < 0), 1]))
            
          }
        }        
      }
    } # m (bc)
    
    # Initial conditions _______________________________________________________
    if (par2[i] != 'Q') {
      
      cOut[['Init']][, iCol[['to']][par2[i]]] <-
        unlist(qlc[['rch']][['conc']][1, iCol[['from']]])
      
      if (par2[i] != 'OrC') {cOut[['Init']][, iCol[['to']][par2[i]]] <- 
        1000 * cOut[['Init']][, iCol[['to']][par2[i]]]}
      
    }
  } # i (par2)
  
  return(cOut)
  
}

# __________________________________________________________________________----
# PROCESS LSWCD DATA ----
lswcd_q2k <- function(cOut = NULL, dir = NULL, nme = NULL) {
  
  # Calculate TEMP using air temp first - fill in with 2017 data where it exists
  cOut <- airT_corr_q2k(cOut = cOut, dir = dir, nme = nme)
  
  # Reads the LSWCD data and returns the Q2K boundary condition object with
  # DO and temperature boundaries.
  
  # Pass the BC object (cOut) and location of the HSPF data (.RData) files (dir)
  lswc <- read.csv(paste0('C:/siletz_misc/from_tmdl_drive/001_data/wq_data/Mon',
                          'itoring 2017/LSWCD/Lincoln_SWCD_SILETZ RIVER_06292017',
                          '-01052018/siletz_volmon_cont_data.csv'))
  
  lswc$DATE.TIME <- as.POSIXct(lswc$DATE.TIME, '%m/%d/%Y %H:%M',
                               tz = 'America/Los_Angeles')

  # Import the regression parameters, relate all stations to Moonshine station
  regs <- read.csv("C:/siletz_tmdl/01_inputs/02_q2k/summary_NN.csv")
  
  # Re-order based on reach order
  regs <- regs %>% arrange(rch)
  
  # Also need to reimport the hspf flow data for the confluence and trim to dates
  qlc <- readRDS(paste0(dir, '/', nme, '_rchQLC_NOx.RData'));

  qlc <- qlc[[1]][which(qlc[[1]]$Date %in% cOut[[1]]$date), 1 : 3]
  
  # Isolate Moonshine data: forms basis of other T bcs including HW
  # Also isolate TSs in cOut for which moonshine data exist
  cnd1 <- which(cOut[[1]]$date %in% lswc$DATE.TIME[which(lswc$STAID == 37396)])
  
  moon <- lswc[which(lswc$STAID == 37396 & lswc$DATE.TIME %in% cOut[[1]]$date), ]

  # Perform correlations; start with confluence first: mix Nth & Sth Q & T
  temp <- data.frame(date = moon$DATE.TIME,
                     tmpN = regF(m = regs$m[1], b = regs$b[1], x = moon$TEMP_C),
                     tmpS = regF(m = regs$m[2], b = regs$b[2], x = moon$TEMP_C),
                     stringsAsFactors = F)

  temp <- merge(qlc, temp, by.x = 'Date', by.y = 'date', all.x = T, all.y = T)

  # Calculate the mixed T for LSWCD data, don't worry about NAs
  temp$tMix <- (temp$Bas1 * temp$tmpN + temp$Bas2 * temp$tmpS) / (temp$Bas1 + temp$Bas2)

  cOut[['HW']]$tmp_dgC[cnd1] <- temp$tMix[cnd1]
  
  # Populate inflow temp DO bcs; use regressions for Moonshine for consistency
  for (i in 1 : (length(cOut) - 1)) {
    if (i > 1) {
      cOut[[i]]$tmp_dgC[cnd1] <- regF(m = regs$m[i + 1], b = regs$b[i + 1],
                                      x = moon$TEMP_C)
    }
    cOut[[i]]$do_mgL <- do_sat(temp = cOut[[i]]$tmp_dgC, elev = regs$elv[i + 1])
  }

  # Initial conditions; select all data from start date (strD)
  iniC <- lswc[which(lswc$DATE.TIME == cOut[[1]]$date[1]), c(1, 3, 5, 7)]
  
  # Initial conditions; select all data from start date (strD)
  for (i in 1 : nrow(cOut[['Init']])) {
    cOut[['Init']][i, 2] <- cOut[[i + 1]]$tmp_dgC[1]
    cOut[['Init']][i, 5] <- cOut[[i + 1]]$do_mgL[1]
  }
  
  return(cOut)

}

# __________________________________________________________________________----
# USE AIR TEMPERATURE TO CREATE INFLOW T and DO BOUNDARIES ----
airT_corr_q2k <- function(cOut = NULL, dir = NULL, nme = NULL) {
  
  # Reads the stream temperatures from daily min/max regressions. Returns the
  # BC object of DO and stream temp.

  # Read in stream temperature and DO
  T_DO <- readRDS('C:/siletz_tmdl/01_inputs/02_q2k/RData/T_strm_BCs_2004_2017.RData')

  # Reimport the hspf flow data for the confluence and trim to dates
  qlc <- readRDS(paste0(dir, '/', nme, '_rchQLC_NOx.RData'));
  
  qlc <- qlc[[1]][which(qlc[[1]]$Date %in% cOut[[1]]$date), 1 : 3]
  
  # Reduce the dates of the T_DO object to those in the scenario
  temp <- T_DO$temp[which(T_DO$temp$date %in% cOut[['HW']]$date), ]
  
  dsO2 <- T_DO$DO[which(T_DO$DO$date %in% cOut[['HW']]$date), ]

  # Headwaters
  cOut$HW$tmp_dgC = (temp$NthFk * qlc$Bas1 + temp$SthFk * qlc$Bas2) / # Temp
                    (qlc$Bas1 + qlc$Bas2)
  
  cOut$HW$do_mgL  = (dsO2$NthFk * qlc$Bas1 + dsO2$SthFk * qlc$Bas2) / # DO
                    (qlc$Bas1 + qlc$Bas2)

  # Trib inflows
  T_DO$sites$hbas <- paste0('B', ifelse(T_DO$sites$hbas < 10, '0', ''),
                            T_DO$sites$hbas)
  
  T_DO$sites$hbas <- 

  # Rename the CTSI trib stations to the HSPF basin (reach) name (for lookups)    
  for (i in 2 : length(names(temp))) {
    names(temp)[i] <- names(dsO2)[i] <-
      T_DO$sites$hbas[which(T_DO$sites$stn == names(temp)[i])]
  }
  
  # Populate the BC object temperature and DO columns for each reach 
  for (i in 2 : (length(cOut) - 2)) {
    cOut[[i]]$tmp_dgC <- temp[, which(names(temp) == names(cOut)[i])]
    cOut[[i]]$do_mgL  <- dsO2[, which(names(dsO2) == names(cOut)[i])]
  }

  # Initial conditions; select all data from start date (strD)
  for (i in 1 : nrow(cOut[['Init']])) {
    cOut[['Init']][i, 2] <- cOut[[i + 1]]$tmp_dgC[1]
    cOut[['Init']][i, 5] <- cOut[[i + 1]]$do_mgL[1]
  }
  
  return(cOut)
  
}

ctsi_q2k <- function(cOut = NULL, dir = NULL, nme = NULL) {
  
  # Reads the 2004 CTSI stream temperatures & returns BC object of DO and T
  
  # Read in stream temperature and DO
  T_DO <- readRDS('C:/siletz_tmdl/01_inputs/02_q2k/RData/ctsi_T_DO_2004.RData')
  
  # Reimport the hspf flow data for the confluence and trim to dates
  qlc <- readRDS(paste0(dir, '/', nme, '_rchQLC_NOx.RData'));
  
  qlc <- qlc[[1]][which(qlc[[1]]$Date %in% cOut[[1]]$date), 1 : 3]
  
  # Reduce the dates of the T_DO object to those in the scenario
  temp <- T_DO$temp[which(T_DO$temp$date %in% cOut[['HW']]$date), ]
  
  dsO2 <- T_DO$dsO2[which(T_DO$dsO2$date %in% cOut[['HW']]$date), ]
  
  # Headwaters
  cOut$HW$tmp_dgC = (temp$NthFk * qlc$Bas1 + temp$SthFk * qlc$Bas2) / # Temp
                    (qlc$Bas1 + qlc$Bas2)
  
  cOut$HW$do_mgL  = (dsO2$NthFk * qlc$Bas1 + dsO2$SthFk * qlc$Bas2) / # DO
                    (qlc$Bas1 + qlc$Bas2)
  
  # Trib inflows
  T_DO$sites$hbas <- paste0('B', ifelse(T_DO$sites$hbas < 10, '0', ''),
                            T_DO$sites$hbas)
  
  # Rename the CTSI trib stations to the HSPF basin (reach) name (for lookups)    
  for (i in 2 : length(names(temp))) {
    names(temp)[i] <- names(dsO2)[i] <-
      T_DO$sites$hbas[which(T_DO$sites$stn == names(temp)[i])]
  }
  
  # Populate the BC object temperature and DO columns for each reach 
  for (i in 2 : (length(cOut) - 2)) {
    cOut[[i]]$tmp_dgC <- temp[, which(names(temp) == names(cOut)[i])]
    cOut[[i]]$do_mgL  <- dsO2[, which(names(dsO2) == names(cOut)[i])]
  }
  
  # Initial conditions; select all data from start date (strD)
  for (i in 1 : nrow(cOut[['Init']])) {
    cOut[['Init']][i, 2] <- cOut[[i + 1]]$tmp_dgC[1]
    cOut[['Init']][i, 5] <- cOut[[i + 1]]$do_mgL[1]
  }
  
  return(cOut)
  
}

# __________________________________________________________________________----
# PROCESS DEQ CONT DATA ----
deq_cont_q2k <- function(cOut = NULL) {
  
  # This script takes the continuous DEQ data and populates the times series data
  # for headwaters, initial and boundary conditions for conductivity and pH

  tz = 'America/Los_Angeles'
  
  # LOAD DATA
  deqC <- read.csv(paste0('C:/siletz_misc/from_tmdl_drive/001_data/wq_data',
                          '/deq_cont_2017.csv'), stringsAsFactors = F)
  
  # Change the date/time to the year of the scenario
  deqC$datetime <- as.POSIXct(paste0(deqC$DATE, ' ', deqC$TIME1), '%m/%d/%Y %H:%M',
                              tz = tz)
  
  year(deqC$datetime) <- year(cOut[[1]]$date[1])

  # Create a season break date
  strD <- min(cOut[['HW']]$date); endD <- max(cOut[['HW']]$date)
  
  coDt <- as.POSIXct('2017-09-01', '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  year(coDt) <- year(cOut[[1]]$date[1])

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
  if (strD < coDt) {pHdt <- pHdt[-c(1 : 22, 305 : nrow(pHdt)), ]}
  
  t1 <- data.frame(date = seq(strD, endD, 900))

  pHdt <- merge(t1, pHdt, by.x = 'date', by.y = 'datetime', all.x = T)
  
  # Add new columns to contain the sinusoidal regression; rename
  pHdt <- cbind(pHdt, matrix(data = 0, nrow = nrow(pHdt), ncol = 3))  

  names(pHdt)[5 : 7] <- paste0('reg_', names(pHdt)[2 : 4])

  # Add a column for periodicity
  pHdt$per <- (hour(pHdt$date) + minute(pHdt$date) / 60) / 24
  
  # Calculate the sinusoidal coefficients and the lines of best fit
  # PERHAPS NEED TO RETHINK THIS!! GO WITH DAILY MAX/MINS
  c = list()
  
  for (i in 1 : 3) {
    
    c[[i]] <- lm(pHdt[, i + 1] ~ sin(2 * pi * pHdt$per) + cos(2 * pi * pHdt$per) +
                                 sin(4 * pi * pHdt$per) + cos(4 * pi * pHdt$per) +
                                 sin(6 * pi * pHdt$per) + cos(6 * pi * pHdt$per))
    
    # NEED TO MAKE THE TIME SERIES REGULARIZED TO OUTPUT A FULL TIME SERIES
    pHdt[, i + 4] <- c[[i]][[1]][1] + c[[i]][[1]][2] * sin(2 * pi * pHdt$per) +
                                      c[[i]][[1]][3] * cos(2 * pi * pHdt$per) +
                                      c[[i]][[1]][4] * sin(4 * pi * pHdt$per) +
                                      c[[i]][[1]][5] * cos(4 * pi * pHdt$per) +
                                      c[[i]][[1]][6] * sin(6 * pi * pHdt$per) +
                                      c[[i]][[1]][7] * cos(6 * pi * pHdt$per)
    # NEED TO MAKE THE TIME SERIES REGULARIZED TO OUTPUT A FULL TIME SERIES
    
  }
  
  # First reduce to hourly
  ts <- seq(strD, endD, 3600)
  
  head(pHdt)
  
  pHdt <- pHdt[which(pHdt$date %in% ts), c(1, 5, 7, 6)]
  
  # Set columns to populate from pHdt
  #         HW, 03, 06, 07, 08, 11, 12, 13, 14, 15, 16, TW
  pHcl <- c( 3,  3,  3,  2,  2,  2,  2,  2,  1,  1,  1,  1) + 1
  
  for (i in 1 : 12) {
    
    if (i == 1) {pHcol = 19} else if (i == 12) {pHcol = 18} else {pHcol = 20}
    
    cOut[[i]][, pHcol] <- pHdt[, pHcl[[i]]]
    
  }
  
  # _____________________________________________________________________________
  # CREATE CONDUCTIVITY TIME SERIES
  cdDt <- dcast(data = deqC[ , c(13, 3, 5)], formula = datetime ~ SITENAME,
                value.var = 'COND', fun.aggregate = mean)
  
  cdMn <- colMeans(cdDt[, 2 : 4], na.rm = T)
  
  cdMn <- cdMn[c(1, 3, 2)]
  
  # Assign Cond as per the pH
  for (i in 1 : 12) {
    
    if (i == 1) {cdCol = 4} else if (i == 12) {cdCol = 3} else {cdCol = 5}
    
    cOut[[i]][, cdCol] <- cdMn[pHcl[i] - 1]
    
  }
  
  # ASSIGN INITIAL CONDITIONS
  # Assign pH according to the pH column vector, minus HW/TW
  cOut[['Init']]$pH <- unlist(pHdt[1, pHcl[2 : 11]])
  
  # Assign cond according to the pH column vector, minus HW/TW
  cOut[['Init']]$cnd_uSc <- unlist(cdMn[(pHcl[2 : 11] - 1)])
  
  # ASSIGN ALKALINITY (@ 20 mg/L CaCO3)
  # Boundaries
  for (i in 1 : 12) {
    
    if (i == 1) {cdCol = 18} else if (i == 12) {cdCol = 17} else {cdCol = 19}
    
    cOut[[i]][, cdCol] <- 20
    
  }
  
  # Initial conditions
  cOut[['Init']]$alk_mgL <- 20

  return(cOut)
  
}

# __________________________________________________________________________----
# PROCESS DEQ GRAB DATA ----
deq_grab_q2k <- function(cOut = NULL) {
  
  # This script takes the DEQ grab data and populates the times series data
  # for headwaters, initial and boundary conditions for CBOD and phyto (chl a)

  # Load data
  deqP <- read.csv(paste0('C:/siletz_misc/from_tmdl_drive/001_data/wq_data/Mon',
                          'itoring 2017/DEQ/Grab_Samples/chl_a.csv'),
                   stringsAsFactors = F)
  
  deqP$date <- as.POSIXct(deqP$date, '%m/%d/%Y %H:%M', tz = 'America/Los_Angeles')
  
  # Change the year based on the scenario
  year(deqP$date) <- year(cOut[[1]]$date[1])
  
  # Isolate the July data
  coDt <- as.POSIXct('2017-07-16', '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  year(coDt) <- year(cOut[[1]]$date[1])
  
  deqP <- deqP[which(deqP$date >= coDt), ]
  
  # Iterate through headwater/tailwater and inflow BCs
  for (i in 1 : 12) {
    
    # CBOD cOut column; phytoplankton input value
    if (i == 1) {
      cCol <- 8; iVal <- 0.20
    } else if (i == 12) {
      cCol <- 7; iVal <- 0.44
    } else {
      cCol <- 9; iVal <- 0.00
    }
    
    # Phytoplankton cOut column
    pCol <- cCol + 6
    
    # CBOD - Set tribs, HW, and IC = 0.1 mg/L. No better information!!!
    cOut[[i]][, cCol] <- 0.1 * 1.46
    
    # No phytoplankton from the tribs -- set HW to Moonshine (0.20 ug/l) and
    # ICs to closets station
    cOut[[i]][, pCol] <- iVal
    
  }
  
  # SET CBOD & PHYTO INIT Cs
  cOut[['Init']]$cf_mgL <- 0.1 * 1.46; 
  
  cOut[['Init']]$phy_ugL <- c(rep(0.3, 4), rep(0.4, 6))
  
  return(cOut)
  
}

# __________________________________________________________________________----
# WRITE BCs TO CSVs ----
write_bcs_q2k <- function(cOut = NULL, oPth = NULL, name = NULL) {
  
  # Function to output the bc object elements to csv files for input to Q2K
  # arguements include the bc object, output directory, optional name to save
  # the bc object as an RDS (default = NO), and an optional suffix to add
  
  # Add forward slash to file path if not given
  if (substr(oPth, nchar(oPth), nchar(oPth)) != '/') {oPth <- paste0(oPth, '/')}

  # calculate number of days in simulation
  nday <- as.numeric(cOut[["HW"]]$date[length(cOut[["HW"]]$date)] -
                     cOut[["HW"]]$date[1])
  
  # Make Headwater/Tailwater bcs first 
  f = data.frame(x = c(rep(seq(0, 23/24, 1/24), nday), 0))
  
  hwtw <- cbind(cOut[['HW']][2 : length(cOut[['HW']])], f,
                cOut[['TW']][2 : length(cOut[['TW']])])
  
  # Headwater/Tailwater output data.frame 
  hwtw <- t(hwtw)
  
  # Make Inflow Boundary Conditions 
  inBC <- cOut[[1]]; inBC$Reach <- 0; inBC <- inBC[which(1 == 0), ]

  for (i in 2 : (length(cOut) - 2)) {

    # Make reach names (numbers) as leading column of the BC output
    cOut[[i]]$Reach <- i - 1
    
    inBC <- rbind(inBC, cOut[[i]])
    
  }
  
  inBC <- inBC[, c(length(inBC), 1 : (length(inBC) - 1))]

  oFil <- data.frame(pths = c('bc_hdwr', 'bc_infw', 'bc_intC'),
                     fils = c('hdwr',    'infw',    'intC'))

  newD <- paste0(oPth, oFil$pths, '/', name)
  
  # Create new folders for scenario inputs
  for (i in 1 : nrow(oFil)) {if (!file.exists(newD[i])) {dir.create(newD[i])}}

  # WRITE TO CSV 
  oFil <- paste0(newD, '/', oFil$fils, '_YR',
                 addZ(year(cOut[[1]]$date[1]) - 2000), '.csv')

  # Write Headwater/Tailwater
  write.csv(x = hwtw, file = oFil[1])
  
  # Write Inflow BCs
  write.csv(x = inBC, file = oFil[2], row.names = F)
  
  # Write Initial Conditions
  write.csv(cOut[['Init']][2 : length(cOut[['Init']])], file = oFil[3],
            row.names = F)  
  
}

# __________________________________________________________________________----
# EXPAND WR DIVERSIONS FOR Q2K ----
exp_dvs <- function(cOut = NULL, dvs = NULL) {
  
  # This function takes a csv input in hourly diversion data by basin and popul-
  # ates the reach bcs list data

  dvs <- read.csv(dvs, stringsAsFactors = F)
  
  # Add columns for reaches 1 - 3; reorder columns
  dvs$R02 <- dvs$R01 <- 0; dvs <- dvs[, c(1 : 2, 11, 12, 3 : 10)]
  
  # Create the time series with an hour column
  dvsL <- data.frame(date = seq(cOut[[1]]$date[1],
                                cOut[[1]]$date[nrow(cOut[[1]])],
                                3600), stringsAsFactors = F) %>%
          mutate(mnth = month(date), hour = hour(date))

  # Parse by month and merge each month to create a monthly/daily time series
  for (i in 1 : length(unique(dvsL$mnth))) {
    
    tmp1 <- dvsL[which(dvsL$mnth == unique(dvsL$mnth)[i]), ]
    
    tmp2 <- dvs[which(dvs$Month == unique(dvsL$mnth)[i]), ]
    
    tmp3 <- merge(tmp1, tmp2, by.x = 'hour', by.y = 'Hour', all = T)
    
    if (i == 1) {temp <- tmp3} else {temp <- rbind(temp, tmp3)}

  }

  # Reorder on date
  temp <- temp %>% arrange(date)
  
  # Popluate the diversion column of each BC data frame
  for (i in 2 : 11) {cOut[[i]]$abs_cms = temp[, i + 3]}
  
  return(cOut)
  
}

# __________________________________________________________________________----
# STP DISCHARGE ----
stp_bcs <- function(cOut = NULL, stp = NULL, q2kR = NULL) {
  
  # This function daily stp data and expands to an hourly time series and popul-
  # ates the reach bcs list data for the specified Q2K reach; if data don't exist
  # for the modeling time period, then the ts is populated with the mean monthly
  # of all of the data.

  # Read and summarize the data
  stp <- read.csv(stp, stringsAsFactors = F)

  stp$DATE <- as.POSIXct(stp$DATE, '%m/%d/%Y', tz = 'America/Los_Angeles')

  stpM <- summarize_DMR(stp)
  
  # Remove the influent WQ columns from the daily data
  stp <- stp[, -c(2 : 6)]

  # Supplement parameters with no DMR data -- from permit data application and
  # Reid etal 2014 - Field valid of SBR to attain low nutrient levels (for P)
  stpM$do_e  <- 6.3; stp$do_e  <- 6.3         # mg/L
  stpM$nox_e <- 2.7; stp$nox_e <- 2.7         # mg/L
  stp$orn_e  <- stp$NH3 * (2.0 / 1.32 - 1)    # NH3 * (TKN / NH3 - 1) from RPA
  stpM$orn_e <- stpM$nh3_e * (2.0 / 1.32 - 1) # NH3 * (TKN / NH3 - 1) from RPA
  # TP (2.5 mg/L) from DMR & (% PO4 & % org P) from Reid etal 2014
  stpM$orp_e <- 2.50 * 0.45; stp$orp_e <- 2.50 * 0.45 
  stpM$po4_e <- 2.50 * 0.55; stp$po4_e <- 2.50 * 0.55
  stpM$cnd_e <- 157; stp$cnd_e <- 157         # mg/L
  
  # Convert N and P species from mg/L to ug/L
  for (i in c(8, 11 : 14)) {
    stp[, i] <- stp[, i] * 1000; stpM[, i] <- stpM[, i] * 1000
  }
  
  # Convert the flow from MGD to m^3s^-1
  stp$Q_E <- stp$Q_E * 4.3812636389e-2; stpM$q_e <- stpM$q_e * 4.3812636389e-2
  
  # Forward fill the gaps for missing dates
  stp <- stp %>% tidyr::fill(everything(), .direction = 'up')

  # Pull out dataframe for river, STP reach, and mixed WQ
  rivr <- wwtp <- cOut[[q2kR + 1]]

  # Set the WWTP and mixed dfs to zero
  wwtp[, c(2 : (length(wwtp) - 1))] <- 0; mxWQ <- wwtp

  # Match the columns
  mtch <- data.frame(pars = names(wwtp)[3 : length(wwtp)],
                     cOut = 3 : length(wwtp),
                     dmrs = c(4, 2, 15,  6, 10, NA,  5, 12,  8, 11, 13, 14, NA,
                              NA, NA, NA, 9,  3), stringsAsFactors = F)
  
  dtes <- unique(date(wwtp$date))

  for (j in 1 : length(dtes)) { # Iterate through each day

    # Create a temporary df of one row based on the date (preferred) or month
    if (length(which(dtes[j] == stp$DATE)) > 0) {
      
      tmp1 <- stp[which(stp$DATE == dtes[j]), ] # Isolate the data for that day  
      
    } else {
      
      tmp1 <- stpM[which(stpM$mnth == month(dtes[j])), ] # Isolate data for month 
      
    }

    for (i in 1 : nrow(mtch)) { # Iterate through columns
      
      if (is.na(mtch[i, 3])) { # Check id data = NA, and populate with 0 is so
        
        wwtp[which(date(wwtp$date) == dtes[j]), mtch[i, 2]] <- 0
        
      } else {
        
        wwtp[which(date(wwtp$date) == dtes[j]), mtch[i, 2]] <- tmp1[, mtch[i, 3]]
          
      }
    }
  }

  # if (!is.null(csvOut)) {
  # 
  #   if (substr(csvOut, nchar(csvOut), nchar(csvOut)) != '/') {
  #     csvOut <- paste0(csvOut, '/')
  #   }
  #   
  #   write.csv(wwtp, paste0(csvOut, 'stp_raw.csv'), row.names = F)
  #   
  # }
  
  # Mixing of inflows and STP (mass balance - acting as a combined stream)
  # First the flows
  mxWQ$qIn_cms <- wwtp$qIn_cms + rivr$qIn_cms
  
  for (i in 4 : 20) {
    
    mxWQ[, i] <- (wwtp$qIn_cms * wwtp[, i] + rivr$qIn_cms * rivr[, i]) /
                  mxWQ$qIn_cms
    
  }
  
  # Add Q2K reach diversion back in (cause we deleted them up above)
  mxWQ[, 2] <- cOut[[q2kR + 1]][, 2]
  
  cOut[[q2kR + 1]] <- mxWQ

  return(cOut)
  
}

# __________________________________________________________________________----
# Supporting Functions ----
sta2sta_reg <- function(df = NULL, st1 = NULL, st2 = NULL, dir = NULL) {

  # Function to return regression relationships for T and DO between two stations
  # Assumes you're passing a data frame with T and DO data from all of the stations
  # Station 1 (st1) is the indicator variable, and st2 the response. If you pass a
  # file path, the function will save the graphs of the regression # in that directory

  # Import format R2 for displaying R^2 on plots  
  source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_wq_calib_hspf.R')

  names(df) <- c('date', 'seas', 'stid', 'stas', 'temp', 'tdql', 'do_c', 'do_s',
                 'ddql')
  
  df <- df[which(df$stid == st1 | df$stid == st2), c(1, 3, 5, 7)]
  
  pars <- names(df)[3 : 4]
  
  xy <- data.frame(x = c(12.5, 9), y = c(15, 11))
  
  regs <- data.frame(m = c(0, 0), b = c(0, 0), r2 = c(0, 0), pm = c(0, 0),
                     pb = c(0, 0))
  
  row.names(regs) <- c('temp', 'DO')
  
  for (i in 1) { # Just to temperature for the moment} : 2) {
    
    temp <- dcast(data = df, formula = date ~ stid, value.var = pars[i],
                  fun.aggregate = mean)
    
    names(temp) <- c('date', paste0('sta_', names(temp)[2 : 3]))
    
    # Rearrange the columns so that st1 is the second and st2 is the third
    if (names(temp)[2] != paste0('sta_', st1)) {temp <- temp[, c(1, 3, 2)]}
    
    # Logsden is the indicator, confluence the response
    t_reg <- summary(lm(formula = temp[, 3] ~ temp[, 2]))
    
    # t_reg[[4]][[1]] = intercept (p << 0.05), t_reg[[4]][[2]] = slope (p << 0.05)
    regL <- data.frame(mnmx = c(min(temp[, 2], na.rm = T),
                                max(temp[, 2], na.rm = T))) %>%
            mutate(regL = t_reg[[4]][[1]] + t_reg[[4]][[2]] * mnmx)
    
    regs[i, ] <- c(t_reg$coefficients[[2]], t_reg$coefficients[[1]],
                   t_reg$adj.r.squared, t_reg[['coefficients']][2, 4],
                   t_reg[['coefficients']][1, 4])
    
    if (!is.null(dir)) {
      
      lbl <- format_R2(m = unlist(t_reg$coefficients[[2]]),
                       b = unlist(t_reg$coefficients[[1]]),
                       r2 = t_reg$adj.r.squared)
      
      plot <- ggplot(data = temp, aes(x = temp[, 2], y = temp[, 3])) +
              geom_point(color = 'blue', size = 0.6) + theme_bw() +
              geom_line(data = regL, aes(x = mnmx, y = regL), size = 1.6) +
              annotate('text', x = xy[i, 1], y = xy[i, 2], parse = T, label = lbl,
                       hjust = 0, size = 6)
      
      ggsave(filename = paste0('regrss_', pars[i], '_', st2, '_', st1, '.png'),
             plot = plot, path = dir, width = 10, height = 7.5, units = 'in',
             dpi = 300)
      
    }
  }
  
  return(regs)
  
}

regF <- function(m, b, x) {y <- m * x + b; return(y)} # Line of best fit

regL <- function(m, b, x) {y <- 10^b * x^m; return(y)} # Log line of best fit 

init_bcs <- function(strD = NULL, endD = NULL) {
  
  # This function creates/initializes the boundary condition object, which are
  # populated by the HSPF and monitoring data

  # Check date format and coerce if not POSIX
  tz = 'America/Los_Angeles'; dFrm <- '%Y-%m-%d'
  
  if (!is.POSIXct(strD)) {strD <- as.POSIXct(x = strD, dFrm, tz = tz)}
  
  if (!is.POSIXct(endD)) {endD <- as.POSIXct(x = endD, dFrm, tz = tz)}
  
  # Create vector of date/times
  ts <- seq(strD, endD, 3600)
  
  hdrO <- c('abs_cms', 'qIn_cms', 'tmp_dgC', 'cnd_uSc', 'iss_mgL', 'do_mgL',
            'cs_mgL', 'cf_mgL', 'orn_ugL', 'nh3_ugL', 'nox_ugL', 'orp_ugL',
            'po4_ugL', 'phy_ugL', 'oss_mgL', 'bct_cfu', 'gen_na', 'alk_mgL', 'pH')
  
  rchN <- c('1 - Up. Moonshine', '2 - Lw. Moonshine', '3 - Logsden', '4 - Sullivans',
            '5 - USGS', '6 - Toledo Intake', '7 - First Bridge', '8 - Ojalla',
            '9 - Jack Morgan', '10 - Cedar Creek')
  
  # HEADWATER/TAILWATER -- doesn't include abstractions
  hdwr <- data.frame(matrix(data = 0, nrow = length(ts), ncol = length(hdrO),
                            dimnames = list(NULL, c('date', hdrO[-1]))))
  
  # LATERAL INFLOWS -- includes abstractions not input in this script
  latQ <- data.frame(matrix(data = 0, nrow = length(ts), ncol = length(hdrO) + 1,
                            dimnames = list(NULL, c('date', hdrO))))
  
  latQ$date <- hdwr$date <- ts; tlwr <- hdwr[, -2]
  
  # INITIAL CONDITIONS -- no inflows or abstractions
  iniC <- data.frame(matrix(data = 0, nrow = length(rchN), ncol = length(hdrO) - 1,
                            dimnames = list(NULL, c('rch', hdrO[-c(1, 2)]))))
  
  iniC$rch <- rchN
  
  # Create the output object (list of DFs)
  cOut <- list(HW = hdwr, B03 = latQ, B06 = latQ, B07 = latQ, B08 = latQ,
               B11 = latQ, B12 = latQ, B13 = latQ, B14 = latQ, B15 = latQ,
               B16 = latQ, TW = hdwr[, -2], Init = iniC)
  
  return(cOut)

}

summarize_DMR <- function(stp = stp) {

  # Tack on a month column
  stp$mnth <- lubridate::month(stp$DATE)
  
  # Aggregate to mean monthly values; effluent data starts at column 7
  stpM <- stp[, c(7 : length(stp))] %>% group_by(mnth) %>%
          summarise(tmp_e = mean(Temp_E, na.rm = T),
                    pH_e  = mean(pH_E, na.rm = T),
                    q_e   = mean(Q_E, na.rm = T),
                    bod_e = mean(CBOD_E, na.rm = T),
                    # bod_e = max(CBOD_E, na.rm = T),
                    tss_e = mean(TSS_E, na.rm = T),
                    tur_e = mean(Turbidity, na.rm = T),
                    nh3_e = mean(NH3, na.rm = T),
                    alk_e = mean(Alkalinity, na.rm = T))

  # Deal with alk which has several NaNs incl. July set to global mean
  stpM$alk_e <- mean(stp$Alkalinity, na.rm = T)
  
  return(stpM) 
  
}

comp_ts <- function(df = NULL, st1 = NULL, st2 = NULL, dir = NULL) {
  
  # Function plots time series data from twp stations
  
  options(warn = -1)
  
  # Import format R2 for displaying R^2 on plots  
  source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_wq_calib_hspf.R')
  
  names(df) <- c('date', 'seas', 'stid', 'stas', 'temp', 'tdql', 'do_c', 'do_s',
                 'ddql')
  
  df <- df[which(df$stid == st1 | df$stid == st2), c(1, 3, 5, 7)]

  lims <- as.POSIXct(c('2004-06-01', '2004-11-01'), '%Y-%m-%d',
                     tz = 'America/Los_Angeles')
  
  plot <- ggplot(data = df, aes(x = date, y = temp, color = stid)) +
          geom_line() + theme_bw() + theme(axis.text.x = element_text(angle = 90)) +
          scale_x_datetime(limits = lims, breaks = date_breaks("1 weeks"),
                           labels = date_format("%m/%d"))
  
  ggsave(filename = paste0('timesr_temp_', st2, '_', st1, '.png'), plot = plot,
         path = dir, width = 10, height = 7.5, units = 'in', dpi = 300)
    
}

do_sat <- function(temp = NULL, elev = NULL) {
  
  # Function to calculate DO saturation at elevation. From: Benson, B.B., and
  # Daniel Krause, Jr, 1984, The concentration and isotopic fractionation of 
  # oxygen dissolved in freshwater and seawater in equilibrium with the 
  # atmosphere: Limnology and Oceanography, vol. 29, no. 3, p. 620-632.
  # Assumes freshwater ~ salinity = 0

  # Coefficients
  yInt <-   -139.34411
  a    <-   1.575701e5
  b    <-   6.642308e7
  c    <-    1.2438e10
  d    <-  8.621949e11

  temp <- temp + 273.15 # convert to Kelvin
  
  doSat <- exp(yInt + a / temp - b / temp^2 + c / temp^3 - d / temp^4) *
           (1 - 0.02255 * elev * 0.0003048)^5.256
  
  return(doSat)

}



