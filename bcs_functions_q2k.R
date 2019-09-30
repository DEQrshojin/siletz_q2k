# Suite of functions to create boundary condition input dfs for Qual2Kw
# Pulls from several sets of disparate data

# __________________________________________________________________________----
# HSPF BC INPUTS ----
hspf_q2k <- function(cOut = NULL, strD = NULL, endD = NULL, dir = NULL) {
  
  # Synopsis
  # Converts HSPF outputs to Qual2Kw inputs. Inputs include:
  # cOut = bc object (list) used to create bcs (by init_bcs()) for qual2kw
  # strD = start date of input data
  # endD = end date of input data (ends on hour 0 of the specified day)
  # dir = directory where the HSPF data are

  # Libraries, functions and options ___________________________________________
  library(lubridate)
  
  source('d:/siletz/scripts/R/utilities.R')

  chrDts <- c(strD, endD) # Save as character dates for warning log
  
  # Coerce dates to POSIXct if not already _____________________________________
  if (!is.POSIXct(strD)) {strD <- as.POSIXct(strD, '%Y-%m-%d',
                                             tz = 'America/Los_Angeles')}
  
  if (!is.POSIXct(endD)) {endD <- as.POSIXct(endD, '%Y-%m-%d', 
                                             tz = 'America/Los_Angeles')}

  # Create/initialize the boundary condition input object if not passed
  if(is.null(cOut)) {cOut <- init_bcs(strD = strD, endD = endD)}

  # Set the parameter list for HSPF outputs ____________________________________
  par2 = c('Q', 'TKN', 'NH3', 'NOx', 'TP', 'PO4', 'OrC')

  # Create a vector of nutrient destination for initial conditions
  iCol <- list(from = c(B03 = 4, B06 = 7, B07 = 8, B08 = 9, B11 = 12,
                        B12 = 13, B13 = 14, B14 = 15, B15 = 16, B16 = 17),
               to = c(TKN = 8, NH3 = 9, NOx = 10, TP = 11, PO4 = 12, OrC = 14))
  
  # Create a list of the basins from which to pull either lateral or reach data
  cBas <- list(HW = list(L = c(1, 2), R = NULL), # Lat from B1 & B2
               B03 = list(L = 3, R = NULL),      # Lat from B3
               B06 = list(L = 6, R = 5),         # Lat from B6 and Rch from B5 
               B07 = list(L = 7, R = NULL),      # Lat from B7
               B08 = list(L = 8, R = 10),        # Lat from B8 and Rch from B10
               B11 = list(L = 11, R = NULL),     # Lat from B11
               B12 = list(L = 12, R = NULL),     # Lat from B12
               B13 = list(L = 13, R = NULL),     # Lat from B13
               B14 = list(L = 14, R = NULL),     # Lat from B14
               B15 = list(L = 15, R = NULL),     # Lat from B15
               B16 = list(L = 16, R = NULL),     # Lat from B16
               TW = list(L = NULL, R = 16))      # Rch from B16
  
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
    qlc <- list(lat = readRDS(paste0(dir, '/', 'latQLC_', var, '.RData')),
                rch = readRDS(paste0(dir, '/', 'rchQLC_', var, '.RData')))
    
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
lswcd_q2k <- function(cOut = NULL) {
  
  lswc <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxyge',
                          'n/Middle_Siletz_River_1710020405/001_data/wq_data/Mon',
                          'itoring 2017/LSWCD/Lincoln_SWCD_SILETZ RIVER_06292017',
                          '-01052018/siletz_volmon_cont_data.csv'))
  
  lswc$DATE.TIME <- as.POSIXct(lswc$DATE.TIME, '%m/%d/%Y %H:%M',
                               tz = 'America/Los_Angeles')

  # PROCESS HEADWATER CONDITIONS
  # Moonshine regressions -- Use this for before 7/17/2017 and for Spawning
  rMoo <- sta2sta_reg(df = lswc, st1 = 37396, st2 = 38912)
  
  # Logsden regressions -- Use this for after 7/17/2017
  rLog <- sta2sta_reg(df = lswc, st1 = 11246, st2 = 38912)

  # Isolate the data for filling:
  data = lswc[lswc$STAID %in% c(11246, 37396, 38912), c(1, 3, 5, 7)]

  pars <- names(data)[3 : 4]
  
  coDt <- as.POSIXct('2017-07-17 14:00', '%Y-%m-%d %H:%M', tz = 'America/Los_Angeles')
  
  hwTDO <- list()
  
  for (i in 1 : 2) {
    
    temp <- dcast(data = data, formula = DATE.TIME ~ STAID, value.var = pars[i],
                  fun.aggregate = mean)
    
    names(temp) <- c('date', paste0(pars[i], '_', names(temp)[2 : 4]))
    
    # Fill in the Confluence NAs from Moonshine
    cnd1 <- which(temp$date < coDt); cnd2 <- which(temp$date >= coDt)
    
    temp[cnd1, 4] <- ifelse(is.na(temp[cnd1, 4]),
                            regF(rMoo[i, 1], rMoo[i, 2], temp[cnd1, 3]),
                            temp[cnd1, 4])
    
    # Fill in the Confluence NAs from Logsden
    temp[cnd2, 4] <- ifelse(is.na(temp[cnd2, 4]),
                            regF(rLog[i, 1], rLog[i, 2], temp[cnd2, 2]),
                            temp[cnd2, 4])
    
    hwTDO[[i]] <- temp
    
  }
  
  # Populate headwater and boundary conditions with above TS; headwaters first!
  # The Headwater conditions will also be the inflow conditions for basins 3 & 6
  for (i in 1 : 3) {
    
    cOut[[i]]$tmp_dgC <- hwTDO[[1]][which(hwTDO[[1]]$date %in% cOut[[i]]$date), 4]
    
    cOut[[i]]$do_mgL <- hwTDO[[2]][which(hwTDO[[2]]$date %in% cOut[[i]]$date), 4]
    
  }
  
  # PROCESS ROCK CREEK CONDITIONS 
  # Combine the Rock Creek data renumber station to 99999
  lswc$STAID <- ifelse((lswc$STAID == 38929 | lswc$STAID == 38930), 99999,
                       lswc$STAID)
  
  # Use USGS gage data for indicator variable for Rock Creek
  rUSG <- sta2sta_reg(df = lswc, st1 = 38918, st2 = 99999)

  # Isolate the data for filling:
  data = lswc[lswc$STAID %in% c(38918, 99999), c(1, 3, 5, 7)]

  hwTDO <- list()
  
  for (i in 1 : 2) {
    
    temp <- dcast(data = data, formula = DATE.TIME ~ STAID, value.var = pars[i],
                  fun.aggregate = mean)
    
    names(temp) <- c('date', paste0(pars[i], '_', names(temp)[2 : 3]))
    
    temp[, 3] <- ifelse(is.na(temp[, 3]),
                        regF(rMoo[i, 1], rMoo[i, 2], temp[, 2]),
                        temp[, 3])
    
    hwTDO[[i]] <- temp
    
  }
  
  for (i in 4 : 12) {
    
    cOut[[i]]$tmp_dgC <- hwTDO[[1]][which(hwTDO[[1]]$date %in% cOut[[i]]$date), 3]
    
    cOut[[i]]$do_mgL <- hwTDO[[2]][which(hwTDO[[2]]$date %in% cOut[[i]]$date), 3]
    
  }
  
  # Initial conditions
  # Select all data from start date (strD)
  iniC <- lswc[which(lswc$DATE.TIME == strD), c(1, 3, 5, 7)]
  
  names(iniC) <- c('date', 'sta', 'temp', 'do')
  
  # Assign based on location and period of record
  if (month(strD) < 8) {
    
    # Assignments: 1. 38944 - 15 & 16; 2. 38300 - 14; 3. 10391 - 12 & 13;
    #              4. 38918 - 7, 8, 11; 5. 37396 - 3 & 6
    assn <- c(5, 5, 4, 4, 4, 3, 3, 2, 1, 1)

  } else {
    
    # Assignments: 1. 36367 - 15 & 16; 2. 38300 - 14; 3. 37848 - 12 & 13
    #              4. 38918 - 11; 5. 38928 - 8; 6. 11246 - 7; 7. 37396 - 3 & 6
    assn <- c(7, 7, 6, 5, 4, 3, 3, 2, 1, 1)
    
  }
  
  cOut[[13]]$tmp_dgC <- iniC[assn, 3]; cOut[[13]]$do_mgL <- iniC[assn, 4]

  return(cOut)

}

# __________________________________________________________________________----
# PROCESS DEQ CONT DATA ----
deq_cont_q2k <- function(cOut = NULL, seas = 'cw') {
  
  # This script takes the continuous DEQ data and populates the times series data
  # for headwaters, initial and boundary conditions for conductivity and pH
  # This script takes the continuous LSWCD data and populates the times series 
  # data for H/W, initial and boundary conditions for temp and DO. Inputs:
  # 2) cOut       - boundary condition object
  # 3) seas       -'CW' for cold-water or 'SP' for spawning

  library(reshape2); library(lubridate); library(ggplot2)
  
  tz = 'America/Los_Angeles'
  
  # LOAD DATA
  deqC <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxy',
                          'gen/Middle_Siletz_River_1710020405/001_data/wq_data',
                          '/deq_cont_2017.csv'), stringsAsFactors = F)
  
  deqC$datetime <- as.POSIXct(paste0(deqC$DATE, ' ', deqC$TIME1), '%m/%d/%Y %H:%M',
                              tz = tz)

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
  
  # PLOT TO CHECK
  # x <- melt(data = pHdt, id.vars = 'date', value.name = 'pH',
  #           variable.name = 'st')  
  # 
  # windows(12, 12)
  # 
  # pl <- ggplot(data = x[which(x$st != 'per'), ],
  #              aes(x = date, y = pH, color = st)) + geom_point(); pl

  # Assign pH by basin (geographical proxy): ODEQ-36367 (JMP) - B14-B16, TW; 
  # ODEQ-38918 (USGS) - B07, B08, B11-B13; ODEQ-37396 (MOON) - B03, B06
  # First reduce to hourly
  ts <- seq(strD,endD, 3600)
  
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
  library(reshape2); library(lubridate); library(ggplot2)
  
  # Load data
  deqP <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxyge',
                          'n/Middle_Siletz_River_1710020405/001_data/wq_data/Mon',
                          'itoring 2017/DEQ/Grab_Samples/chl_a.csv'),
                   stringsAsFactors = F)
  
  deqP$date <- as.POSIXct(deqP$date, '%m/%d/%Y %H:%M', tz = 'America/Los_Angeles')
  
  # Isolate the July data
  coDt <- as.POSIXct('2017-07-16', '%Y-%m-%d', tz = 'America/Los_Angeles')
  
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
  
  cOut[['Init']]$phy_ugL2 <- c(rep(0.3, 4), rep(0.4, 6))
  
  return(cOut)
  
}

# __________________________________________________________________________----
# WRITE BCs TO CSVs ----
write_bcs_q2k <- function(cOut = NULL, oPth = NULL, sveRDS = NULL,
                          addSfx = NULL) {
  
  # Function to output the bc object elements to csv files for input to Q2K
  # arguements include the bc object, output directory, optional name to save
  # the bc object as an RDS (default = NO), and an optional suffix to add
  
  # Add forward slash to file path if not given
  if (substr(oPth, nchar(oPth), nchar(oPth)) != '/') {oPth <- paste0(oPth, '/')}
  
  # calculate nday
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
    
    cOut[[i]]$Reach <- as.numeric(gsub('B', '', names(cOut)[i]))
    
    inBC <- rbind(inBC, cOut[[i]])
    
  }
  
  inBC <- inBC[, c(length(inBC), 1 : (length(inBC) - 1))]

  oFil <- c('headwtr_bc', 'inflows_bc', 'init_conds')

  # WRITE TO CSV 
  if (is.null(addSfx)) {
    oFil <- paste0(oPth, oFil, '.csv')  
  } else {
    oFil <- paste0(oPth, oFil, '_', addSfx, '.csv')  
  }

  # Write Headwater/Tailwater
  write.csv(x = hwtw, file = oFil[1])
  
  # Write Inflow BCs
  write.csv(x = inBC, file = oFil[2], row.names = F)
  
  # Write Initial Conditions
  write.csv(cOut[['Init']][2 : length(cOut[['Init']])],
              file = oFil[3], row.names = F)  
  
  if (!is.null(sveRDS)) {
    saveRDS(object = cOut, file = paste0(oPth, sveRDS, '.RData'))
  }
}

# __________________________________________________________________________----
# Supporting Functions ----
sta2sta_reg <- function(df = NULL, st1 = NULL, st2 = NULL, dir = NULL) {

  # Function to return regression relationships for T and DO between two stations
  # Assumes you're passing a data frame with T and DO data from all of the stations
  # Station 1 (st1) is the indicator variable, and st2 the response. If you pass a
  # file path, the function will save the graphs of the regression # in that directory

  library(ggplot2); library(reshape2); library(dplyr)

  # Import format R2 for displaying R^2 on plots  
  source('D:/siletz/scripts/R/water_quality_calib.R')
  
  df <- df[which(df$STAID == st1 | df$STAID == st2), c(1, 3, 5, 7)]
  
  pars <- names(df)[3 : 4]
  
  xy <- data.frame(x = c(12.5, 9), y = c(15, 11))
  
  regs <- data.frame(m = c(0, 0), b = c(0, 0), r2 = c(0, 0))
  
  row.names(regs) <- c('temp', 'DO')
  
  for (i in 1 : 2) {
    
    temp <- dcast(data = df, formula = DATE.TIME ~ STAID, value.var = pars[i],
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
                   t_reg$adj.r.squared)
    
    if (!is.null(dir)) {
      
      lbl <- format_R2(m = unlist(t_reg$coefficients[[2]]),
                       b = unlist(t_reg$coefficients[[1]]),
                       r2 = t_reg$adj.r.squared)
      
      plot <- ggplot(data = temp, aes(x = temp[, 2], y = temp[, 3])) +
              geom_point(color = 'blue', size = 0.6) + theme_bw() +
              geom_line(data = regL, aes(x = mnmx, y = regL), size = 1.6) +
              annotate('text', x = xy[i, 1], y = xy[i, 2], parse = T, label = lbl,
                       hjust = 0, size = 6); plot
      
      ggsave(filename = paste0('stas', st1, '_', st2, '_', pars[i], '_regrss.png'),
             plot = plot, path = pth2, width = 10, height = 7.5, units = 'in', dpi = 300)
      
    }
  }
  
  return(regs)
  
}

regF <- function(m, b, x) {y <- m * x + b; return(y)} # Line of best fit

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
