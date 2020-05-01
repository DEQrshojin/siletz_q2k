rm(list = ls()); cat('\014')

suppressMessages(library(lubridate)); suppressMessages(library(dplyr))
suppressMessages(library(reshape2)); suppressMessages(library(ggplot2))
suppressMessages(library(tidyr)); suppressMessages(library(scales))

source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/bcs_functions_q2k.R')
source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')
source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/q2k_utilities.R')
source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/met_functions_q2k.R')

# Get the model information from both control files
nNme <- read_ctrF_H(); ctrF <- read_ctrF_Q(); tz = 'America/Los_Angeles'

# Create a data frame with all of the iterable components
strD <- as.POSIXct(ctrF$str1, '%Y-%m-%d', tz = tz)
endD <- as.POSIXct(ctrF$end1, '%Y-%m-%d', tz = tz)
wrmU <- as.numeric(ctrF$wrm1)

# INITIALIZE BLANK BC OBJECT 
cOut <- init_bcs(strD = strD, endD = endD)

# HSPF BC INPUTS
if (ctrF$hspf == 'TRUE') { 
  
  if (is.na(ctrF$scen)) { # If most recent HSPF model outputs
    
    cOut <- hspf_q2k(cOut = cOut, dir = ctrF$iDir, nme = nNme$name)
    
  } else {                # If using a previous versions of HSPF outputs
    
    cOut <- hspf_q2k(cOut = cOut, dir = ctrF$iDir, nme = ctrF$scen)
    
  }
  
} else {
  
  cOut <- hspf_q2k(cOut = cOut, dir = ctrF$iDir, nme = ctrF$scen)
  
}

if (ctrF$mntr == 'TRUE') {
  
  nme <- ifelse(is.na(ctrF$scen), nNme$name, ctrF$scen)
  
  if (year(strD) == 2017) { # 2017 DATA & AirT Corr outside of data bounds
    
    cOut <- lswcd_q2k(cOut = cOut, dir = ctrF$iDir, nme = nme) 

  } else if (year(strD) == 2004) { # From 2004 CTSI temperature data

    cOut <- ctsi_q2k(cOut = cOut, dir = ctrF$iDir, nme = nme)

  } else { # Min/Max Air T corr for years other than 2004 & 2017

    cOut <- airT_corr_q2k(cOut = cOut, dir = ctrF$iDir, nme = nme) 
    
  }
  
  cOut <- deq_cont_q2k(cOut = cOut) # DEQ CONT DATA

  cOut <- deq_grab_q2k(cOut = cOut) # DEQ GRAB DATA
  
}

# WR DIVERSIONS
if (ctrF$dvrs == 'TRUE') {cOut <- exp_dvs(cOut = cOut, dvs = ctrF$dvrF)}

# STP DISCHARGE
if (ctrF$stpI == 'TRUE') {cOut <- stp_bcs(cOut = cOut, q2kR = 7, stp = ctrF$stpF)}

if (wrmU != 0 & !is.null(wrmU)) {     # ADD WARM-UP DAYS
  for (j in 1 : (length(cOut) - 1)) { 
    cOut[[j]] <- add_warm_up(df = cOut[[j]], nday = itrs$wrmU[i])
  }
}

for (j in 1 : length(cOut)) { # FILL IN NAs
  cOut[[j]] <- cOut[[j]] %>% tidyr::fill(everything(), .direction = 'up')
}

# WRITE BCs TO CSV
write_bcs_q2k(cOut = cOut, oPth = ctrF$oDir, name = nNme$name) 
