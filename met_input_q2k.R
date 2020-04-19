rm(list = ls()); cat('\014')

suppressMessages(library(raster)); suppressMessages(library(dplyr))
suppressMessages(library(lubridate))

# LOAD FUNCTIONS, VARS & DATA ----
source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/met_functions_q2k.R')
source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')
source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/q2k_utilities.R')
source('C:/siletz_tmdl/04_scripts/03_hs/02_R/fnct_eff_shd.R')

# Get the model information from both control files
nNme <- read_ctrF_H(); ctrF <- read_ctrF_Q(); a <- ctrF$oDir

if (substr(a, nchar(a), nchar(a)) != '/') {ctrF$oDir <- paste0(ctrF$oDir, '/')}

tz = 'America/Los_Angeles'

# Create a data frame with all of the iterable components
itrs <- data.frame(strD = as.POSIXct(c(ctrF$str1, ctrF$str2), '%Y-%m-%d', tz = tz),
                   endD = as.POSIXct(c(ctrF$end1, ctrF$end2), '%Y-%m-%d', tz = tz),
                   wrmU = as.numeric(c(ctrF$wrm1, ctrF$wrm2)), seas = c('cw', 'sp'))

x <- readRDS(paste0(ctrF$mDir, 'met_data_4_Q2Kw.RData'))

dirV <- c('mt_Ta', 'mt_Td', 'mt_CC', 'mt_wnd', 'mt_eShd')

for (i in 1 : 2) {

  if (itrs$wrmU[i] == 0) {nday = NULL} else {nday = itrs$wrmU[i]}
  
  # AIR TEMPERATURE ____________________________________________________________
  if (ctrF$airT == 'TRUE') {

    airT <- t_air_q2k(strD = itrs$strD[i], endD = itrs$endD[i],
                      hBsn = c(3, 6, 7, 8, 11, 12, 13, 14, 15, 16), q2k = T,
                      nday = nday)
    
    if (ctrF$mdTa == 'TRUE') {
      airT <- modify_met_df(df = airT, strD = itrs$strD[i], endD = itrs$endD[i],
                            mdfy = ctrF$mdTa)
    }
    
    write.csv(x = airT, row.names = F,
              file = paste0(ctrF$oDir, dirV[1], '/', nNme$name, '_airT_',
                            itrs$seas[i],'.csv'))
    
  }

  # DEW POINT TEMPERATURE ______________________________________________________
  if (ctrF$dwpT == 'TRUE') {
    
    dwpT <- t_dwp_q2k(strD = itrs$strD[i], endD = itrs$endD[i],
                      hBsn = c(3, 6, 7, 8, 11, 12, 13, 14, 15, 16), q2k = T,
                      nday = nday)
    
    if (ctrF$mdTd == 'TRUE') {
      dwpT <- modify_met_df(df = dwpT, strD = itrs$strD[i], endD = itrs$endD[i],
                            mdfy = ctrF$mdTd)
    }
    
    write.csv(x = dwpT, row.names = F,
              file = paste0(ctrF$oDir, dirV[2], '/', nNme$name, '_dwpT_',
                            itrs$seas[i],'.csv'))
    
  }

  # CLOUD COVER ________________________________________________________________
  if (ctrF$cCov == 'TRUE') {
    
    cCov <- cloud_q2k(x = x, strD = itrs$strD[i], endD = itrs$endD[i],
                      nday = nday)
    
    # Hard code this modification in 
    d <- as.POSIXct(c('2017-07-07', '2017-08-29'), '%Y-%m-%d',
                    tz = 'America/Los_Angeles')
    
    if (length(ctrF$mdCc) != 0 & length(ctrF$mdCc) != 1) {
      cCov <- modify_met_df(df = cCov, strD = d[1], endD = d[2], mdfy = ctrF$mdCc)
    }
    
    write.csv(x = cCov, row.names = F,
              file = paste0(ctrF$oDir, dirV[3], '/', nNme$name, '_cCov_',
                            itrs$seas[i],'.csv'))
    
  }

  # WIND SPEED _________________________________________________________________
  if (ctrF$wndU == 'TRUE') {

    wndU <- wind_q2k(x = x, strD = itrs$strD[i], endD = itrs$endD[i], 
                     nday = nday)
    
    if (length(ctrF$mdWs) != 0 & length(ctrF$mdWs) != 1) {
      wndU <- modify_met_df(df = wndU, strD = itrs$strD[i], endD = itrs$endD[i],
                            mdfy = ctrF$mdWs)
    }
    
    write.csv(x = wndU, row.names = F,
              file = paste0(ctrF$oDir, dirV[4], '/', nNme$name, '_wndS_',
                            itrs$seas[i],'.csv'))

  }
  
  # EFFECTIVE SHADE ____________________________________________________________
  if (ctrF$eShd == 'TRUE') {
    
    # Load the shade data
    allD <- readRDS(ctrF$sDir)
    
    eShd <- effshd_4_q2k(ES = allD, strD = itrs$strD[i], endD = itrs$endD[i],
                         nday = nday)
    
    write.csv(x = eShd, row.names = F,
              file = paste0(ctrF$oDir, dirV[5], '/', nNme$name, '_eShd_',
                            itrs$seas[i],'.csv'))

  }
}  
