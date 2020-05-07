suppressMessages(library(dplyr)); suppressMessages(library(lubridate))

# LOAD FUNCTIONS, VARS & DATA ----
source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/met_functions_q2k.R')
source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')
source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/q2k_utilities.R')
source('C:/siletz_tmdl/04_scripts/03_hs/02_R/fnct_eff_shd.R')

# Get the model information from both control files
nNme <- read_ctrF_H(); ctrF <- read_ctrF_Q(); a <- ctrF$oDir

if (substr(a, nchar(a), nchar(a)) != '/') {ctrF$oDir <- paste0(ctrF$oDir, '/')}

tz = 'America/Los_Angeles'

strD <- as.POSIXct(ctrF$str1, '%Y-%m-%d', tz = tz)

endD <- as.POSIXct(ctrF$end1, '%Y-%m-%d', tz = tz)

year <- paste0('YR', addZ(year(strD) - 2000))

wrmU <- as.numeric(ctrF$wrm1)

x <- readRDS(paste0(ctrF$mDir, 'met_data_4_Q2Kw.RData'))

dirV <- c('mt_Ta', 'mt_Td', 'mt_CC', 'mt_wnd', 'mt_eShd')

newD <- paste0(ctrF$oDir, dirV, '/', nNme$name)

# Create new folders for scenario inputs
for (i in 1 : length(dirV)) {if (!file.exists(newD[i])) {dir.create(newD[i])}}

if (wrmU == 0) {nday = NULL} else {nday = wrmU}

# AIR TEMPERATURE ____________________________________________________________
if (ctrF$airT == 'TRUE') {

  airT <- t_air_q2k(strD = strD, endD = endD, q2k = T, nday = nday,
                    hBsn = c(3, 6, 7, 8, 11, 12, 13, 14, 15, 16))
  
  if (ctrF$mdTa == 'TRUE') {
    airT <- modify_met_df(df = airT, strD = strD, endD = endD,
                          mdfy = ctrF$mdTa)
  }
  
  write.csv(airT, paste0(newD[1], '/airT_', year, '.csv'), row.names = F)

}

# DEW POINT TEMPERATURE ______________________________________________________
if (ctrF$dwpT == 'TRUE') {

  dwpT <- t_dwp_q2k(strD = strD, endD = endD, q2k = T, nday = nday,
                    hBsn = c(3, 6, 7, 8, 11, 12, 13, 14, 15, 16))
  
  if (ctrF$mdTd == 'TRUE') {
    dwpT <- modify_met_df(df = dwpT, strD = strD, endD = endD,
                          mdfy = ctrF$mdTd)
  }
  
  write.csv(dwpT, paste0(newD[2], '/dwpT_', year, '.csv'), row.names = F)

}

# CLOUD COVER ________________________________________________________________
if (ctrF$cCov == 'TRUE') {
  
  cCov <- cloud_q2k(x = x, strD = strD, endD = endD, nday = nday)

  if (length(ctrF$mdCc) != 0 & length(ctrF$mdCc) != 1) {
    cCov <- modify_met_df(df = cCov, strD = strD, endD = endD, mdfy = ctrF$mdCc)
  }
  
  write.csv(cCov, paste0(newD[3], '/cCov_', year, '.csv'), row.names = F)

}

# WIND SPEED _________________________________________________________________
if (ctrF$wndU == 'TRUE') {

  wndU <- wind_q2k(x = x, strD = strD, endD = endD, nday = nday)
  
  if (length(ctrF$mdWs) != 0 & length(ctrF$mdWs) != 1) {
    wndU <- modify_met_df(df = wndU, strD = strD, endD = endD, mdfy = ctrF$mdWs)
  }
  
  write.csv(wndU, paste0(newD[4], '/wndS_', year, '.csv'), row.names = F)

}

# EFFECTIVE SHADE ____________________________________________________________
if (ctrF$eShd == 'TRUE') {

  # Load the shade data
  allD <- readRDS(ctrF$sDir)
  
  eShd <- effshd_4_q2k(ES = allD, strD = strD, endD = endD, nday = nday)
  
  write.csv(x = eShd, paste0(newD[5], '/eShd_', year, '.csv'), row.names = F)

} else {
  
  copy_inputs(inpt = 'ES', scnN = nNme$name, scnO = ctrF$sOld)
  
}
