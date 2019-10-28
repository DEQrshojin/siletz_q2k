library(lubridate)

source("D:/siletz_q2k/04_scripts/bcs_functions_q2k.R")

# Load data?
# cOut <- readRDS("D:/siletz_q2k/02_input/BC_inputs_jul2017_w_dvs.RData")

# Cold-water period
strD <- '2017-07-17'; endD <- '2017-07-22'

# Spawning period
# strD <- '2017-09-10'; endD <- '2017-09-15'

oDir <- 'D:/siletz_q2k/02_input/' # Output director for csv files

iDir <- 'D:/siletz/outputs/calib_20190611' # Input HSPF data directory

# __________________________________________________________________________----
# INITIALIZE BLANK BC OBJECT ----
cOut <- init_bcs(strD = strD, endD = endD)

# __________________________________________________________________________----
# HSPF BC INPUTS ----
cOut <- hspf_q2k(cOut = cOut, strD = strD, endD = endD, dir = iDir)

# __________________________________________________________________________----
# LSWCD DATA ----
cOut <- lswcd_q2k(cOut = cOut, dir = iDir)

# __________________________________________________________________________----
# DEQ CONT DATA ----
cOut <- deq_cont_q2k(cOut = cOut)

# __________________________________________________________________________----
# DEQ GRAB DATA ----
cOut <- deq_grab_q2k(cOut = cOut)

# __________________________________________________________________________----
# WR DIVERSIONS ----
dvs <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle_S',
              'iletz_River_1710020405/001_data/water_rights/all_diversions_4R.csv')

cOut <- exp_dvs(cOut = cOut, dvs = dvs)

# __________________________________________________________________________----
# STP DISCHARGE ----
stp <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle_S',
              'iletz_River_1710020405/001_data/npdes/slz_dmr.csv')

cOut <- stp_bcs(cOut = cOut, stp = stp, q2kR = 7)

# __________________________________________________________________________----
# WRITE BCs TO CSV ----
addSfx <- 'jul2017'; saveRDS <- 'BC_inputs_jul2017'

cOut <- write_bcs_q2k(cOut = cOut, oPth = oDir, sveRDS = saveRDS,
                      addSfx = addSfx)




