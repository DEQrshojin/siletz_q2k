rm(list = ls())

library(lubridate)

sapply(c("D:/siletz_q2k/04_scripts/bcs_functions_q2k.R",
         "D:/siletz_q2k/04_scripts/bcs_functions_q2k_ss.R"), source)

# Load data?
# cOut <- readRDS("D:/siletz_q2k/02_input/BC_inputs_jul2017_w_dvs.RData")

# Cold-water period
strD <- '2017-07-20'; endD <- '2017-07-21'

# Spawning period
# strD <- '2017-09-10'; endD <- '2017-09-15'

oDir <- 'D:/siletz_q2k/02_input/ss_wSTP_20191126' # Output director for csv files

iDir <- 'D:/siletz/outputs/q2k_noSTP' # Input HSPF data directory

# __________________________________________________________________________----
# INITIALIZE BLANK BC OBJECT ----
cOut <- init_bcs(strD = strD, endD = endD)

sOut <- mod_bcs_ss(cOut) # Modify the bc object for steady state R05 & L06

# __________________________________________________________________________----
# HSPF BC INPUTS ----
cOut <- hspf_q2k(cOut = cOut, strD = strD, endD = endD, dir = iDir)

sOut <- hspf_q2k_ss(sOut = sOut, dir = iDir)

# __________________________________________________________________________----
# LSWCD DATA ----
cOut <- lswcd_q2k(cOut = cOut, dir = iDir)

sOut <- lswcd_q2k_ss(sOut = sOut, dir = iDir)

# __________________________________________________________________________----
# DEQ CONT DATA ----
cOut <- deq_cont_q2k(cOut = cOut)

sOut <- deq_cont_q2k_ss(sOut = sOut)

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

# Create cOut_dummy such that the STP loads get processed and writen to csv, but
# not included in the cOut object. STP will be included as PS, and cOut for B13
# are non-point source
cOut_dummy <- stp_bcs(cOut = cOut, stp = stp, q2kR = 7, csvOut = oDir)

# __________________________________________________________________________----
# WRITE BCs TO CSV ----
addSfx <- 'ss_20170720'; saveRDS <- 'ss_20170720'

cOut <- write_bcs_q2k(cOut = cOut, oPth = oDir, sveRDS = saveRDS,
                      addSfx = addSfx)

sOut <- write_bcs_q2k_ss(sOut = sOut, oPth = oDir, sveRDS = saveRDS,
                         addSfx = addSfx)

rm(list = ls())

dir = 'D:/siletz_q2k/02_input/ss_wSTP_20191126'

sOut <- recombine_ss_bsc(dir = 'D:/siletz_q2k/02_input/ss_wSTP_20191126')

