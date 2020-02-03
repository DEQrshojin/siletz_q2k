library(lubridate)

rm(list = ls()); cat('\014')

source("D:/siletz_q2k/04_scripts/bcs_functions_q2k.R")

# Cold-water period
# strD <- '2017-07-07'; endD <- '2017-08-29'

# Spawning period
strD <- '2017-09-08'; endD <- '2017-10-16'

oDir <- 'D:/siletz_q2k/02_input/wq_sp_valid' # Output director for csv files

iDir <- 'D:/siletz/outputs/q2k_noSTP' # Input HSPF data directory

# __________________________________________________________________________----
# INITIALIZE BLANK BC OBJECT ----
cOut <- init_bcs(strD = strD, endD = endD)

# sOut <- mod_bcs_ss(cOut) # Modify the bc object for steady state R05 & L06

# __________________________________________________________________________----
# HSPF BC INPUTS ----
cOut <- hspf_q2k(cOut = cOut, strD = strD, endD = endD, dir = iDir)

# sOut <- hspf_q2k_ss(sOut = sOut, dir = iDir)

# __________________________________________________________________________----
# LSWCD DATA ----
cOut <- lswcd_q2k(cOut = cOut, dir = iDir)

# sOut <- lswcd_q2k_ss(sOut = sOut, dir = iDir)

# __________________________________________________________________________----
# DEQ CONT DATA ----
cOut <- deq_cont_q2k(cOut = cOut)

# sOut <- deq_cont_q2k_ss(sOut = sOut)

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

# This is for steady state simulations leave in for dynamic (continuous) sims
# Create cOut_dummy such that the STP loads get processed and writen to csv, but
# not included in the cOut object. STP will be included as PS, and cOut for B13
# are non-point source
cOut <- stp_bcs(cOut = cOut, stp = stp, q2kR = 7, csvOut = NULL)

# __________________________________________________________________________----
# ADD WARM-UP DAYS ----
cOut <- add_warm_up(cOut = cOut, nday = 7)

# __________________________________________________________________________----
# FILL IN NAs ----
for (i in 1 : length(cOut)) {cOut[[i]] <- cOut[[i]] %>% fill(everything())}

# __________________________________________________________________________----
# WRITE BCs TO CSV ----
addSfx <- 'sp_valid'; saveRDS <- 'sp_valid'

write_bcs_q2k(cOut = cOut, oPth = oDir, sveRDS = saveRDS, addSfx = addSfx)

# sOut <- write_bcs_q2k_ss(sOut = sOut, oPth = oDir, sveRDS = saveRDS,
#                          addSfx = addSfx)

# rm(list = ls())
# 
# dir = 'D:/siletz_q2k/02_input/ss_wSTP_20191126'
# 
# sOut <- recombine_ss_bsc(dir = 'D:/siletz_q2k/02_input/ss_wSTP_20191126')

