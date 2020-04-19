rm(list = ls()); cat('\014')

library(lubridate)

source("D:/siletz_q2k/04_scripts/bcs_functions_q2k.R")

# Time period
# strD <- '2017-07-07'; endD <- '2017-08-29' # Cold-water period
strD <- '2017-09-08'; endD <- '2017-10-16' # Spawning period
# Output directory
oDir <- 'D:/siletz_q2k/02_input/11_updated_WR' # CW .csv Output directory
# oDir <- 'D:/siletz_q2k/02_input/12_updated_WR' # SP .csv Output directory
# Input HSPF data directory
iDir <- 'D:/siletz/outputs/q2k_noSTP' 

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

# This is for steady state simulations leave in for dynamic (continuous) sims
# Create cOut_dummy such that the STP loads get processed and writen to csv, but
# not included in the cOut object. STP will be included as PS, and cOut for B13
# are non-point source
cOut <- stp_bcs(cOut = cOut, stp = stp, q2kR = 7,
                csvOut = 'D:/siletz_q2k/02_input/11_sp_onlySTP')

# __________________________________________________________________________----
# ADD WARM-UP DAYS ----
cOut <- add_warm_up(cOut = cOut, nday = 7)

# __________________________________________________________________________----
# FILL IN NAs ----
for (i in 1 : length(cOut)) {cOut[[i]] <- cOut[[i]] %>% tidyr::fill(everything())}

# __________________________________________________________________________----
# WRITE BCs TO CSV ----
addSfx <- 'sp_WR_only'; saveRDS <- 'sp_WR_only'

write_bcs_q2k(cOut = cOut, oPth = oDir, sveRDS = saveRDS, addSfx = addSfx)
