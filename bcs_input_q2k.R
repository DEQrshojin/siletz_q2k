library(lubridate)

source("D:/siletz_q2k/04_scripts/bcs_functions_q2k.R")

strD <- '2017-09-10'; endD <- '2017-09-15'

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
cOut <- lswcd_q2k(cOut = cOut, strD = strD, endD = endD)

# __________________________________________________________________________----
# DEQ CONT DATA ----
cOut <- deq_cont_q2k(cOut = cOut)

# __________________________________________________________________________----
# DEQ GRAB DATA ----
cOut <- deq_grab_q2k(cOut = cOut)

# __________________________________________________________________________----
# WRITE BCs TO CSV ----
cOut <- write_bcs_q2k(cOut = cOut, oPth = oDir, sveRDS = NULL, addSfx = NULL)
