rm(list=ls())

source('d:/siletz_q2k/04_scripts/cal_functions_q2k.R')

# ________________________________________________________________________----
# PROCESS OBSERVATIONS FOR PEST ----
strD <- '2017-07-17'; endD <- '2017-07-22'

obs_CW <- obs4PEST(strD, endD)

# ________________________________________________________________________----
# REMOVE OBSERVATIONS ----
dtes <- c('2017-07-20 15:00', '2017-07-20 16:00', '2017-07-20 17:00')

obs_CW <- remove_obs(dtes = dtes, parm = 'phX', q2kR = '05', obID = obs_CW)

dtes <- c('2017-07-20 17:00', '2017-07-20 18:00')

obs_CW <- remove_obs(dtes = dtes, parm = 'phX', q2kR = '09', obID = obs_CW)

# ________________________________________________________________________----
# OUTPUT OBSERVATIONS LIST FOR CONTROL (.PST) FILE ----
df <- add_weights(obs_CW[["obs"]])

fOut <- 'd:/siletz_q2k/08_pest/supp/obs_jul2017.txt'

df <- output_obs(df = df, fOut = fOut)

# ________________________________________________________________________----
# OUTPUT INSTRUCTION (.INS) FILE ----
obID <- obs_CW[["obs"]]$obID

iOut <- 'd:/siletz_q2k/08_pest/siletz_CW.ins'

ins4PEST(obID = obID, iOut = iOut)

# ________________________________________________________________________----
# OUTPUT MODEL OUTPUT (.OUT) FILE ----
obID <- obs_CW[["obs"]]$obID

mOut <- 'd:/siletz_q2k/01_models/CW/20190930'

fOut <- 'd:/siletz_q2k/08_pest/siletz_CW.out'

x <- mod4PEST(mOut, obID, strD, fOut)

# ________________________________________________________________________----
# RUN SUPPLEMENTAL CALIBRATION SCRIPTS ----
strD <- '2017-07-17'; endD <- '2017-07-22'

# mOut <- '//deqhq1/rshojin/siletz_q2k/03_output'
mOut <- 'D:/siletz_q2k/01_models/CW/20190930'

oOut <- obs_CW[['obs']][, c(2, 3, 6, 5)]

x <- cal_supp(strD, mOut, oOut, nDir)




