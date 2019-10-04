
rm(list=ls())

source('d:/siletz_q2k/04_scripts/cal_functions_q2k.R')

# COLD WATER ----
strD <- '2017-07-17'; endD <- '2017-07-22'

obs_CW <- obs4PEST(strD, endD)

# CREATE INSTRUCTION FILE
obID <- obs_CW[["obs"]]$obID

iOut <- 'd:/siletz_q2k/08_pest/siletz_CW.ins'

ins4PEST(obID = obID, iOut = iOut)

# CREATE OBSERVATIONS LIST FOR .PST FILE
nas <- obs_CW[["obNA"]]$indx

df <- add_weights(obs_CW[["obs"]])

fOut <- 'd:/siletz_q2k/08_pest/obs_jul2017.txt'

df <- output_obs(df = df, fOut = fOut)

# CREATE MODEL OUTPUT FILE
obID <- obs_CW[["obs"]]$obID

mOut <- 'd:/siletz_q2k/01_models/CW/20190930'

fOut <- 'd:/siletz_q2k/08_pest/siletz_CW.out'

x <- mod4PEST(mOut, obID, strD, fOut)

# SPAWNING ----
strD <- '2017-09-10'; endD <- '2017-09-15'

obs_SP <- obs4PEST(strD, endD)

df <- add_weights(obs_SP[["obs"]])

fOut <- 'd:/siletz_q2k/08_pest/obs_sep2017.txt'

df <- output_obs(df = df, fOut = fOut)