source('d:/siletz_q2k/04_scripts/cal_functions_q2k.R')

# COLD WATER ----
strD <- '2017-07-17'; endD <- '2017-07-22'

obs_CW <- obs4PEST(strD, endD)

df <- add_weights(obs_CW[["obs"]])

fOut <- 'd:/siletz_q2k/08_pest/obs_jul2017.txt'

df <- output_obs(df = df, fOut = fOut)

# SPAWNING ----
strD <- '2017-09-10'; endD <- '2017-09-15'

obs_SP <- obs4PEST(strD, endD)

df <- add_weights(obs_SP[["obs"]])

fOut <- 'd:/siletz_q2k/08_pest/obs_sep2017.txt'

df <- output_obs(df = df, fOut = fOut)






