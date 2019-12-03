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

fOut <- 'd:/siletz_q2k/08_pest/05_supp/obs_jul2017.txt'

df <- output_obs(df = df, fOut = fOut)

# ________________________________________________________________________----
# OUTPUT INSTRUCTION (.INS) FILE ----
obID <- obs_CW[["obs"]]$obID

iOut <- 'd:/siletz_q2k/08_pest/siletz_CW.ins'

ins4PEST(obID = obID, iOut = iOut)

# ________________________________________________________________________----
# OUTPUT MODEL OUTPUT (.OUT) FILE ----
obID <- obs_CW[["obs"]]$obID

mOut <- 'd:/siletz_q2k/08_pest/01_hydr'

fOut <- 'd:/siletz_q2k/08_pest/01_hydr/slz_q2k_H.out'

x <- mod4PEST(mOut, obID, strD, fOut)

# ________________________________________________________________________----
# RUN SUPPLEMENTAL CALIBRATION SCRIPTS ----
strD <- '2017-07-17'; endD <- '2017-07-22'

# mOut <- '//deqhq1/rshojin/siletz_q2k/03_output'
mOut <- 'D:/siletz_q2k/01_models/CW/20190930'

oOut <- obs_CW[['obs']][, c(2, 3, 6, 5)]

x <- cal_supp(strD, mOut, oOut, nDir)

# ________________________________________________________________________----
# CALCULATE RATING CURVES ----

# The Heat Source hydraulic data were extracted from the Heat Source 2004
# Current Conditions model, using the 'get_stream_width.R' script in the C drive.
# See that script for Heat Source prediction location
rm(list=ls())

source('d:/siletz_q2k/04_scripts/cal_functions_q2k.R')

hyHS <- readRDS("D:/siletz_q2k/02_input/HS_hydraulics_2004.RData")

path <- 'D:/siletz_q2k/06_figures/h_cal/'

vlrc <- list(); wdrc <- list()

trns <- 'LL'

for (i in 1 : length(hyHS)) {

  tmp1 <- hyHS[[i]][, c(3, 4)] # depth = 2, flow = 3, veloc = 4, width = 5

  vlrc[[i]] <- rating_curves(df = tmp1,
                             par = 'Velocity',
                             trns = trns)
                             # file = paste0(path, 'vel_rc_', names(hyHS)[i], '.png'))

  names(vlrc)[i] <- names(hyHS)[i]

  tmp2 <- hyHS[[i]][, c(3, 5)] # depth = 2, flow = 3, veloc = 4, width = 5

  wdrc[[i]] <- rating_curves(df = tmp2, par = 'Width', trns = trns)
                             # file = paste0(path, 'wdt_rc_', trns, '_',
                             #               names(hyHS)[i], '.png'))

  names(wdrc)[i] <- names(hyHS)[i]
  
}

# Even though the sqrt-sqrt transformation is a better fit, Qual2Kw requires a
# rating curve that has a log-log transformed relationship.
rc <- list(velocity = vlrc, width = wdrc)

# rc <- vlrc

saveRDS(object = rc, file = 'D:/siletz_q2k/02_input/HS_rating_curves.RData')

# Output to a csv to import into Qual2Kw
df <- data.frame(rch = 0, b_v = 0, a_v = 0, b_w = 0, a_w = 0, stringsAsFactors = F)

for (i in 1 : 10) {

  df <- rbind(df,
              c(rch = names(rc[[1]])[i],
                b_v = 10^rc[["velocity"]][[i]][["coef"]][2],
                a_v = rc[["velocity"]][[i]][["coef"]][1],
                b_w = 10^rc[["width"]][[i]][["coef"]][2],
                a_w = rc[["velocity"]][[i]][["coef"]][1]))
    
}

df <- df[-1, ] 

write.csv(df, file = 'D:/siletz_q2k/02_input/rating_curve_coefficients.csv')






















