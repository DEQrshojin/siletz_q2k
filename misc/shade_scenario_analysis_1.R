# Read in Scenario 4 results.
# Pick lowest DO for each year and use that as the basis of evaluation/load cap
# Loop through each year and extract DO from 4, 7, and 8 for the low DO dates

library(ggplot2); library(lubridate); library(dplyr)

# DATA IMPORT AND ORGANIZATION ----
rm(list = ls()); cat('\014')

# READ IN THE DATA STARTING WITH 'CURRENT CONDITIONS'
source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/proc_results.R')

base <- readRDS("C:/siletz_tmdl/02_outputs/02_q2k/base/results_summary.RData")

yrs  <- c(2006, 2014, 2015)

rchs <- c(4, 7, 8)

for (i in 1 : 3) {

  tmp1 <- base[["excr"]][["sp_DO_X"]][which(year(base$excr$sp_DO_X$dte) == yrs[i]), ]

  tmp1 <- tmp1[which(tmp1$`7DS_V` == min(tmp1$`7DS_V`)), ]

  if (i == 1) {tmp2 <- tmp1} else {tmp2 <- rbind(tmp2, tmp1)}

}

dt <- list(base = base)

scen <- paste0('S0', 40 : 56)

# READ AND PROCESS THE OTHER SCENARIO DATA
for (i in 1 : length(scen)) {

  dt[[i + 1]] <- readRDS(paste0('C:/siletz_tmdl/02_outputs/02_q2k/', scen[i],
                                '/results_summary.RData'))

  names(dt)[i + 1] <- scen[i]

}

for (i in 1 : length(dt)) {

  tmp3 <- dt[[i]][[7]][which(dt[[i]][[7]]$dte %in% tmp2$dte), c(1, 2, 4)]

  tmp3 <- tmp3[which(tmp3$rch %in% rchs), ]

  tmp3$scen <- names(dt)[i]

  if (i == 1) {excr <- tmp3} else {excr <- rbind(excr, tmp3)}

}

excr$year <- year(excr$dte)

# PLOT TO SEE CRITICAL LOW DO vs. SCENARIO
pl <- ggplot(excr, aes(x = scen, y = stat)) + geom_point() +
      theme_classic() + geom_hline(yintercept = 95, linetype = 'dashed') +
      scale_y_log10() + facet_grid(year ~ rch)

windows(12, 12)

pl

# READ AND PROCESS ALL OF THE FOR THE THERMAL LOAD CALCULATIONS
yrs  <- c('YR06', 'YR14', 'YR15')

path <- 'C:/siletz_tmdl/01_inputs/02_q2k/mt_CC/base/'

# Import cloud cover
for (i in 1 : 3) {

  # Use base (base) scenario for cloud cover
  tmp5 <- read.csv(paste0(path, '/cCov_', yrs[i], '.csv'), stringsAsFactors = F)

  tmp5 <- data.frame(t(tmp5), stringsAsFactors = F)

  tmp5$date <- as.POSIXct(row.names(tmp5), 'X%Y.%m.%d.%H.%M.%S',
                          tz = 'America/Los_Angeles')

  row.names(tmp5) <- 1 : nrow(tmp5)

  tmp5 <- tmp5[which(floor_date(tmp5$date, 'day') %in% tmp2$dte), c(11, 1 : 10)]

  if (i == 1) {cCov <- tmp5} else {cCov <- rbind(cCov, tmp5)}

}

# Direct solar above topographic features (SR1)
SR1  <- read.csv("C:/siletz_tmdl/02_outputs/03_hs/CC/Heat_SR1_09_sep.csv",
                 stringsAsFactors = F, skip = 6)

# SR1_BU <- SR1
# SR1 <- SR1_BU

SR1$Datetime <- round((SR1$Datetime - SR1$Datetime[1]) * 24 * 60 * 60, 0)

SR1$Datetime <- as.POSIXct(SR1$Datetime, origin = '2017-09-01',
                           tz = 'America/Los_Angeles') + hours(7)

SR1$day <- day(SR1$Datetime); SR1$hour <- hour(SR1$Datetime)

days <- day(as.POSIXct(tmp2$dte, '%Y-%m-%d', tz = 'America/Los_Angeles'))

SR1 <- SR1[which(SR1$day %in% days), ]

SR1 <- aggregate(SR1[, 2 : (length(SR1) - 2)], by = list(SR1$day, SR1$hour),
                 FUN = 'mean')

# transpose for aggregating on reach
SR1 <- data.frame(t(SR1), stringsAsFactors = F)

SR1$RKM <- row.names(SR1)

hdrs <- SR1[1 : 2, 1 : 72]; SR1 <- SR1[3 : nrow(SR1), ]

# Rename and reorder the columns
hdrs <- data.frame(t(hdrs))

hdrs$colNme <- ifelse(
  hdrs$Group.1 == 1, paste0('D150901_H', addZ(hdrs$Group.2)),
  ifelse(hdrs$Group.1 == 15, paste0('D060915_H', addZ(hdrs$Group.2)),
         paste0('D140924_H', addZ(hdrs$Group.2)))
)

names(SR1) <- c(hdrs$colNme, 'RKM')

SR1 <- SR1[, order(names(SR1))]

# Format RKM and merge with the nodes (ndes)
SR1$RKM <- as.numeric(gsub('X', '', SR1$RKM))

# Bring in stream widths to calculate surface area
dtes <- c('2006-07-01', '2014-07-01', '2015-07-01')

for (i in 1 : 3) {

  tmp6 <- read_q2k_out_2(mOut = 'C:/siletz_tmdl/02_outputs/02_q2k/base',
                         strD = dtes[i], pars = 'Width')

  tmp6$Time <- round((tmp6$Time - tmp6$Time[1]) * 24 * 60 * 60, 0)

  tmp6$Time <- as.POSIXct(tmp6$Time, origin = dtes[i],
                          tz = 'America/Los_Angeles') + hours(7)

  # Reduce df to output on that day
  tmp6 <- tmp6[which(floor_date(tmp6$Time, 'day') == tmp2$dte[i]), ]

  # Aggregate to hourly
  tmp6 <- aggregate(tmp6$width, by = list(tmp6$Reach, tmp6$Distance, hour(tmp6$Time)),
                    FUN = mean)

  tmp6$year <- year(tmp2$dte[i])

  if (i == 1) {
    modl <- unique(tmp6[, 1 : 2])
    wdth <- tmp6
  } else {
    wdth <- rbind(wdth, tmp6)
  }

}

names(wdth) <- c('rch', 'rkm', 'hour', 'wdth', 'year')

wdth <- wdth[which(wdth$rch != 0), ]

wdth_BU <- wdth
# wdth <- wdth_BU

modl$lgth<- 0

# Calculate segment length
for (i in 1 : 10) {modl$lgth[i] <- (modl$Group.2[i + 1] - modl$Group.2[i]) * 1000}

wdth <- merge(wdth, modl[, c(1, 3)], by.x = 'rch', by.y = 'Group.1', all.y = F)

wdth$SA_m2 <- wdth$wdth * wdth$lgth

# Read in the nodes with HSPF basin designations
ndes <- read.csv("C:/siletz_tmdl/01_inputs/03_hs/slz_nodes_HSPF.csv",
                 stringsAsFactors = F)

# Isolate the basins and river km
ndes <- ndes[, c(6, 105)]

# Clean up
ndes[which(ndes$HSPF_Bas == 1 | ndes$HSPF_Bas == 2), 2] = 3

ndes[which(ndes$HSPF_Bas == 17), 2] = 16

q2kR <- data.frame(basn = unique(ndes$HSPF_Bas), q2kR = 1 : 10, stringsAsFactors = F)

ndes <- merge(ndes, q2kR, by.x = 'HSPF_Bas', by.y = 'basn')

# Merge SR1 and nodes (with Q2K reaches)
SR1 <- merge(SR1, ndes[, 2 : 3], by.x = 'RKM', by.y = 'STREAM_KM', all.x = F)

# Aggregate to the mean of reach solar now (but first calc SD)
SR1_SD <- aggregate(SR1[2 : (length(SR1) - 1)], by = list(SR1$q2kR), FUN = 'sd')

SR1 <- aggregate(SR1[2 : (length(SR1) - 1)], by = list(SR1$q2kR), FUN = 'mean')

names(SR1)[1] <- 'Q2KR'

write.csv(SR1, 'C:/siletz_tmdl/02_outputs/03_hs/critical_SP_SR1_x_reach.csv',
          row.names = F)

# Read in effective shade
scen <- names(dt)

eShd <- list()

fils <- paste0('C:/siletz_tmdl/01_inputs/02_q2k/mt_eShd/', scen, '/eShd_YR06.csv')

dte2 <- tmp2$dte; year(dte2) <- 2017

for (i in 1 : length(dt)) {

  eShd[[i]] <- read.csv(fils[i])
  
  colX <- which(floor_date(
    as.POSIXct(names(eShd[[i]]),'X%Y.%m.%d.%H.%M.%S', tz = 'America/Los_Angeles'),
    'day') %in% dte2
  )

  eShd[[i]] <- eShd[[i]][, colX]

  names(eShd)[i] <- scen[i]

}

# Save data as a list() object, clear the environment and reimport:
save <- list(allD = dt,
             solr = SR1,
             excr = excr,
             wdth = wdth,
             cCov = cCov,
             eShd = eShd,
             dtes = tmp2)

saveRDS(save, 'C:/siletz_tmdl/02_outputs/03_hs/load_cap_solar_calcs.RData')

# MORE REORGANIZING AND CONVERTING ----
rm(list = ls()); cat('\014')

dt <- readRDS('C:/siletz_tmdl/02_outputs/03_hs/load_cap_solar_calcs.RData')

addZ <- function(v) ifelse(v < 10, paste0(0, v), as.character(v))

# REARRANGE CLOUD COVER (cCov) to fit sequential days-of-the-month
# Remove all columns but 1, I forgot they are all the same
dt[['cCov']] <- dt[['cCov']][, 1 : 2]

# Create the unique time identifier code (See solr column headers)
dt[['cCov']]$code <- paste0('D', addZ(year(dt[['cCov']]$date) - 2000),
                            addZ(month(dt[['cCov']]$date)),
                            addZ(day(dt[['cCov']]$date)), '_H',
                            addZ(hour(dt[['cCov']]$date)))
                     
# cloud attenuation: ac = 1 - 0.65cCov^2 (Ecology, 2019, Draft Q2K Theory) 
dt[['cCov']]$ac <- 1 - (dt[['cCov']]$X1)^2

dt[['cCov']] <- dt[['cCov']][, 3 : 4]

# REARRANGE SOLAR (SR1) to fit the sequential days-of-the-month
row.names(dt[['solr']]) <- dt[['solr']]$Q2KR; dt[['solr']] <- dt[['solr']][, -1]

dt[['solr']] <- data.frame(t(dt[['solr']]), stringsAsFactors = F)

dt[['solr']]$code <- row.names(dt[['solr']])

row.names(dt[['solr']]) <- 1 : nrow(dt[['solr']])

# REARRANGE STREAM WIDTH
dt[['dtes']]$year <- year(dt[['dtes']]$dte)

dt[['dtes']]$day <- day(dt[['dtes']]$dte)

dt[['wdth']] <- merge(dt[['wdth']], dt[['dtes']][, 11 : 12]) 

dt[['wdth']]$code <- paste0('D', addZ(dt[['wdth']]$year - 2000), '09',
                            addZ(dt[['wdth']]$day), '_H',
                            addZ(dt[['wdth']]$hour))

dt[['wdth']] <- reshape2::dcast(dt[['wdth']], code ~ rch, value.var = 'SA_m2',
                                fun.aggregate = mean)

# REARRANGE AND ORGANIZE EFFECTIVE SHADE
for (i in 1 : length(dt[['eShd']])) {
  
  tmp1 <- dt[['eShd']][[i]]
  
  tmp1 <- data.frame(t(tmp1), stringsAsFactors = F)
  
  tmp1$date <- as.POSIXct(row.names(tmp1), 'X%Y.%m.%d.%H.%M.%S',
                          tz = 'America/Los_Angeles')
  
  year(tmp1$date) <- ifelse(day(tmp1$date) == 1, 2015,
                            ifelse(day(tmp1$date) == 15, 2006, 2014))

  tmp1$code <- paste0('D', addZ(year(tmp1$date) - 2000), addZ(month(tmp1$date)),
                      addZ(day(tmp1$date)), '_H', addZ(hour(tmp1$date)))
  
  row.names(tmp1) <- 1 : nrow(tmp1)
  
  dt[['eShd']][[i]] <- tmp1[, c(12, 1 : 10)]
  
  # Reorder Shade rows
  dt[['eShd']][[i]] <- dt[['eShd']][[i]][order(dt[['eShd']][[i]]$code), ]
  
}

# CALCULATE SOLAR LOADS ----
# Now take tmp4 and convert the scenarios to upstream solar contribution
# solr  =  ET  x   at x      ac x (1 - RF) x (1 - ES)
# SR1   =  ET  x   at x           (1 - RF)
# solr  =  SR1 x             ac x            (1 - ES)
# SR4   = [SR1 x             ac x            (1 - ES)] * SA (m2)

load <- list()

# Pull out each variable and order based on the code
SR1 <- dt[['solr']][order(dt[['solr']]$code), c(11, 1 : 10)]

SA  <- dt[['wdth']][order(dt[['wdth']]$code), ]

ac <- dt[['cCov']][order(dt[['wdth']]$code), ]

ES <- dt[['eShd']]

# Check to see if they're all ordered correctly ********************************
x <- data.frame(SR1 = SR1$code,
                ac  = ac$code,
                ES  = ES[[i]]$code,
                SA  = SA$code,
                stringsAsFactors = F)

x$c34 <- x$c24 <- x$c23 <- x$c14 <- x$c13 <- x$c12 <- 0

comb <- data.frame(c1 = c(1, 1, 1, 2, 2, 3),
                   c2 = c(2, 3, 4, 3, 4, 4))

for (n in 1 : nrow(comb)) {
  x[, n + 4] <- ifelse(x[, comb[n, 1]] == x[, comb[n, 2]], 0, 1)
}

chck <- colSums(x[5 : 10]) # YEP, ALL GOOD!
# Check to see if they're all ordered correctly ********************************

lCap <- list()

# Now calculate the load capacity
for (i in 1 : length(ES)) {
  
  ES[[i]] <- ES[[i]][order(ES[[i]]$code), ]; load[[i]] <- ES[[i]]
  
  for (j in 2 : length(ES[[i]])) {

    # This calculates the thermal load as a result of direct solar radiation
    # to the individual stream segment in MW/m2 (convert from W/m2)
    # solr          =      SR1 x    ac x           (1 - ES) x SA (m2)
    load[[i]][, j] <- SR1[, j] * ac$ac * (1 - ES[[i]][, j]) * SA[, j] / 10^6
    
  }
  
  # Ordering hierarchy:
  # scenario (scen; 19); date (dte; 3); reach (rch; 3)
  tmp2 <- data.frame(rch  = rep(c(4, 7, 8), 3),
                     date = c(rep(dt[['dtes']]$dte[1], 3),
                              rep(dt[['dtes']]$dte[2], 3),
                              rep(dt[['dtes']]$dte[3], 3)),
                     scen = names(dt[['eShd']])[i],
                     load = c(sum(load[[i]][ 1 : 24, 2 : 5]),
                              sum(load[[i]][25 : 48, 2 : 8]),
                              sum(load[[i]][49 : 72, 2 : 9]),
                              sum(load[[i]][ 1 : 24, 2 : 5]),
                              sum(load[[i]][25 : 48, 2 : 8]),
                              sum(load[[i]][49 : 72, 2 : 6]),
                              sum(load[[i]][ 1 : 24, 2 : 5]),
                              sum(load[[i]][25 : 48, 2 : 8]),
                              sum(load[[i]][49 : 72, 2 : 9])))

  if (i == 1) {lCap <- tmp2} else {lCap <- rbind(lCap, tmp2)}
  
}

lCap <- cbind(lCap, dt[['excr']]$stat)

names(lCap)[4 : 5] <- c('load_MW', 'doSat')

# PLOT TO SEE CRITICAL LOW DO vs. SCENARIO
windows(12, 12)

pl <- ggplot(lCap, aes(x = load_MW, y = doSat)) + geom_point() +
      theme_classic() + geom_hline(yintercept = 95, linetype = 'dashed') +
      scale_x_log10() + facet_grid(date ~ rch); pl

ggsave(filename = 'C:/siletz_tmdl/02_outputs/02_q2k/load_cap/solar_load_cap.png',
       plot = pl, width = 11, height = 8.5, units = 'in', dpi = 300)

dt[['loads_MW_m2']] <- load; dt[['lCap']] <- lCap

saveRDS(dt, 'C:/siletz_tmdl/02_outputs/02_q2k/load_cap/load_cap_solar_calcs.RData')

rm(list = ls()); cat('\014')
















