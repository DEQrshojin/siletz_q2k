rm(list = ls()); cat('\014')

df <- readRDS("C:/siletz_tmdl/05_misc/figures/t_corr/2004_ctsi/t_corr_data.RData")

source("C:/siletz_tmdl/04_scripts/02_q2k/02_R/bcs_functions_q2k.R")

df <- df[['tmpW']]

library(ggplot2)

windows(12, 12)

pl <- ggplot(df, aes(date, tmpC)) + geom_line() + theme_classic() + facet_wrap(.~ stns, ncol = 4); pl

df$dsO2 <- do_sat(temp = df$tmpC, elev = df$elev)

temp <- reshape2::dcast(df, date ~ stns, value.var = 'tmpC', fun.aggregate = mean)

dsO2 <- reshape2::dcast(df, date ~ stns, value.var = 'dsO2', fun.aggregate = mean)

temp <- temp[, -which(names(temp) == 'SRCTS')]; dsO2 <- dsO2[, -which(names(dsO2) == 'SRCTS')]

# Now bring in the estimated data to fill in times where CTSI data don't exist
df <- readRDS('C:/siletz_tmdl/01_inputs/02_q2k/RData/T_strm_BCs_2004_2017.RData')

tmp2 <- df[["temp"]]; dO22 <- df$DO

tmp2 <- tmp2[which(tmp2$date %in% temp$date), ]

dO22 <- dO22[which(dO22$date %in% temp$date), ]

tmpF <- dO2F <- temp # flag DFs for estimated/measured data 

for (i in 2 : length(temp)) {
  
  colm <- which(names(tmp2) == names(temp)[i])
  
  tmpF[, i] <- is.nan(temp[, i]); dO2F <- is.nan(dsO2[, i])
  
  temp[, i] <- ifelse(is.nan(temp[, i]), tmp2[, i], temp[, i])
  
  dsO2[, i] <- ifelse(is.nan(dsO2[, i]), dO22[, i], dsO2[, i])
  
}
 
oOut <- list(flags = list(temp = tmpF, dsO2 = dO2F),
             dsO2  = dsO2,
             temp  = temp,
             sites = df$sites)

saveRDS(oOut, 'C:/siletz_tmdl/01_inputs/02_q2k/RData/ctsi_T_DO_2004.RData')








frmt <- '%Y-%m-%d'; tz = 'America/Los_Angeles'

dtes <- data.frame(perd = c('cw', 'sp'),
                   strD = as.POSIXct(c('2004-07-07', '2004-09-01'), format = frmt, tz = tz),
                   endD = as.POSIXct(c('2004-08-29', '2004-10-16'), format = frmt, tz = tz),
                   stringsAsFactors = F)

# Bring in flows to calculate headwaters
flow <- readRDS("C:/siletz_tmdl/02_outputs/01_hspf/base_rchQLC_NOx.RData")

flow <- flow[[1]][, 1 : 3]

flow <- flow[which(flow$Date >= dtes$strD[1] & flow$Date <= dtes$endD[2]), ]

# Read in the Temp & DO data
data <- read.csv(paste0('C:/siletz_tmdl/05_misc/figures/t_corr/2004_ctsi/estim',
                        'ated_trib_inflow_T_DO.csv'), stringsAsFactors = F)

data$date <- as.POSIXct(data$date, '%Y-%m-%d %H:%M:%S', tz = 'America/Los_Angeles')

# Deal with the headwaters first
HW <- data[which(data$stns %in% c('NthFk', 'SthFk')), ]

HWTC <- dcast(HW, date ~ stns, value.var = 'tmpC_F', fun.aggregate = mean)

HWDO <- dcast(HW, date ~ stns, value.var = 'dsO2_F', fun.aggregate = mean)

flow$cnfl <- flow$Bas1 + flow$Bas2

HW <- data.frame(date = flow$Date,
                 tmpC = (HWTC$NthFk * flow$Bas1 + HWTC$SthFk * flow$Bas2) / flow$cnfl,
                 dsO2 = (HWDO$NthFk * flow$Bas1 + HWDO$SthFk * flow$Bas2) / flow$cnfl,
                 stringsAsFactors = F)

row.names(HW) <- format(flow$Date, 'X%Y.%m.%d.%H.%M.%S')

HWCW <- HW[which(HW$date >= dtes$strD[1] & HW$date <= dtes$endD[1]), ]

HWSP <- HW[which(HW$date >= dtes$strD[2] & HW$date <= dtes$endD[2]), ]

path <- 'C:/siletz_tmdl/01_inputs/02_q2k/bc_hdwr/'

write.csv(x = HWCW, paste0(path, 'T_DO_scen_021_cw.csv'))

write.csv(x = HWSP, paste0(path, 'T_DO_scen_021_sp.csv'))

# Now deal with the tribs
data <- data[-which(data$stns %in% c('NthFk', 'SthFk')), ]

unique(data$rch)

data <- data %>% arrange(rch, date)

IFCW <- data[which(data$date >= dtes$strD[1] & data$date <= dtes$endD[1]), 
            c(3, 2, 4, 5)]

IFSP <- data[which(data$date >= dtes$strD[2] & data$date <= dtes$endD[2]), 
             c(3, 2, 4, 5)]

View(IFCW)

path <- 'C:/siletz_tmdl/01_inputs/02_q2k/bc_infw/'

write.csv(x = IFCW, paste0(path, 'T_DO_scen_021_cw.csv'))

write.csv(x = IFSP, paste0(path, 'T_DO_scen_021_sp.csv'))
