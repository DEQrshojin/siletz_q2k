rm(list = ls()); cat('\014')

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
