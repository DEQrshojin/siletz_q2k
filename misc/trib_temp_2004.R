library(reshape2); library(ggplot2); library(gridExtra); library(lubridate)
library(dplyr)

rm(list = ls()); cat('\014')

source('c:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_wq_calib_hspf.R')
source('c:/siletz_tmdl/04_scripts/02_q2k/02_R/met_functions_q2k.R')
source('c:/siletz_tmdl/04_scripts/02_q2k/02_R/bcs_functions_q2k.R')

# PROCESS DATA ----
# Read in the CTSI data
ctsi <- read.csv(paste0('C:/siletz_misc/from_tmdl_drive/001_data/ctsi_temperat',
                        'ure/siletz_ctsi_temp_2004.csv'), stringsAsFactors = F)

ctsi$date <- as.POSIXct(ctsi$date, '%Y-%m-%d %H:%M', tz = 'America/Los_Angeles')

# Read in the trib names
desc <- read.csv(paste0('C:/siletz_misc/from_tmdl_drive/001_data/ctsi_temperat',
                        'ure/stn_Tair_corr.csv'), stringsAsFactors = F)

# Lookup the Reach for the trib 
ctsi <- merge(ctsi, desc[, c(1, 4)], by.x = 'STAID_UD', by.y = 'STN', all = T)

names(ctsi) <- c('stns', 'site', 'date', 'tmpC', 'type', 'rkm', 'q2kr')

ctsi$q2kr <- ifelse(ctsi$stns == 'SthFk', 'SF',
                    ifelse(ctsi$stns == 'NthFk', 'NF',
                           ifelse(ctsi$stns == 'SRLYY', 'LY', ctsi$q2kr)))

path <- 'C:/siletz_tmdl/05_misc/figures/t_corr/2004_ctsi/'

plDt <- as.POSIXct(c('2004-06-01', '2004-11-01'), '%Y-%m-%d',
                   tz = 'America/Los_Angeles')

# Graph first as reference 
# pl <- ggplot(ctsi, aes(date, tmpC, color = type)) + geom_line() +
#       theme_classic() + scale_x_datetime(limits = plDt) +
#       facet_wrap(.~ stns, ncol = 4)
# 
# ggsave(filename = paste0(path, 'stream_temp_2004_all_data.png'), plot = pl,
#        width = 11, height = 8.5, units = 'in', dpi = 300)

# Isolate stations that need extension; Remove bogus data
incl <- c('11248', '11249', '38916', '38930', '38939', 'BtlCk', 'DewCk', 'GrvCk',
          'MCSlz', 'NthFk', 'SthFk', 'SunCk')

ctsi <- ctsi[which(ctsi$stns %in% incl), ]

ctsiBU <- ctsi

indS <- c('38916', 'DewCk', 'MCSlz'); resS <- setdiff(incl, indS)

ctsi[which(ctsi$stns %in% indS), 'type'] <- 'Indicator'

ctsi[which(ctsi$stns %in% resS), 'type'] <- 'Response'

# Graph first as reference 
# pl <- ggplot(ctsi, aes(date, tmpC, color = type)) + geom_line() +
#       theme_classic() + scale_x_datetime(limits = plDt) +
#       facet_wrap(.~ stns, ncol = 4)
# 
# ggsave(filename = paste0(path, 'stream_temp_2004_trib_stns.png'), plot = pl,
#        width = 11, height = 8.5, units = 'in', dpi = 300)

# Remove data of before of after large gaps (see graphs)
audt <- read.csv(paste0(path, 'ctsi_audit_dates.csv'), stringsAsFactors = F)

for (i in 2 : 3) {
  audt[, i] <- as.POSIXct(audt[, i], '%Y-%m-%d %H:%M', tz = 'America/Los_Angeles')
}

for (i in 1 : nrow(audt)) {
  
  tmp1 <- ctsi[which(ctsi$stns == audt$stns[i]), ]
  
  if (!is.na(audt$start[i])) {tmp1 <- tmp1[which(tmp1$date > audt$start[i]), ]}
  
  if (!is.na(audt$end[i]))   {tmp1 <- tmp1[which(tmp1$date < audt$end[i]), ]}
  
  if (i == 1) {tmp2 <- tmp1} else {tmp2 <- rbind(tmp2, tmp1)}
  
}

# Graph audited stations to check -- GOOD! 
# pl <- ggplot(ctsi, aes(date, tmpC, color = type)) + geom_line() +
#       theme_classic() + scale_x_datetime(limits = plDt) +
#       facet_wrap(.~ stns, ncol = 4)
# 
# ggsave(filename = paste0(path, 'stream_temp_2004_audited.png'), plot = pl,
#        width = 11, height = 8.5, units = 'in', dpi = 300)

ctsi <- tmp2

x <- unique(ctsi$stns[which(ctsi$type == 'Indicator')])

y <- setdiff(unique(ctsi$stns), x)

# PERFORM REGRESSIONS -- DAILY MAX/MIN ----
ctsI <- aggregate(x = ctsi$tmpC, FUN = 'min', na.rm = T,
                  by = list(ctsi$type, ctsi$stns, floor_date(ctsi$date, 'day')))

ctsX <- aggregate(x = ctsi$tmpC, FUN = 'max', na.rm = T,
                  by = list(ctsi$type, ctsi$stns, floor_date(ctsi$date, 'day')))

names(ctsI) <- names(ctsX) <- c('type', 'stns', 'date', 'tmpC')

ctsI <- ctsI %>% arrange(type, stns, date)

ctsX <- ctsX %>% arrange(type, stns, date)

ctsD <- cbind(ctsI, ctsX$tmpC); names(ctsD)[4 : 5] <- c('tMin', 'tMax')

# Initialize the regression summary data table
summ <- data.frame(matrix(data = NA, nrow = length(x) * length(y), ncol = 12,
                          dimnames = list(1 : (length(x) * length(y)),
                                          c('indc', 'resp', 'mI', 'bI', 'pmI',
                                            'pbI', 'R2I', 'mX', 'bX', 'pmX',
                                            'pbX', 'R2X'))),
                   stringsAsFactors = F)

# Start with indicator variable loop
for (i in 1 : length(x)) {
  
  a <- ctsD[which(ctsD$stns == x[i]), c('date', 'tMin', 'tMax')]
  
  names(a)[2 : 3] <- c('tMin_Ind', 'tMax_Ind')
  
  rowX <- 9 * (i - 1) + (1 : 9)

  for (j in 1 : length(y)) {
    
    b <- ctsD[which(ctsD$stns == y[j]), c('date', 'tMin', 'tMax')]
    
    names(b)[2 : 3] <- c('tMin_Rsp', 'tMax_Rsp')
    
    c <- merge(a, b, all = T); c <- c[which(complete.cases(c)), ]
    
    r <- list(min = summary(lm(c$tMin_Rsp ~ c$tMin_Ind)),
              max = summary(lm(c$tMax_Rsp ~ c$tMax_Ind))) 
    
    summ[rowX[j], ] <- c(x[i], y[j], r[[1]][[4]][2, 1], r[[1]][[4]][1, 1],
                         r[[1]][[4]][2, 4], r[[1]][[4]][1, 4], r[[1]][[8]],
                         r[[2]][[4]][2, 1], r[[2]][[4]][1, 1],
                         r[[2]][[4]][2, 4], r[[2]][[4]][1, 4], r[[2]][[8]])

  }
}

for (i in 3 : length(summ)) {summ[, i] <- as.numeric(summ[, i])}

# Output to csv
# write.csv(summ, paste0(path, 'station_correlations_daily_minmax_2004.csv'), row.names = F)

# Station 38916-ORDEQ has the highest R2 for each of the response stations
r <- summ[which(summ$indc == '38916'), ]

# First expand the time series to match that of 38916
ts <- ctsi$date[which(ctsi$stns == '38916')]; ts <- ts[order(ts)]

# Cast to wide based on Q2Kr
ctsW <- dcast(data = ctsi, formula = date ~ stns, value.var = 'tmpC',
              fun.aggregate = mean)

dtes = as.POSIXct(c('2004-07-07', '2004-10-16'), '%Y-%m-%d',
                  tz = 'America/Los_Angeles')

ctsW <- ctsW[which(ctsW$date >= dtes[1] & ctsW$date <= dtes[2]), c('date', y, x)]

ctsE <- ctsW[, c('date', y)]

for (i in 2 : length(ctsE)) {
  
  ctsE[, i] <- r[which(r$resp == names(ctsE)[i]), 3] * ctsW$`38916` +
    r[which(r$resp == names(ctsE)[i]), 4]  
  
}

ctsW <- melt(ctsW, id.vars = 'date', value.name = 'tmpC', variable.name = 'stns')

ctsE <- melt(ctsE, id.vars = 'date', value.name = 'tmpC', variable.name = 'stns')

ctsW$srce <- 'Observed'; ctsE$srce <- 'Estimate'

ctsL <- rbind(ctsW, ctsE)

# Plot to verify
# pl <- ggplot(ctsL, aes(date, tmpC, color = srce)) + geom_line(size = 0.25) +
#       theme_classic() + facet_wrap(.~ stns, ncol = 4)
# 
# ggsave(filename = paste0(path, 'stream_temp_2004_estimated.png'), plot = pl,
#        width = 11,height = 8.5, units = 'in', dpi = 300)

# ESTIMATE TS -- DAILY MIN/MAX ----
summ <- summ[which(summ$indc == '38916'), ]

# Calculate the daily min/max times of the indicator station (38916)
indc <- ctsi[which(ctsi$stns == '38916'), c('date', 'tmpC')]

resp <- ctsi[which(ctsi$stns %in% y), c('date', 'stns', 'tmpC')]

indc <- indc[order(indc$date), ]

# Cut off the end days (partial)
dtes <- floor_date(c(indc$date[1], indc$date[nrow(indc)]), 'days')

indc <- indc[-which(floor_date(indc$date, 'days') %in% dtes), ]

indc <- rbind(indc, data.frame(date = dtes[2], tmpC = indc$tmpC[nrow(indc)]))

IXtm <- minmax_time(indc)

indI <- aggregate(indc$tmpC, by = list(floor_date(indc$date, 'day')), FUN = 'min')

indX <- aggregate(indc$tmpC, by = list(floor_date(indc$date, 'day')), FUN = 'max')

lngt <- 1 : (nrow(indI) - 1)

# Initialize the data frame of the estimated T
ts <- seq(indX$Group.1[1], indX$Group.1[nrow(indX)], 3600)

temp <- data.frame(date = ts, tmpC = 0, stringsAsFactors = F)

# Iterate through each Response station and calculate the hourly Temp TS
for (i in 1 : length(y)) {

  tmp1 <- data.frame(date = IXtm$date,
                     min  = indI$x[lngt] * summ$mI[i] + summ$bI[i],
                     max  = indX$x[lngt] * summ$mX[i] + summ$bX[i])
  
  for (j in 1 : nrow(IXtm)) {

    if (j == 1) { # Special case: no initial temp
      
      cond <- which(temp$date <= IXtm$min[j])
      
      temp[cond, 2] <- calc_sin(t1 = tmp1$min[j], t2 = tmp1$min[j],
                                x = temp$date[cond])
      
      # Now calculate the 
        
    } else if (j == nrow(IXtm)) { # Special case: no final temp
      
      cond <- which(temp$date >= IXtm$max[j])
      
      temp[cond, 2] <- calc_sin(t1 = tmp1$max[j], t2 = tmp1$max[j],
                                x = temp$date[cond])
      
    }
    
    # Calculate AM to PM time series temperatures
    cond <- which(temp$date >= IXtm$min[j] & temp$date <= IXtm$max[j])
    
    temp[cond, 2] <- calc_sin(t1 = tmp1$min[j], t2 = tmp1$max[j],
                              x = temp$date[cond])
    
    # Calculate PM to AM (next day) time series temperatures
    cond <- which(temp$date >= IXtm$max[j] & temp$date <= IXtm$min[j + 1])
    
    temp[cond, 2] <- calc_sin(t1 = tmp1$max[j], t2 = tmp1$min[j + 1],
                              x = temp$date[cond])

    }
    
  if (i == 1) {tmp3 <- temp} else {tmp3 <- cbind(tmp3, temp$tmpC)}
  
  names(tmp3)[i + 1] <- y[i]
  
}

ctsE <- melt(tmp3, id.vars = 'date', value.name = 'tmpC', variable.name = 'stns')

ctsX <- ctsi[which(ctsi$stns %in% c('38916', 'DewCk', 'MCSlz')), c('date', 'stns', 'tmpC')]

ctsE <- rbind(ctsE, ctsX)

ctsE <- ctsE[which(ctsE$date %in% unique(ctsL$date)), ]

regs <- read.csv("C:/siletz_tmdl/01_inputs/02_q2k/summary_NN.csv")

ctsE <- merge(ctsE, regs[, c('resp', 'rch', 'elv')], by.x = 'stns',
              by.y = 'resp', all.y = F)

# Calculate DO (assuming saturation)
ctsE$dsO2 <- do_sat(temp = ctsE$tmpC, elev = ctsE$elv)

ctsE <- ctsE %>% arrange(stns, date)

ctsE <- ctsE[, c(1, 4, 2, 3, 6)]

names(ctsE)[4 : 5] <- c('tmpC_E', 'dsO2_E')

# Bring in the observed data and substitute estimations if no observations
stns <- unique(ctsE$stns)

for (i in 1 : length(stns)) {
  
  tmp1 <- ctsE[which(ctsE$stns == stns[i]), ]
  
  tmp2 <- ctsi[which(ctsi$stns == stns[i]), ]
  
  tmp3 <- merge(tmp1, tmp2[, c('date', 'tmpC')], all.x = T, all.y = F)
  
  names(tmp3)[6] <- 'tmpC_O'
  
  if (i == 1) {temp <- tmp3} else {temp <- rbind(temp, tmp3)}

}

ctsE <- temp

ctsE <- merge(ctsE, regs[, c('resp', 'elv')], by.x = 'stns',
              by.y = 'resp', all.y = F)

# Calculate DO (assuming saturation)
ctsE$dsO2_O <- do_sat(temp = ctsE$tmpC_O, elev = ctsE$elv)

ctsE <- ctsE[, -7]

# Plot to verify
# windows(12, 12)
# 
# pl <- ggplot() + theme_classic() + facet_wrap(.~ stns, ncol = 4) +
#       geom_line(data = ctsE, aes(date, tmpC_E), color = 'darkred', alpha = 0.3) +
#       geom_line(data = ctsE, aes(date, tmpC_O), color = 'darkblue', alpha = 0.3); pl
# 
# pl <- ggplot() + theme_classic() + facet_wrap(.~ stns, ncol = 4) +
#       geom_line(data = ctsE, aes(date, dsO2_E), color = 'darkred', alpha = 0.3) +
#       geom_line(data = ctsE, aes(date, dsO2_O), color = 'darkblue', alpha = 0.3); pl

ctsE$tmpC_F <- ifelse(is.na(ctsE$tmpC_O), ctsE$tmpC_E, ctsE$tmpC_O)

ctsE$dsO2_F <- ifelse(is.na(ctsE$dsO2_O), ctsE$dsO2_E, ctsE$dsO2_O)

ctsE <- ctsE[, -c(4 : 7)]

# Plot to verify
windows(12, 12)

pl <- ggplot() + theme_classic() + facet_wrap(.~ stns, ncol = 4) +
      geom_line(data = ctsE, aes(date, tmpC_F), color = 'darkred'); pl

pl <- ggplot() + theme_classic() + facet_wrap(.~ stns, ncol = 4) +
      geom_line(data = ctsE, aes(date, dsO2_F), color = 'darkred'); pl

write.csv(ctsE, paste0(path, 'estimated_trib_inflow_T_DO.csv'), row.names = F)

# SCRATCH ----
# # Set up the scenario dates
# dtes <- data.frame(perd = c('cw', 'sp'),
#                    strD = as.POSIXct(c('2004-07-07', '2004-09-01'), '%Y-%m-%d',
#                                      tz = 'America/Los_Angeles'),
#                    endD = as.POSIXct(c('2004-08-29', '2004-10-16'), '%Y-%m-%d',
#                                      tz = 'America/Los_Angeles'),
#                    stringsAsFactors = F)
# 
# cw <- trib[which(trib$date >= dtes$strD[1] & trib$date <= dtes$endD[1]), ]
# 
# sp <- trib[which(trib$date >= dtes$strD[2] & trib$date <= dtes$endD[2]), ]
# 
# # Plot and facet on reach to check continuity
# path <- 'C:/siletz_tmdl/01_inputs/02_q2k/bc_figures/T_h20_2004/'
# 
# plCW <- ggplot(cw, aes(date, tmpC, color = resp)) + geom_line() +
#         theme_classic() + facet_wrap(.~ stns, ncol = 4)
# 
# ggsave(filename = paste0(path, 'cw_stream_temp_2004.png'), plot = plCW,
#        width = 11,height = 8.5, units = 'in', dpi = 300)
# 
# plSP <- ggplot(sp, aes(date, tmpC, color = resp)) + geom_line() +
#         theme_classic() + facet_wrap(.~ stns, ncol = 4)
# 
# ggsave(filename = paste0(path, 'sp_stream_temp_2004.png'), plot = plSP,
#        width = 11, height = 8.5, units = 'in', dpi = 300)
# Plot to verify
# ctsE <- rbind(ctsE, ctsW)
# 
# ctsE$srce <- 'Estimate'
# 
# pl <- ggplot(ctsE, aes(date, tmpC, color = srce)) + geom_line(size = 0.25) +
#   theme_classic() + facet_wrap(.~ stns, ncol = 4)
# 
# ggsave(filename = paste0(path, 'stream_temp_2004_est_minmax.png'), plot = pl,
#        width = 11,height = 8.5, units = 'in', dpi = 300)

# PERFORM REGRESSIONS -- ALL DATA ----
# Now regress stations against data for each station w/ complete SP time series
# r <- list()
# 
# for (i in 1 : length(x)) {
#   
#   r[[i]] <- list(); names(r)[i] <- x[i]
#   
#   a <- ctsi[which(ctsi$stns == x[i]), c('date', 'tmpC')]; names(a)[2] <- 'Indicator'
#   
#   for (j in 1 : length(y)) {
#     
#     b <- ctsi[which(ctsi$stns == y[j]), c('date', 'tmpC')]; names(b)[2] <- 'Response'
#     
#     c <- merge(a, b, all = T); c <- c[which(complete.cases(c)), ]
#     
#     r[[i]][[j]] <- summary(lm(c$Response ~ c$Indicator))
#     
#     m <- r[[i]][[j]][[4]][2, 1]; b <- r[[i]][[j]][[4]][1, 1]
#     
#     # Plot for selected station 38916
#     if (x[i] == '38916') {
#       
#       lbls <- format_R2(m  = m, b = b, r2 = r[[i]][[j]][[8]][1])
#       
#       xlim <- c(0.95 * min(c$Indicator, na.rm = T),
#                 1.05 * max(c$Indicator, na.rm = T))
#       
#       rlne <- data.frame(x = xlim, y = c(m * xlim[1] + b, m * xlim[2] + b),
#                          stringsAsFactors = F)
#       
#       xlab <- paste0('Indicator: Station ', x[i], ' Temp (oC)')
#       
#       ylab <- paste0('Response: Station ', y[j], ' Temp (oC)')
#       
#       p2 <- ggplot(c, aes(x = Indicator, y = Response)) + geom_point() +
#         theme_classic() + xlab(xlab) + ylab(ylab) + 
#         geom_line(data = rlne, aes(x = x, y = y), size = 1.3,
#                   linetype = 'dashed', color = 'darkred') +
#         annotate('text', x = 1.5, y = 0.5, parse = T, label = lbls,
#                  hjust = 0, size = 4.5)
#       
#       c  <- melt(c, id.vars = 'date', value.name = 'temp', variable.name = 'vars')
#       
#       p1 <- ggplot(c, aes(x = date, y = temp, color = vars)) + geom_line() +
#         theme_classic() + ylab('Temperature (oC)') +
#         theme(axis.title.x = element_blank(),
#               legend.position = c())
#       
#       p3 <- grid.arrange(p1, p2)
#       
#       ggsave(filename = paste0(path, 'sta_', x[i], '_', y[j], '.png'),
#              plot = p3, width = 11, height = 8.5, units = 'in', dpi = 300)
#       
#     }
#   }
#   
#   names(r[[i]]) <- y
#   
# }
# 
# # Distill to a data table
# summ <- data.frame(matrix(data = NA, nrow = length(x) * length(y), ncol = 7,
#                           dimnames = list(1 : (length(x) * length(y)),
#                                           c('indc', 'resp', 'm', 'b', 'pm',
#                                             'pb', 'R2'))))
# 
# for (i in 1 : length(x)) {
#   
#   rowX <- 9 * (i - 1) + (1 : 9)
#   
#   for (j in 1 : length(y)) {
#     
#     rowY <- rowX[j]
#     
#     summ[rowY, ] <- c(x[i], y[j],             # Indicator, response
#                       r[[i]][[j]][[4]][2, 1], # Slope, m  
#                       r[[i]][[j]][[4]][1, 1], # Intercept, b
#                       r[[i]][[j]][[4]][2, 4], # Probability, m
#                       r[[i]][[j]][[4]][1, 4], # Probability, b
#                       r[[i]][[j]][[8]])       # R2
#     
#   }
# } 
# 
# for (i in 3 : length(summ)) {summ[, i] <- as.numeric(summ[, i])}
# 
# # Output to csv
# write.csv(summ, paste0(path, 'station_correlations_2004.csv'), row.names = F)
# 
# # ESTIMATE TS -- ALL DATA ----
# # Station 38916-ORDEQ has the highest R2 for each of the response stations
# r <- summ[which(summ$indc == '38916'), ]
# 
# # First expand the time series to match that of 38916
# ts <- ctsi$date[which(ctsi$stns == '38916')]; ts <- ts[order(ts)]
# 
# # Cast to wide based on Q2Kr
# ctsW <- dcast(data = ctsi, formula = date ~ stns, value.var = 'tmpC',
#               fun.aggregate = mean)
# 
# dtes = as.POSIXct(c('2004-07-07', '2004-10-16'), '%Y-%m-%d',
#                   tz = 'America/Los_Angeles')
# 
# ctsW <- ctsW[which(ctsW$date >= dtes[1] & ctsW$date <= dtes[2]), c('date', y, x)]
# 
# ctsE <- ctsW[, c('date', y)]
# 
# for (i in 2 : length(ctsE)) {
#   
#   ctsE[, i] <- r[which(r$resp == names(ctsE)[i]), 3] * ctsW$`38916` +
#     r[which(r$resp == names(ctsE)[i]), 4]  
#   
# }
# 
# ctsW <- melt(ctsW, id.vars = 'date', value.name = 'tmpC', variable.name = 'stns')
# 
# ctsE <- melt(ctsE, id.vars = 'date', value.name = 'tmpC', variable.name = 'stns')
# 
# ctsW$srce <- 'Observed'; ctsE$srce <- 'Estimate'
# 
# ctsL <- rbind(ctsW, ctsE)
# 
# # Plot to verify
# pl <- ggplot(ctsL, aes(date, tmpC, color = srce)) + geom_line(size = 0.25) +
#   theme_classic() + facet_wrap(.~ stns, ncol = 4)
# 
# ggsave(filename = paste0(path, 'stream_temp_2004_estimated.png'), plot = pl,
#        width = 11,height = 8.5, units = 'in', dpi = 300)
# 

