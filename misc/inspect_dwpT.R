# RH: =100*(EXP((17.625*TD)/(243.04+TD))/EXP((17.625*T)/(243.04+T)))
# TD = 243.04 * (LN(RH / 100) + ((17.625* T)/(243.04+T)))/(17.625-LN(RH/100)-((17.625*T)/(243.04+T)))
# T: =243.04*(((17.625*TD)/(243.04+TD))-LN(RH/100))/(17.625+LN(RH/100)-((17.625*TD)/(243.04+TD)))

rm(list = ls())

library(reshape2); library(ggplot2)

# ------------------------------------------------------------------------------
# HEAT SOURCE
hs <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Temperature/Heats',
                      'ource/Siletz/SIM01_CCC_2004/Final_CCC/inputs/met.csv'),
               stringsAsFactors = F)

hs$DATETIME <- as.POSIXct(hs$DATETIME, '%m/%d/%Y %H:%M', tz = 'America/Los_Angeles') 

# R02 met 2, R05 met 4, R08 met 6
cond <- which(substr(names(hs), nchar(names(hs)), nchar(names(hs))) %in% c(2, 4, 6) &
              substr(names(hs), 1, (nchar(names(hs)) - 1)) %in% c('RELATIVE_HUMIDITY', 
                                                                'AIR_TEMPERATURE'))

hs <- hs[, c(1, cond)]

names(hs) <- c('date', paste0(rep(c('RH', 'Ta'), 3), '_',
                              c(rep('R02', 2), rep('R05', 2),rep('R08', 2))))

# Calculate dewpoint temperature
hs$Td_R02 <- hs$Td_R05 <- hs$Td_R08 <- 0

for (i in 1 : 3) {
  
  hs[, i + 7] <- 243.04 * (log(hs[, 2 * i]) + ((17.625 * hs[, 2 * i + 1]) /
                                               (243.04 + hs[, 2 * i + 1]))) /
                (17.625 -  log(hs[, 2 * i]) - ((17.625 * 2 * i + 1) /
                                               (243.04 + 2 * i + 1)))
  
}

hsT <- melt(hs[, c(1, 3, 5, 7 : 10 )], id.vars = 'date', value.name = 'ToC',
            variable.name = 'var')

hsT$rch <- substr(hsT$var, 4, 6); hsT$var <- substr(hsT$var, 1, 2)

windows(12, 12)

xlim <- as.POSIXct(c('2004-07-22', '2004-08-01'), '%Y-%m-%d',
                   tz = 'America/Los_Angeles')

p <- ggplot(hsT, aes(x = date, y = ToC, group = var, color = var)) + 
     geom_line() + scale_x_datetime(limits = c(xlim[1], xlim[2])) + 
     scale_y_log10(limits = c(5, 40)) + facet_wrap(.~rch, ncol = 1); p

# ------------------------------------------------------------------------------
# QUAL2Kw
pth <- 'D:/siletz_q2k/02_input/ext_temp_0707_0829/'

q2k <- list(airT = read.csv(paste0(pth, 'air_temp_jul17_ext.csv'),
                            stringsAsFactors = F),
            dwpT = read.csv(paste0(pth, 'dwp_temp_jul17_ext.csv'),
                            stringsAsFactors = F))

# Transpose
for (i in 1 : 2) {
  
  q2k[[i]] <- data.frame(t(q2k[[i]]))
  
  q2k[[i]]$date <- row.names(q2k[[i]])
  
  row.names(q2k[[i]]) <- 1 : nrow(q2k[[i]])
  
  q2k[[i]]$date <- as.POSIXct(q2k[[i]]$date, 'X%Y.%m.%d.%H.%M.%S',
                              tz = 'America/Los_Angeles') 
  
  q2k[[i]] <- q2k[[i]][, c(11, 2, 5, 8)]

}

q2k <- data.frame(date   = q2k[[1]]$date,
                  Ta_R02 = q2k[[1]]$X2,
                  Ta_R05 = q2k[[1]]$X5,
                  Ta_R08 = q2k[[1]]$X8,
                  Td_R02 = q2k[[2]]$X2,
                  Td_R05 = q2k[[2]]$X5,
                  Td_R08 = q2k[[2]]$X8)

q2k <- melt(q2k, id.vars = 'date', value.name = 'ToC', variable.name = 'var')

q2k$rch <- substr(q2k$var, 4, 6); q2k$var <- substr(q2k$var, 1, 2)

windows(12, 12)

xlim <- as.POSIXct(c('2017-07-22', '2017-08-01'), '%Y-%m-%d',
                   tz = 'America/Los_Angeles')

p <- ggplot(q2k, aes(x = date, y = ToC, group = var, color = var)) + 
     geom_line() + scale_x_datetime(limits = c(xlim[1], xlim[2])) + 
     scale_y_log10(limits = c(5, 40)) + facet_wrap(.~rch, ncol = 1); p

