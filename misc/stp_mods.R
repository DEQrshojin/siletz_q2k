library(ggplot2); library(lubridate); library(reshape2)

rm(list = ls()); cat('\014')

# Import river data
rvrT <- readRDS("C:/siletz_tmdl/02_outputs/02_q2k/base/results_summary.RData")

rvrT <- rvrT[['daily_stats']][which(rvrT[['daily_stats']]$Reach == 6 &
                                    rvrT[['daily_stats']]$stat == 'Daily mean'), c(1, 2, 3, 5)]

# Import STP data
stp  <- read.csv("C:/siletz_tmdl/01_inputs/02_q2k/stp/slz_dmr.csv",
                 stringsAsFactors = F)

stp$DATE <- as.POSIXct(stp$DATE, '%m/%d/%Y', tz = 'America/Los_Angeles')

stp <- merge(stp, rvrT[, c(2, 4)], by.x = 'DATE', by.y = 'dte', all.x = T, all.y = F)

stp$Temp_E <- ifelse(is.na(stp$tmpC), stp$Temp_E, stp$tmpC)

pth <- 'C:/siletz_tmdl/01_inputs/02_q2k/stp/'

stp <- stp[, -15]

write.csv(x = stp, file = paste0(pth, 'stp_dmr_bg_T.csv'), row.names = F)


# GRAPH TO CHECK
# temp <- stp[which(!is.na(stp$tmpC)), c(1, 7, 15)]
# 
# names(temp) <- c('date', 'tmp_E', 'tmp_R')
# 
# temp$year <- year(temp$date)
# 
# temp$doy <- yday(temp$date)
# 
# temp$diff <- temp$tmpE - temp$tmp_R
# 
# temp <- melt(temp, id.vars = c('date', 'year', 'doy'), value.name = 'T_C',
#              variable.name = 'SRCE')
# 
# pl <- ggplot(temp, aes(x = doy, y = T_C, color = SRCE)) + geom_line() +
#       facet_wrap(.~ year) + theme_classic()
# 
# ggsave(filename = 'C:/siletz_tmdl/01_inputs/02_q2k/stp/river_stp_temp_comparison.png',
#        pl, width = 11, height = 8.5, dpi = 300)
# 
# p2 <- ggplot(temp, aes(x = doy, y = diff)) + geom_col() + facet_wrap(.~ year) + theme_classic()
# 
# ggsave(filename = 'C:/siletz_tmdl/01_inputs/02_q2k/stp/river_stp_temp_difference.png',
#        p2, width = 11, height = 8.5, dpi = 300)

