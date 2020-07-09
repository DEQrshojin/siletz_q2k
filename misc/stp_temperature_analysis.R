library(ggplot2)

rm(list = ls()); cat('\014')

rivr <- wwtp <- list()

year <- c('YR06', 'YR14', 'YR15')

pth <- 'C:/siletz_tmdl/02_outputs/02_q2k/load_cap/stp'

for (i in 1 : 3) {
  
  rivr[[i]] <- read.csv(paste0(pth, 'rvr_loads_R07_', year[i], '.csv'))
  
  wwtp[[i]] <- read.csv(paste0(pth, 'stp_loads_R07_', year[i], '.csv'))
  
}

rivr <- rbind(rivr[[1]], rivr[[2]], rivr[[3]])

wwtp <- rbind(wwtp[[1]], wwtp[[2]], wwtp[[3]])

data <- data.frame(date = rivr$date,
                   Q_r  = rivr$qIn_cms,
                   Q_e  = wwtp$qIn_cms,
                   T_r  = rivr$tmp_dgC,
                   T_e  = wwtp$tmp_dgC,
                   stringsAsFactors = F)

data$Q_x <- data$Q_r + data$Q_e

data$T_x <- (data$Q_r * data$T_r + data$Q_e * data$T_e) / (data$Q_r + data$Q_e)

data$year <- year(data$date)  
  
  
  
  