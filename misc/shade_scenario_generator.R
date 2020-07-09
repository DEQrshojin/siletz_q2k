
# Scenario 4 (S017) defines the lower shade limits for the shade scenarios
# Scenario 5 (S021) defines the upper shade limits
# Gradually increase shade from Scenario 4 by increments of XXX percent until
# No appreciable shade increses occur

rm(list = ls()); cat('\014')

source("C:/siletz_tmdl/04_scripts/02_q2k/02_R/cal_functions_q2k.R")

scen <- c('S017', 'S021'); dt <- list()

for (i in 1 : 2) {

  # Read in the data 
  dt[[i]] <- read.csv(paste0('C:/siletz_tmdl/01_inputs/02_q2k/mt_eShd/', scen[i],
                             '/eShd_YR04.csv'), stringsAsFactors = F)
  
  # Transpose
  dt[[i]] <- data.frame(t(dt[[i]]), stringsAsFactors = F)
  
  # Dates
  dt[[i]]$date <- as.POSIXct(row.names(dt[[i]]), 'X%Y.%m.%d.%H.%M.%S',
                             tz = 'America/Los_Angeles')
  
  row.names(dt[[i]]) <- 1 : nrow(dt[[i]])
  
}

names(dt) <- scen

# Add another day (last day in Oct) to S017 because it doesn't have additional day
dt[[1]] <- rbind(dt[[1]], dt[[1]][(nrow(dt[[1]]) - 23) : nrow(dt[[1]]), ])

dt[[1]]$date <- dt[[2]]$date

# Establish baseline conditions on the difference between CC and SPV shade
base <- chng <- sum(colSums(dt[[2]][, 1 : 10] - dt[[1]][, 1 : 10]))

# Increase shade above S017 by increment
incr <- 5 # In percentage

cntr <- 3

while (chng != 0) {
  
  dt[[cntr]] <- dt[[1]]
  
  for (i in 1 : 10) {
    
    dt[[cntr]][, i] <- ifelse(dt[[1]][, i] + incr * (cntr - 2) / 100 < dt[[2]][, i],
                              dt[[1]][, i] + incr * (cntr - 2) / 100,  dt[[2]][, i])
    
  }
  
  chng <- sum(colSums(dt[[2]][, 1 : 10] - dt[[cntr]][, 1 : 10]))
  
  names(dt)[cntr] <- paste0('p', addZ(incr * (cntr - 2)))
  
  cntr <- cntr + 1

}

# Take the middle iterations (3 : cntr - 2)
for (i in 3 : (length(dt) - 1)) {
  
  # Row names from the date column; and remove date column
  row.names(dt[[i]]) <- dt[[i]]$date; dt[[i]] <- dt[[i]][, -11]
  
  # Transpose
  dt[[i]] <- t(dt[[i]])

  scen <- paste0('S0', 19 + i)
  
  dir <- paste0('C:/siletz_tmdl/01_inputs/02_q2k/mt_eShd/', scen)
  
  dir.create(paste0('C:/siletz_tmdl/01_inputs/02_q2k/mt_eShd/', scen))

  fils <- c(paste0('C:/siletz_tmdl/01_inputs/02_q2k/mt_eShd/', scen, '/eShd_YR',
                   addZ(4 : 17), '.csv'))
  
  for (j in 1 : length(fils)) {write.csv(dt[[i]], fils[j], row.names = F)}
  
}

pars <- c('CC', 'TA', 'TD', 'WND')

scnO <- 'S017'

# Copy the other met conditions for the scenarios just generated
for (i in 3 : (length(dt) - 1)) {
  copy_inputs(inpt = pars, scnN = paste0('S0', 19 + i), scnO = scnO)
}















