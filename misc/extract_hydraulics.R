extract_hydraulics <- function(outF = NULL, strD = NULL, wudy = 10) {
  
  library(lubridate)
  
  source('D:/siletz_q2k/04_scripts/cal_functions_q2k.R')
  
  # Examine the output file and determine where to extract data
  dt <- readLines(outF)
  
  splt <- function(strg) substring(strg, seq(1, 553, 24), seq(24, 576, 24))
  
  # Format the headers
  hdrs <- as.vector(sapply(X = dt[1], FUN = splt))
  
  hdrs <- gsub(" ", "", hdrs)
  
  # Format the data
  dt <- dt[3 : length(dt)]
  
  dt <- data.frame(t(sapply(X = dt, FUN = splt)), stringsAsFactors = F)
  
  for (i in 1 : length(dt)) {dt[, i] <- as.numeric(dt[, i])}
  
  row.names(dt) <- 1 : nrow(dt); names(dt) <- hdrs; dt <- dt[, c(1, 3, 19 : 24)]
  
  if(!is.POSIXct(strD)) {
    strD <- as.POSIXct(strD, '%Y-%m-%d', tz = 'America/Los_Angeles')
  }
  
  # Remove warm-up days
  dt <- dt[which(dt$Time >= wudy), ]; dt$Time <- dt$Time - wudy

  # Time - convert from days to seconds and convert to POSIXct
  dt$Time <- as.POSIXct(dt$Time * 86400, origin = strD,
                         tz = 'America/Los_Angeles')

  # Add a leading 0 to the reaches
  dt$Reach <- paste0('R', addZ(dt$Reach))

  return(dt)
  
}