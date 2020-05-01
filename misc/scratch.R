library(lubridate); library(ggplot2)

rm(list = ls()); cat('\014')

addZ <- function(z = NULL, ndgt = 1) {
  
  x <- z # Create a duplicate (pristine) vector for the comparisons
  
  # CHECK
  if (max(z) > 10^(ndgt)) {
    
    cat('ERROR: Max vector element > number of digits: ', 10^(ndgt)); stop()
    
  }
  
  # z is vector to add to, ndgt is the number of full string (e.g., 4 = '000X')
  for (i in 1 : ndgt) {z <- ifelse(x < 10^i, paste0(0, z), z)}
  
  return(z)
  
}

source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/proc_results.R')

mOut <- 'C:/siletz_tmdl/02_outputs/02_q2k/test3'

hdrs <- read_headers(mOut)

data <- read_q2k_out_2(mOut = mOut, pars = unlist(hdrs$parm), strD = '2017-07-01')

data <- data[-which(data$Reach == 0), -c(2, 4, 6)] # 

# Time - convert from days to seconds and convert to POSIXct
data$Time <- as.POSIXct(data$Time * 86400, origin = '2017-07-01',
                        tz = 'America/Los_Angeles') + hours(7)

for (i in 4 : length(data)) {
  
  labl <- paste0(hdrs$parm[i], ' ', hdrs$unit[i])
  
  pl <- ggplot(data, aes(x = Time, y = data[, i])) + geom_line() +
        theme_classic() + ylab(labl) + facet_wrap(.~ Reach, ncol = 2) + 
        theme(axis.title.x = element_blank())
  
  fils <- paste0(mOut, addZ(z = i, ndgt = ceiling(log10(nrow(hdrs)))), '_',
                 hdrs$parm[i], '.png')
  
  ggsave(filename = fils, plot = pl, width = 11, height = 8.5, units = 'in',
         dpi = 300)

}