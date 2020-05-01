rm(list = ls()); cat('\014')

#2345678901234567890123456789012345678901234567890123456789012345678901234567890
load_inflow_BCs <- function(dir = NULL, strD = NULL, wudy = NULL, scen = NULL) {
  
  library(lubridate)
  
  addZ <- function(v) ifelse(v < 10, paste0(0, v), as.character(v))
  
  if (substr(dir, nchar(dir), nchar(dir)) != '/') {dir <- paste0(dir, '/')}
    
  ifls <- paste0(dir, 'bc_', c('infw', 'hdwr'), '/', scen, '_', c('infw', 'hdwr'),
                 '.csv')
  
  datI <- read.csv(ifls[1], stringsAsFactors = F) # Load lateral inflow data

  datH <- read.csv(ifls[2], stringsAsFactors = F) # Load headwater data
  
  # Process headwater data
  datH <- data.frame(t(datH[1 : 18, ]), stringsAsFactors = F)
  
  # Make the first row into headers
  names(datH) <- datH[1, ]; datH <- datH[-1, ]
  
  # Deal with reach names:
  datH$Reach <- 'HW'; datI$Reach <- paste0('R', addZ(datI$Reach))

  # Convert lateral BCs date to posix
  datI$date <- as.POSIXct(datI$date, '%Y-%m-%d %H:%M:%S', tz = 'America/Los_Angeles')

  # Add dates to the HW df
  datH$date <- seq(min(datI$date), max(datI$date), 3600)

  # Deal with dates
  if (!is.POSIXct(strD)) strD <- as.POSIXct(strD, '%Y-%m-%d',
                                            tz = 'America/Los_Angeles')

  # Rearrange hewadwater DF columns
  datH <- datH[, c(19, 20, 1 : 18)]
  
  # Convert wq columns of headwaters to numeric
  for (i in 3 : length(datH)) datH[, i] <- as.numeric(datH[, i])
  
  # Need to remove diversions from lateral inflow BCs
  datI <- datI[, -3]

  # Combine the dataframes, headwaters first, and return
  data <- rbind(datH, datI)
  
  # Remove warm-up days
  strD <- strD + days(wudy)
  
  data <- data[which(data$date >= strD), ]
  
  return(data)

}
#2345678901234567890123456789012345678901234567890123456789012345678901234567890

library(reshape2); library(ggplot2)

scen <- 'test'; strD <- '2017-07-01'

# Graph lateral catchment inflows
for (n in 1 : length(scen)) {

  #45678901234567890123456789012345678901234567890123456789012345678901234567890
  # Load data
  df <- load_inflow_BCs(dir = 'C:/siletz_tmdl/01_inputs/02_q2k',  strD = strD,
                          wudy = 0, scen = scen)
  
  # Cull unnecessary columns
  #            R  D  |-----SET 1------||------SET 2------|
  df <- df[, c(1, 2, 3,  4,  7, 20, 16, 12, 11, 10, 14, 13)]

  # Add in a row with NAs
  addR <- df[which(df$date == min(df$date)), ]
  
  year <- year(unique((addR$date)))
  
  addR$date <- as.POSIXct(paste0(year, '-09-01'), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  addR[, c(3 : length(addR))] <- NA
  
  df <- rbind(df, addR)
  
  # Melt to long
  df <- melt(df, id.vars = c('Reach', 'date'), value.name = 'val',
             variable.name = 'par')
  
  sets <- list(set1 = c('qIn_cms', 'tmp_dgC',  'do_mgL',      'pH', 'oss_mgL'),
               set2 = c('nox_ugL', 'nh3_ugL', 'orn_ugL', 'po4_ugL', 'orp_ugL'))
  
  df[which(df$par %in% sets[['set2']]), 4] <- df[which(df$par %in% sets[['set2']]), 4] / 1000
  
  hdrs <- data.frame(abbr = unique(df$par),
                     full = c('(A) Flow (cms)', '(B) Temp (oC)', '(C) DO (mg/L)',
                              '(D) pH (su)', '(E) Organic C (mg/L)', '(A) NO3 (mg/L)',
                              '(B) NH3 (mg/L)', '(C) Ogranic N (mg/L)', '(D) PO4 (mg/L)',
                              '(E) Organic P (mg/L)'))
  
  # Merge the df and the full parameter names (and units)
  df <- merge(df, hdrs, by.x = 'par', by.y = 'abbr', all = T)
  
  # Path for saving figure
  pth <- 'C:/siletz_tmdl/01_inputs/02_q2k/bc_figures'
  
  nmes <- paste0(scen[n], c('_inflow_bcs_I.png', '_inflow_bcs_II.png'))
  
  dtes <- as.POSIXct(c('2017-09-15', '2017-09-21'), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  df <- df[which(df$date >= dtes[1] & df$date <= dtes[2]), ]
  
  for (i in 1 : 2) {
    
    temp <- df[which(df$par %in% sets[[i]]), ]
    
    pl <- ggplot(data = temp, aes(x = date, y = val, color = Reach)) +
          geom_line() + xlab(NULL) + ylab(NULL) + theme_bw() +
          scale_x_datetime(breaks = "2 days", date_labels = '%m-%d') + 
          scale_y_continuous(breaks = scales::pretty_breaks(7), limits = c(0, NA)) +
          facet_wrap(.~ full, strip.position = "left", ncol = 1, scales = 'free') +
          theme(strip.background = element_blank(), strip.placement = "outside",
                legend.position = 'bottom', legend.title = element_blank(),
                legend.text  = element_text(size = 11),
                strip.text   = element_text(size = 13),
                axis.text.y  = element_text(size = 11),
                axis.text.x  = element_text(size = 11),
                axis.title.y = element_text(size = 13),
                axis.title.x = element_text(size = 13))
    
    ggsave(filename = paste0(pth, '/', nmes[i]), plot = pl, width = 8.5,
           height = 11, dpi = 300, units = "in")
    
  }
}


