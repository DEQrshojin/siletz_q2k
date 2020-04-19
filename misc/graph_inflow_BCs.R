rm(list = ls()); cat('\014')

#2345678901234567890123456789012345678901234567890123456789012345678901234567890
load_inflow_BCs <- function(dir = NULL, seas = NULL, strD = NULL, wudy = NULL,
                            scen = NULL) {
  
  library(lubridate)
  
  addZ <- function(v) ifelse(v < 10, paste0(0, v), as.character(v))
  
  if (substr(dir, nchar(dir), nchar(dir)) != '/') {dir <- paste0(dir, '/')}
    
  ifls <- paste0(dir, 'bc_', c('infw', 'hdwr'), '/', scen, '_',
                 c('infw', 'hdwr'), '_', seas, '.csv')
  
  datI <- read.csv(ifls[1], stringsAsFactors = F) # Load headwater data

  datH <- read.csv(ifls[2], stringsAsFactors = F) # Load lateral inflow data
  
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

# scen <- c(  'scen_004',   'scen_005',   'scen_006',   'scen_009',   'scen_013',
#             'scen_014',   'scen_018',   'scen_019',   'scen_020')
# 
# strC <- c('2017-07-07', '2017-07-07', '2014-07-07', '2015-07-07', '2014-07-07',
#           '2015-07-07', '2005-07-07', '2007-07-07', '2012-07-07')
# 
# strS <- c('2017-09-01', '2017-09-01', '2014-09-01', '2015-09-01', '2014-09-01',
#           '2014-09-01', '2005-09-01', '2007-09-01', '2012-09-01')

scen <- 'scen_022'; strC <- '2004-07-07'; strS <- '2004-09-01'; n = 1

# Graph lateral catchment inflows
for (n in 1 : length(scen)) {

  #45678901234567890123456789012345678901234567890123456789012345678901234567890
  # Load data
  cw <- load_inflow_BCs(dir = 'C:/siletz_tmdl/01_inputs/02_q2k', seas = 'cw', 
                        strD = strC[n], wudy = 4, scen = scen[n])
  
  sp <- load_inflow_BCs(dir = 'C:/siletz_tmdl/01_inputs/02_q2k', seas = 'sp', 
                        strD = strS[n], wudy = 7, scen = scen[n])

  # sp$date <- sp$date + years(1)
  
  cw$seas <- 'CW'; sp$seas <- 'SP'
  
  df <- rbind(cw, sp)
  
  # Cull columns
  #            R  D   S   |-----SET 1------||------SET 2------|
  df <- df[, c(1, 2, 21,  3,  4,  7, 20, 16, 12, 11, 10, 14, 13)]
  
  # cw <- cw[, c(1, 2, 5)]; cw <- cw[which(cw$date == min(cw$date)), ]
  # sp <- sp[, c(1, 2, 5)]; sp <- sp[which(sp$date == min(sp$date)), ]
  
  # FOR ONLY STPS REMOVE ALL REACHES EXCEPT REACH 07
  # df <- df[which(df$Reach == 'R07'), ]
  
  # Add in a row with NAs
  addR <- df[which(df$date == min(df$date)), ]
  
  year <- year(unique((addR$date)))
  
  addR$date <- as.POSIXct(paste0(year, '-09-01'), '%Y-%m-%d',
                          tz = 'America/Los_Angeles')
  
  addR[, c(4 : length(addR))] <- NA
  
  df <- rbind(df, addR)
  
  # Melt to long
  df <- melt(df, id.vars = c('Reach', 'seas', 'date'), value.name = 'val',
             variable.name = 'par')
  
  sets <- list(set1 = c('qIn_cms', 'tmp_dgC',  'do_mgL',      'pH', 'oss_mgL'),
               set2 = c('nox_ugL', 'nh3_ugL', 'orn_ugL', 'po4_ugL', 'orp_ugL'))
  
  df[which(df$par %in% sets[['set2']]), 5] <- df[which(df$par %in% sets[['set2']]), 5] / 1000
  
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
  
  for (i in 1 : 2) {
    
    temp <- df[which(df$par %in% sets[[i]]), ]
    
    pl <- ggplot(data = temp, aes(x = date, y = val, color = Reach)) +
          geom_line() + xlab(NULL) + ylab(NULL) + theme_bw() +
          scale_x_datetime(breaks = "2 weeks") + 
          scale_y_continuous(breaks = scales::pretty_breaks(7), limits = c(0, NA)) +
          facet_wrap(.~ full, strip.position = "left", ncol = 1, scales = 'free') +
          # guides(col = guide_legend(ncol = 5)) +
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


