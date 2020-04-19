rm(list = ls()); cat('\014')

# LOAD FUNCTIONS, VARS & DATA ----
source('D:/siletz_q2k/04_scripts/met_functions_q2k.R')

# CHANCE DATES HERE
# strD <- '2017-07-07'; endD <- '2017-08-29'
# strD <- '2017-09-08'; endD <- '2017-10-16'
strD <- '2004-01-01'; endD <- '2018-01-01'
# CHANCE DATES HERE

for (i in 1) {
  
  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  pth <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle_S',
                'iletz_River_1710020405/001_data/met_data/')
  
  x <- readRDS(file = paste0(pth, 'met_data_4_Q2Kw.RData'))
  
  shp = paste0('C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/GIS/001',
               '_data/shape/catchment_centroids_HSPF_wgs84.shp')
  
  dirA = 'C:/Users/rshojin/Desktop/002_gis/003_climate/prism/'
  
  dirW = 'C:/Users/rshojin/Desktop/002_gis/003_climate/prism/tdmean/'

}

# AIR TEMP SCRATCH ----
# for (i in 1) {
# 
  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')

  # column 5 for air temperature at Newport
  mDat <- x[which(x$time >= dtes[1] & x$time <= dtes[2]), c(1, 5)]

  tmes <- minmax_time(mDat); tmnt <- tmes$min; tmxt <- tmes$max

  airT <- t_air_q2k(strD = strD, endD = endD, shp = shp, dir = dirA, tmxt = tmxt,
                    tmnt = tmnt, nday = 7)
  
# 
#   # plot <- plot_temps(df = airT); windows(12, 12); plot
#   # 
#   # ggsave(filename = 'air_temp_initC.png', plot = plot, path = 'D:/siletz_q2k/02_input',
#   #        width = 15, height = 10, units = 'in', dpi = 300)
# 
# }
# 
# # DEWP TEMP SCRATCH ----
# for (i in 1) {

  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')

  # column 3 for dew point at Newport and convert to celsius
  dpts <- x[which(x$time >= dtes[1] & x$time <= dtes[2]), c(1, 3)]

  tmes <- minmax_time(dpts)
  
  tmes$min <- ifelse(is.na(tmes$min), tmes$date + hours(5), tmes$min)
  
  tmes$max <- ifelse(is.na(tmes$max), tmes$date + hours(17), tmes$max)
  
  tmes$min <- as.POSIXct(tmes$min, origin = '1970-01-01', tz = 'America/Los_Angeles')
  
  tmes$max <- as.POSIXct(tmes$max, origin = '1970-01-01', tz = 'America/Los_Angeles')
  
  tmnt <- tmes$min; tmxt <- tmes$max

  dpts$tdw_nwp <- (dpts$tdw_nwp - 32) / 1.8
  
  airT <- readRDS('D:/siletz_TMDL/01_inputs/02_q2k/RData/T_dwp_2004_2017.RData')

  dwpT <- suppressMessages(t_dwpnt_q2k(dpts = dpts, strD = strD, endD = endD,
                                       shp = shp, dir = dirW, tmxt = tmxt,
                                       tmnt = tmnt, airT = airT, nday = 7))

# }

# CLOUD, WIND & SOLAR ----
for (i in 1) {

  cCov <- cloud_q2k(x = x, strD = strD, endD = endD, nday = NULL)

  # mdfy has to have same length as the df. Smaller number means less cloudy
  cCov <- modify_met(df = cCov, strD = '2017-08-05', endD = '2017-08-14',
                     mdfy = c(20, 20, 70, 70, rep(90, 6)))
  
  # wndU <- wind_q2k(x = x, strD = strD, endD = endD,
  #                  mdfr = c(0, 5, 10, 10, 25, 25, 25, 25, 35, 35))

  # solr <- solar_q2k(strD = strD, endD = endD, nday = 7)
  
}

# SAVE FILES ----
path <- 'D:/siletz_q2k/02_input/wq_cw_11_noWU/'; sffx <- 'wq_cw_11_noWU'

# write.csv(x = airT, file = paste0(path, 'air_temp_', sffx, '.csv'), row.names = F)
# write.csv(x = dwpT, file = paste0(path, 'dwp_temp_', sffx, '.csv'), row.names = F)
write.csv(x = cCov, file = paste0(path, 'cld_covr_MDFY_', sffx, '.csv'), row.names = F)
# write.csv(x = wndU, file = paste0(path, 'wind_spd_', sffx, '.csv'), row.names = F)
# write.csv(x = solr, file = paste0(path, 'solr_rad_', sffx, '.csv'), row.names = T)



