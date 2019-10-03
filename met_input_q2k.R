# LOAD FUNCTIONS, VARS & DATA ----
source('D:/siletz_q2k/04_scripts/met_functions_q2k.R')

# CHANCE DATES HERE
strD = "2017-09-10"; endD = "2017-09-15"
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
for (i in 1) {

  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')

  # column 5 for air temperature at Newport
  mDat <- x[which(x$time >= dtes[1] & x$time <= dtes[2]), c(1, 5)]
  
  tmes <- minmax_time(mDat); tmnt <- tmes$min; tmxt <- tmes$max
  
  airT <- t_air_q2k(strD = strD, endD = endD, shp = shp, dir = dirA, tmxt = tmxt,
                    tmnt = tmnt)
  
  # plot <- plot_temps(df = airT); windows(12, 12); plot
  # 
  # ggsave(filename = 'air_temp.png', plot = plot, path = 'D:/siletz_q2k/02_input',
  #        width = 15, height = 10, units = 'in', dpi = 300)

}

# DEWP TEMP SCRATCH ----
for (i in 1) {
  
  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  # column 3 for dew point at Newport and convert to celsius
  dpts <- x[which(x$time >= dtes[1] & x$time <= dtes[2]), c(1, 3)]
  
  tmes <- minmax_time(dpts); tmnt <- tmes$min; tmxt <- tmes$max
  
  dpts$tdw_nwp <- (dpts$tdw_nwp - 32) / 1.8
  
  dwpT <- t_dwpnt_q2k(dpts = dpts, strD = strD, endD = endD, shp = shp, dir = dirW,
                      tmxt = tmxt, tmnt = tmnt)

}

# CLOUD, WIND & SOLAR ----
for (i in 1) {
 
  cCov <- cloud_q2k(x = x, strD = strD, endD = endD)
  
  wndU <- wind_q2k(x = x, strD = strD, endD = endD)
  
  solr <- solar_q2k(strD = strD, endD = endD)
  
}

# SAVE FILES ----

write.csv(x = airT, file = 'D:/siletz_q2k/02_input/air_temp_sep_10_15.csv', row.names = F)
write.csv(x = dwpT, file = 'D:/siletz_q2k/02_input/dwp_temp_sep_10_15.csv', row.names = F)
write.csv(x = cCov, file = 'D:/siletz_q2k/02_input/cloud_cover_sep_10_15.csv', row.names = F)
write.csv(x = wndU, file = 'D:/siletz_q2k/02_input/wind_speed_sep_10_15.csv', row.names = F)
write.csv(x = solr, file = 'D:/siletz_q2k/02_input/solar_sep_10_15.csv', row.names = T)



