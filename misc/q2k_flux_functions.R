compare_heat_budgets <- function(it = NULL) {
  
  suppressMessages(library(lubridate)); suppressMessages(library(reshape2))
  suppressMessages(library(ggplot2)); suppressMessages(library(scales))
  suppressMessages(library(grid)); suppressMessages(library(cowplot))
  suppressMessages(library(ggpubr)); suppressMessages(library(gridExtra))
  
  source('D:/siletz_q2k/04_scripts/cal_functions_q2k.R')
  
  outF <- paste0('D:/siletz_q2k/08_pest/02_temp/out/out_it', addZ(it),
                 c('_20170722.rms', '_20170801.rms'))
  
  # Look at reaches 02, 05, and 08 on 7/22 (good match) and 8/1 (bad match)
  q <- list('d_0722' = extract_flux(outF[1]), 'd_0801' = extract_flux(outF[2]))
  
  # Add the stream reach information
  q[[1]]$rch <- q[[2]]$rch <- 0
  
  for (i in 0 : 10) {
    
    int <- (1 : 513) + i * 513
    
    q[[1]][int, 12] <- q[[2]][int, 12] <- paste0("R", addZ(i))
    
  }
  
  org <- c('2017-07-22', '2017-08-01')
  
  # Add the stream temperatures
  for (i in 1 : 2) {
    
    t <- extract_stream_temp(outF[i]); q[[i]]$tmpC <- t$tmpC
    
    # Isolate reaches 2, 5 and 8
    q[[i]] <- q[[i]][which(q[[i]]$rch == 'R02' | q[[i]]$rch == 'R05' |
                             q[[i]]$rch == 'R08'), ]
    
    # Set the dates then combine the data frames  
    q[[i]]$hr <- as.POSIXct(q[[i]]$hr * 3600, origin = org[i],
                            tz = 'America/Los_Angeles')  + hours(7)    
    
  }
  
  qDat <- rbind(q[[1]], q[[2]])
  
  # Extract the date
  qDat$date <- floor_date(qDat$hr, 'days')
  
  # Combine downwell/upwell longwave
  qDat$net_LW <- qDat$longat + qDat$back
  
  qDat <- qDat[, c(12, 14, 1, 2, 15, 5 : 7, 13)]
  
  names(qDat) <- c('rch', 'date', 'time', 'SR', 'LW', 'CV', 'EV', 'CN', 'TC')
  
  rmDt <- as.POSIXct(c('2017-07-23', '2017-08-02'), '%Y-%m-%d',
                     tz = 'America/Los_Angeles')
  
  # Remove last obs (hour 24) of the next day
  qDat <- qDat[-which(qDat$date %in% rmDt), ]
  
  # Pull in the Heat Source data
  dir1 <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Temperature/Heatsour',
                 'ce/Siletz/SIM01_CCC_2004/Final_CCC/outputs/')
  
  hFil <- c('Heat_SR5.csv', 'Heat_Long.csv', 'Heat_Conv.csv', 'Heat_Evap.csv', 
            'Heat_Cond.csv')
  
  pars <- c('solr', 'lngw', 'conv', 'evap', 'cond')
  
  # Node stream kilometers for comparison
  rch <- c(56.9, 39.8, 21.3)
  
  d <- list()
  
  # Load the Heat Source heat data and process into a list of DFs
  for (i in 1 : length(hFil)) {
    
    d[[i]] <- read.table(file = paste0(dir1, hFil[i]), skip = 6,
                         stringsAsFactors = F)
    
    nmes <- d[[i]][1, ]; d[[i]] <- d[[i]][-1, ]; names(d)[i] <- pars[i]
    
    d[[i]] <- do.call('rbind', strsplit(x = d[[i]], split = ','))
    
    # Deal with the names: only need nodes from 75.0 to 2.7
    nmes <- strsplit(x = nmes, split = ',')[[1]]
    
    ndes <- as.numeric(nmes[2 : length(nmes)])
    
    d[[i]] <- data.frame(apply(X = d[[i]], MARGIN = 2, FUN = as.numeric),
                         stringsAsFactors = F)
    
    # Cull nodes
    d[[i]] <- d[[i]][, c(1, which(ndes %in% rch) + 1)]
    
    names(d[[i]]) <- c('dttm', paste0('R0', c(2, 5, 8)))
    
    # Coerce dates
    d[[i]]$dttm <- as.POSIXct((d[[i]]$dttm - (1 + 17/24)) * 86400,
                              origin = '1900-01-01',
                              tz = 'America/Los_Angeles')
    
    d[[i]]$dttm <- round_date(d[[i]]$dttm, 'hour')
    
    # Melt to long
    d[[i]] <- melt(d[[i]], id.vars = 'dttm', variable.name = 'var',
                   value.name = 'flx')
    
    if (i == 1) {d[[i]]$date <- floor_date(d[[i]]$dttm, 'days')}
    
  }
  
  hDat <- data.frame(rch  = d[[1]]$var,
                     date = d[[1]]$date,
                     time = d[[1]]$dttm,
                     SR   = d[[1]]$flx,
                     LW   = d[[2]]$flx,
                     CV   = d[[3]]$flx,
                     EV   = d[[4]]$flx,
                     CN   = d[[5]]$flx,
                     TC   = NA, stringsAsFactors = F)
  
  dtes <- unique(qDat$date) - years(x = 13)
  
  # Pull out the same days of the year (for similarity of time of year/solar)
  hDat <- hDat[which(hDat$date %in% dtes), ]
  
  # Now add the years back to make concurrent with the Q2K data
  hDat$date <- hDat$date + years(13); hDat$time <- hDat$time + years(13)
  
  dtes <- dtes + years(13)
  
  # Add a column for model identifier
  qDat$src <- 'q2k'; hDat$src <- 'HS'
  
  dt <- rbind(qDat, hDat)
  
  dt$TT <- rowSums(dt[, 4 : 8])
  
  # Cast to long
  dt <- melt(dt, id.vars = c('rch', 'date', 'time', 'src'), value.name = 'ht_wm2',
             variable.name = 'comp')
  
  rchs <- unique(dt$rch)
  
  for (i in 1 : 3) {   # Cycle through reaches
    
    p <- t <- list()
    
    temp <- dt[which(dt$rch == rchs[i]), ]
    
    for (j in 1 : 2) { # Cycle through dates; will be spliced together
      
      p[[j]] <- ggplot(temp[which(temp$date == dtes[j] & temp$comp != 'TC'), ],
                       aes(x = time, y = ht_wm2, color = src)) +
                geom_line(size = 1.2) + theme_bw() +
                ylab('Heat In/Out (W/m2)') +
                theme(axis.title.x = element_blank(),
                      legend.position = c(0.9, 0.25),
                      legend.key = element_rect(colour = "transparent")) +
                scale_y_continuous(limits = c(-1000, 1000),
                                   breaks = seq(-1000, 1000, 250)) +
                scale_x_datetime(date_breaks = '3 hours',
                                 date_labels = "%H:%M") +
                facet_wrap(. ~comp, ncol = 1)
      
      t[[j]] <- ggplot(temp[which(temp$date == dtes[j] & temp$comp == 'TC'), ],
                       aes(x = time, y = ht_wm2, color = src)) +
                geom_line(size = 1.2) + theme_bw() +
                ylab('Temp (oC)') +
                theme(axis.title.x = element_blank(), legend.position = 'none',
                      axis.title.y = element_text(margin = margin(r = 16))) +
                scale_x_datetime(date_breaks = '3 hours', date_labels = "%H:%M") +
                facet_wrap(. ~comp, ncol = 1)
      
    }
    
    layout <- rbind(c(1, 2),
                    c(1, 2),
                    c(1, 2),
                    c(1, 2),
                    c(1, 2),
                    c(1, 2),
                    c(3, 4))
    
    # Splice together
    plot <- grid.arrange(p[[1]], p[[2]], t[[1]], t[[2]], layout_matrix = layout,
                         ncol = 2, nrow = 7)
    
    # Save
    ggsave(filename = paste0(rchs[i], '_heat_budget_it', addZ(it), '.png'),
           plot = plot, path = 'D:/siletz_q2k/06_figures/heat_budget',
           width = 17, height = 11, dpi = 300, units = "in")
    
  }
  
  print('Your graphs are completed.')
  
}

rename_out <- function(outF = NULL, date = NULL) {
  
  # Copy the file and rename to the date formatted as 'out_YMD.txt'
  filN <- paste0(dirname(outF), '/out_', format(date, '%Y%m%d'), '.rms')

  file.copy(from = outF, to = filN, overwrite = T)
  
}

extract_flux <- function(outF = NULL) {
  
  # Examine the output file and determine where to extract data
  dt <- readLines(outF)
  
  str <- grep('Diel fluxes of heat', dt)
  
  end <- grep('Temperature summary \\(hyporheic pore water temperature\\)', dt)
  
  splt <- function(strg) substring(strg, seq(1, 241, 24), seq(24, 264, 24))
  
  # Format the headers - spaced delimited text file at 24 character intervals
  # and we only need the first 264 characters
  hdrs <- dt[(str + 2)]
  
  hdrs <- as.vector(sapply(X = hdrs, FUN = splt))
  
  hdrs <- gsub(" ", "", hdrs)
  
  # Format the data
  dt <- dt[(str + 4) : (end - 2)]

  dt <- data.frame(t(sapply(X = dt, FUN = splt)), stringsAsFactors = F)
  
  for (i in 1 : length(dt)) {dt[, i] <- as.numeric(dt[, i])}
  
  row.names(dt) <- 1 : nrow(dt); names(dt) <- hdrs
  
  return(dt)

}

extract_stream_temp <- function(outF = NULL) {
  
  # Examine the output file and determine where to extract data
  dt <- readLines(outF)
  
  str <- grep('Diel water quality in the main channel \\(part 1 of 2\\)', dt)
  
  end <- grep('Diel water quality in the main channel \\(part 2 of 2\\)', dt)
  
  splt <- function(strg) substring(strg, seq(1, 241, 24), seq(24, 264, 24))
  
  # Format the headers - spaced delimited text file at 24 character intervals
  # and we only need the first 264 characters
  hdrs <- dt[(str + 2)]
  
  hdrs <- as.vector(sapply(X = hdrs, FUN = splt))
  
  hdrs <- gsub(" ", "", hdrs)
  
  # Format the data
  dt <- dt[(str + 4) : (end - 2)]
  
  dt <- data.frame(t(sapply(X = dt, FUN = splt)), stringsAsFactors = F)
  
  for (i in 1 : length(dt)) {dt[, i] <- as.numeric(dt[, i])}
  
  row.names(dt) <- 1 : nrow(dt); dt <- dt[, c(1, 3, 4)]
  
  names(dt) <- c('rch', 'hrs', 'tmpC')
  
  return(dt)
  
}

extract_date <- function(q2kF = NULL) {
  
  q2kD <- readLines(q2kF)
  
  # Find the date on the second line
  date <- substr(q2kD[2],
                 regexpr('Siletz River \\(', q2kD[2]) + 14,
                 regexpr('2017\\)', q2kD[2]) + 3)
  
  date <- as.POSIXct(date, '%m/%d/%Y', tz = 'America/Los_Angeles')
  
  return(date)
  
}

write_q2k_date <- function(q2kF = NULL) {
  
  suppressMessages(library(lubridate))
  
  q2kF <- file(q2kF)
  
  q2kD <- readLines(q2kF)
  
  # Find the date on the second line
  date <- substr(q2kD[2],
                 regexpr('Siletz River \\(', q2kD[2]) + 14,
                 regexpr('2017\\)', q2kD[2]) + 3)
  
  date <- as.POSIXct(date, '%m/%d/%Y', tz = 'America/Los_Angeles')
  
  # Change the output date and rewrite the list
  date <- date + days(1)
  
  q2kD[2] <- paste0(substr(q2kD[2], 1, regexpr('Siletz River \\(', q2kD[2]) + 13),
                    format(date, '%m/%d/%Y'),
                    substr(q2kD[2], regexpr('2017\\)', q2kD[2]) + 4, nchar(q2kD[2])))
  
  # Save the Q2K file
  writeLines(as.character(q2kD), q2kF)
  
  close(q2kF)  
  
}

change_output_names <- function(date = NULL, m = NULL) {
  
  library(lubridate)
  
  source('D:/siletz_q2k/04_scripts/cal_functions_q2k.R')
  
  dir <- 'D:/siletz_q2k/08_pest/02_temp/'
  
  if (!is.POSIXct(date)) {
    date <- as.POSIXct(date, '%Y-%m-%d', tz = 'America/Los_Angeles')
  }
  
  # Move the file
  file.copy(paste0(dir, 'slz_q2k_T.out'),
            paste0(dir, 'out/out_it', addZ(m), '_', format(date, '%Y%m%d'), '.rms'))
  
  file.remove(paste0(dir, 'slz_q2k_T.out'))

}