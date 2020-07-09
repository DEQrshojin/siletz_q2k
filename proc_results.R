# Summarize DO excursions
DO_summary <- function(data = NULL) {

  # Reorganize the data
  cw <- data.frame(data$cw_DO_30DM_C[, c(1, 2, 4, 5)],
                   data$cw_DO_30DM_S[, c(4, 5)],
                    data$cw_DO_7DI_C[, c(4, 5)],
                   data$cw_DO_ABSM_C[, c(4, 5)],
                     data$cw_T_7DADM[, c(4, 5)])
  
  sp <- data.frame(data$sp_DO_7D_C[, c(1, 2, 4, 5)],
                   data$sp_DO_7D_S[, c(4, 5)],
                   data$sp_T_7DADM[, c(4, 5)])
  
  names(cw)[3 : length(cw)] <- c('30DC_V', '30DC_F', '30DS_V', '30DS_F',
                                 '7DIC_V', '7DIC_F', 'ABSM_V', 'ABSM_F',
                                 'T7DADM_V', 'T7DADM_F')
  
  names(sp)[3 : length(sp)] <- c('7DC_V', '7DC_F', '7DS_V', '7DS_F',
                                 'T7DADM_V', 'T7DADM_F')
  
  # Identify excursions
  cw$`30D_X` <- ifelse(cw$`30DC_V` < 8 & cw$`30DS_V` < 90, 1, 0)
  
  cw$`7DI_X` <- ifelse(cw$`7DIC_V` < 6.5, 1, 0)
  
  cw$`ABSM_X` <- ifelse(cw$`ABSM_V` < 6.0, 1, 0)
  
  cw$T7DADM_X <- ifelse(cw$T7DADM_V > 16.0, 1, 0)
  
  sp$`7D_X` <- ifelse(sp$`7DC_V` < 11 & sp$`7DS_V` < 95, 1, 0)
  
  sp$T7DADM_X <- ifelse(sp$T7DADM_V > 13.0, 1, 0)
  
  cwDO <- cw[which(cw$`30D_X` == 1 | cw$`7DI_X` == 1 | cw$`ABSM_X` == 1), ]
  
  cw_T <- cw[which(cw$T7DADM_X == 1), ]
  
  spDO <- sp[which(sp$`7D_X` == 1), ]
  
  sp_T <- sp[which(sp$T7DADM_X == 1), ]
  
  data[['excr']] <- list(cw_DO_X = cwDO,
                         cw_T_X = cw_T,
                         sp_DO_X = spDO,
                         sp_T_X = sp_T)
  
  return(data)
  
}

# Function that serves as wrapper to analyzing DO and temperature data
DO_analysis <- function(scen = NULL, wudy = NULL, strD = NULL) {
  
  fils <- paste0('C:/siletz_tmdl/02_outputs/02_q2k/', scen)
  
  pars <- c('Temp', 'DissO2', 'DOsat')
  
  mOut <- read_q2k_out_2(mOut = fils, pars = pars, strD = strD, wudy = wudy)

  names(mOut) <- c('rch', 'dst', 'tme', 'tmp', 'doc', 'sdo')
  
  # Remove reach 0 (HW) -- trivial
  mOut <- mOut[-which(mOut$rch == 0), ]
  
  # Calculate DO saturation (%) and capped DO sat (%)
  mOut <- calc_DO_sat(df = mOut)
  
  # Return the date (without hour) of each timestep
  mOut$dte <- floor_date(mOut$tme, 'day')
  
  # Read in result constituent specs (pars, criteria, stat base, and graphing)
  cnst <- read.csv('C:/siletz_tmdl/02_outputs/02_q2k/graph_specs_IV.csv',
                   stringsAsFactors = F)
  
  # Convert the constituent dates to POSIX
  for (i in 8 : 10) {
    cnst[, i] <- as.POSIXct(cnst[, i], '%m/%d/%Y', tz = 'America/Los_Angeles')
  }
  
  # Calculate daily min/mean/max for each constituent
  dOut <- mOut %>% group_by(rch, dte) %>%
          summarize(tmpI = min(tmp), tmpN = mean(tmp), tmpX = max(tmp),
                    docI = min(doc), docN = mean(doc), docX = max(doc),
                    dosI = min(dos), dosN = mean(dos), dosX = max(dos),
                    dopI = min(dos_cap), dopN = mean(dos_cap),
                    dopX = max(dos_cap))
        
  # Calculate statistical base for comparisons (e.g., 7DADM)
  sOut <- list()
  
  # Cutoff date for return of statistical information
  coDt <- as.POSIXct(paste0(year(mOut$dte[1]), '-09-01'), '%Y-%m-%d',
                     tz = 'America/Los_Angeles')
  
  for (i in 1 : 8) {
    
    sOut[[i]] <- rolling_mean(df = dOut[, c(1, 2, cnst$cCol[i])],
                              nday = cnst$nday[i])
    
    names(sOut)[i] <- cnst$name[i]
    
    if (cnst$seas[i] == 'cw') { 
      
      sOut[[i]] <- sOut[[i]][which(sOut[[i]]$dte < coDt), ]
      
    } else {
      
      sOut[[i]] <- sOut[[i]][which(sOut[[i]]$dte >= coDt), ]
      
    }
    
  }
  
  sOut[[i + 1]] <- dOut; names(sOut)[i + 1] <- 'daily_stats'
  
  return(sOut)
  
}

# Function to remove warm-up days from the output time series
remove_wudy <- function(df = NULL, wudy = NULL, strD) {
  
  df <- df[which(df$Time >= wudy), ]
  
  df$Time <- df$Time - wudy
  
  # Time - convert from days to seconds and convert to POSIXct
  df$Time <- as.POSIXct(df$Time * 86400, origin = strD,
                        tz = 'America/Los_Angeles') + hours(7)
  
  return(df)
  
}

# Function to calculate DO saturation (as %) from DO conc and sat DO conc
calc_DO_sat <- function(df = NULL) {
  
  # Calculate DO saturation from DO conc and saturated DO concentration
  df$dos <- round(100 * (df$doc / df$sdo), 1)
  
  # Cap at 100% saturation
  df$dos_cap <- ifelse(df$dos > 100, 100, df$dos)
  
  # Remove the sat DO concentration column
  df <- df[, -6]
  
  return(df)
  
}

# Function to calculate the rolling means from the processed data
rolling_mean <- function(df = NULL, nday = NULL) {
  
  df$flag <- df$stat <- 0
  
  rchs <- unique(df$rch)
  
  for (i in 1 : length(rchs)) {
    
    temp <- df[which(df$rch == rchs[i]), ]
    
    for (n in 1 : nrow(temp)) {
      
      if (n < nday) { # Rolling average for less than nday
        
        temp$stat[n] <- mean(unlist(temp[1 : n, 3]), na.rm = T)
        
        temp$flag[n] <- -1 # Indicate that the stat has fewer days than nday
        
      } else {        # Rolling average for more than nday
        
        temp$stat[n] <- mean(unlist(temp[(n - (nday - 1)) : n, 3]), na.rm = T)
        
        temp$flag[n] <- 1 # Stat has sufficient number of days to calulate stat
        
      }
    }
    
    if (i == 1) {dfS <- temp} else {dfS <- rbind(dfS, temp)}
    
  }
  
  return(dfS)
  
}

# Function that returns the headers of the QUAL2Kw output files (full)
read_headers <- function(mOut = NULL, strD = NULL) {
  
  year <- paste0('YR', addZ(as.numeric(substr(strD, 1, 4)) - 2000))
  
  if (substr(mOut, nchar(mOut), nchar(mOut)) != '/') {mOut <- paste0(mOut, '/')}
  
  exts <- c('a', 'b', 'c', 'd')
  
  mOut <- paste0(mOut, 'dynamic_MC', exts, '_', year, '.txt')

  # Read in the headers
  hd <- list()
  
  for (i in 1 : 4) {
    
    hd[[i]] <- readLines(con = mOut[i], n = 2)
    
    hd[[i]] <- data.frame(t(do.call("rbind", strsplit(hd[[i]], "\\s+"))),
                          stringsAsFactors = F)
    
    hd[[i]]$X3 <- 1 : nrow(hd[[i]])
    
    hd[[i]]$X4 <- exts[i]
    
  }
  
  # Combine into one list
  hd <- rbind(hd[[1]], hd[[2]], hd[[3]], hd[[4]])
  
  names(hd) <- c('parm', 'unit', 'cols', 'fils')
  
  # Remove blank rows and duplicates of Reach, Distance, Time
  hd <- hd[-which(hd$parm == ''), ] 
  
  delX <- c('Reach', 'Distance', 'Time')
  
  for (i in 1 : length(delX)) {
    
    indx <- which(hd$parm == delX[i]); indx <- indx[2 : length(indx)]
    
    hd <- hd[-indx, ]
    
  }
  
  return(hd)
  
}

# Function to read and pre-process Q2K data output files (dynamic_MC)
read_q2k_out_2 <- function(mOut = NULL, pars = NULL, strD = NULL, wudy = NULL) {
  
  year <- paste0('YR', addZ(as.numeric(substr(strD, 1, 4)) - 2000))
  
  # Read in the header column names and file metadata
  hdrs <- read_headers(mOut, strD)
  
  # Standardize parameter names and selected pars by coercing to lower case
  parm <- hdrs$parm; par2 <- tolower(pars)
  
  hdrs$parm <- tolower(hdrs$parm)
  
  # Start the 'keeper data.frame with the first three rows (reach, time, dist)  
  keep <- hdrs[1 : 3, ]; fail <- ''
  
  # Check that each par is in the list of result outputs
  for (i in 1 : length(pars)) {
    
    if (par2[i] %in% hdrs$parm) {
      
      keep <- rbind(keep, hdrs[which(par2[i] == hdrs$parm), ])
      
    } else {
      
      fail <- append(fail, pars[i])
      
    }
  }
  
  # Return a list of bogus parameters 
  if (length(fail[-1]) != 0) {
    cat('\nThese parameters--', paste0(fail[-1], collapse = ', '),
        '--are not in the results.\n')
  }
  
  # Reorder for efficiency
  keep <- keep[order(keep$fils, keep$cols), ]
  
  if (substr(mOut, nchar(mOut), nchar(mOut)) != '/') {mOut <- paste0(mOut, '/')}  
  
  for (i in 1 : length(unique(keep$fils))) {
    
    # Read in the data from the file
    temp <- readLines(paste0(mOut, 'dynamic_MC', unique(keep$fils)[i], '_',
                             year, '.txt'))
    
    # Create a data frame from the data
    temp <- data.frame(do.call("rbind", strsplit(temp, "\\s+")),
                       stringsAsFactors = F)
    
    # Keep the columns from the requested parameters
    temp <- temp[, keep$cols[which(keep$fils == unique(keep$fils)[i])]]
    
    # Rename columns, clean up the data, and make numeric
    if (!is.data.frame(temp)) {
      
      temp <- temp[-(1 : 2)]; temp <- as.numeric(temp)
      
      temp <- data.frame(temp, stringsAsFactors = F)
      
      names(temp) <- keep$parm[which(keep$fils == unique(keep$fils)[i])]
      
    } else {
      
      names(temp) <- temp[1, ]; temp <- temp[-(1 : 2), ]
      
      temp <- data.frame(apply(temp, MARGIN = 2, FUN = as.numeric),
                         stringsAsFactors = F)
      
    }
    
    # Combine all of the data to create the output  
    if (i == 1) {data <- temp} else {data <- cbind(data, temp)}
    
  }
  
  # Remove the warm-up days (wudy)
  if(!is.null(wudy)) {data <- remove_wudy(df = data, wudy = wudy, strD = strD)}
  
  return(data)
  
}

# Graph the full suite of figures for periods & criteria
graph_output <- function(df = NULL, scen = NULL, path = NULL, n = NULL) {
  
  # Detect the year for saving the files
  year <- paste0("YR", addZ(year(df$dte[1]) - 2000))
  
  year(df$dte) <- 2017
  
  # Read in result specs (pars, criteria, stat base, and graphing)
  g <- read.csv('C:/siletz_tmdl/02_outputs/02_q2k/graph_specs_IV.csv',
                stringsAsFactors = F)[n, ]
  
  # Convert dates to POSIX
  for (x in 8 : 10) {
    g[, x] <- as.POSIXct(g[, x], '%m/%d/%Y', tz = 'America/Los_Angeles')
  }
  
  # Rename reach col for graphing
  names(df)[1] <- 'Reach'
  
  # GRAPH, PART 1 - Essential components (not comparing to standard)
  pl <- ggplot(data = df, aes(x = dte, y = stat)) + theme_classic () +
        geom_line(aes(linetype = as.factor(flag)), size = 0.9) +
        ylab(g$ylab) + scale_y_continuous(limits = c(g$ymin, g$ymax)) +    
        scale_x_datetime(limits = c(g$dte1, g$dte2)) +
        facet_wrap(.~ Reach, ncol = 2, labeller = label_both)

  if (is.na(g$nday)) {
    
    pl <- pl + theme(legend.position   = 'none',
                     axis.text.x       = element_text(size = 11),
                     axis.text.y       = element_text(size = 11),
                     axis.title.y      = element_text(size = 12),
                     axis.title.x      = element_blank(),
                     strip.text        = element_text(size = 12))
    
  } else if (g$nday == 1) {
    
    plce <- data.frame(wrds = paste0('Standard = ', g$stnd, ' ', g$unit),
                       dte  = g$dteP, stat = g$stdP)
    
    pl <- pl + theme(legend.position   = 'none',
                     axis.text.x       = element_text(size = 11),
                     axis.text.y       = element_text(size = 11),
                     axis.title.y      = element_text(size = 12),
                     axis.title.x      = element_blank(),
                     strip.text        = element_text(size = 12)) +
          geom_segment(aes(x = g$dte1, xend = g$dte2, y = g$stnd, yend = g$stnd),
                       color = 'darkblue', linetype = 2) +
          geom_text(data = plce, label = plce$wrds, hjust = g$hjst,
                    vjust = g$vjst, size = 3.25)
    
  } else {
    
    # Stardard graph label (label = wrds; x = date; y = value)
    plce <- data.frame(wrds = paste0('Standard = ', g$stnd, ' ', g$unit),
                       dte  = g$dteP, stat = g$stdP)

    pl <- pl + geom_text(data = plce, label = plce$wrds, hjust = g$hjst,
                         vjust = g$vjst, size = 3.25) +
          geom_segment(aes(x = g$dte1, xend = g$dte2, y = g$stnd, yend = g$stnd),
                       color = 'darkblue', linetype = 2) +
          theme(legend.position   = c(g$lgd1, g$lgd2),
                legend.direction  = 'horizontal',
                legend.background = element_blank(),
                legend.title      = element_blank(),
                axis.text.x       = element_text(size = 11),
                axis.text.y       = element_text(size = 11),
                axis.title.y      = element_text(size = 12),
                axis.title.x      = element_blank(),
                strip.text        = element_text(size = 12))
    
    if (length(unique(df$flag)) != 1) {
      
      lgnd <- paste0(c('Fewer', '  More'), ' than ', g$asmP,
                     ' days of predictions')

      pl <- pl + scale_linetype_manual(values = c(6, 1), labels = lgnd)

    } else {
      
      pl <- pl + theme(legend.position = 'none')
      
    }

  }

  ggsave(filename = paste0(path, '/', g$name, '_', year, '.png'),
         plot = pl, width = 11, height = 8.5, units = 'in', dpi = 300)
  
}

graph_ts <- function(df = NULL, scen = NULL, path = NULL) {

  # Detect the year for saving the files
  year <- paste0("YR", addZ(year(df$dte[1]) - 2000))
  
  # Create copy of dates for later
  df$dte2 <- df$dte
  
  # Change date year to 2017 for graphing
  year(df$dte) <- 2017

  # Rename reach col for graphing
  names(df)[1] <- 'Reach'
  
  subs <- data.frame(lblT = c('tmpI', 'tmpN', 'tmpX'), 
                     lblC = c('docI', 'docN', 'docX'),
                     lblS = c('dosI', 'dosN', 'dosX'),
                     lblG = c('Daily minimum', 'Daily mean', 'Daily maximum'),
                     stringsAsFactors = F)
  
  # Iterate through each parameter (temp, DO conc, and DO sat)
  ts <- list()

  # Reshape and change stat name for graphing
  for (i in 1 : 3) {

    ts[[i]] <- reshape2::melt(df[, c(1, 2, 15, (i * 3) + (0 : 2))],
                              id.vars = c('Reach', 'dte', 'dte2'),
                              value.name = 'valu',
                              variable.name = 'stat')
    
    for (j in 1 : 3) {ts[[i]]$stat <- gsub(subs[j, i], subs[j, 4], ts[[i]]$stat)}
    
  }

  # Read in result specs (pars, criteria, stat base, and graphing)
  g <- read.csv('C:/siletz_tmdl/02_outputs/02_q2k/graph_specs_ts.csv',
                stringsAsFactors = F)

  # Convert dates to POSIXc
  for (x in 3 : 5) {
    g[, x] <- as.POSIXct(g[, x], '%m/%d/%Y', tz = 'America/Los_Angeles')
  }

  for (i in 1 : 3) {

    pl <- ggplot(data = ts[[i]], aes(x = dte, y = valu, color = stat)) +
          theme_classic () + geom_line(size = 0.7) + ylab(g$ylab[i]) +
          scale_y_continuous(limits = c(g$ymin[i], g$ymax[i])) +
          scale_x_datetime(limits = c(g$dte1[i], g$dte2[i])) +
          facet_wrap(.~ Reach, ncol = 2, labeller = label_both) +
          scale_color_manual(values = c('darkgray', 'black', 'darkgray')) +
          geom_segment(aes(x = g$dte1[i], y = g$std1[i],
                           xend = g$dteC[i], yend = g$std1[i]),
                       size = 0.6, linetype = 'dashed', color = 'black') +
          geom_segment(aes(x = g$dteC[i], y = g$std2[i],
                           xend = g$dte2[i], yend = g$std2[i]),
                       size = 0.6, linetype = 'dashed', color = 'black') +
          annotate(geom = 'text', x = g$dte1[i] + 86400 * 6, y = g$stP1[i], hjust = 0,
                   label = paste0('Standard = ', g$std1[i], ' ', g$unit[i]), size = 3.25) +
          annotate(geom = 'text', x = g$dte2[i] - 86400 * 6, y = g$stP2[i], hjust = 1,
                   label = paste0('Standard = ', g$std2[i], ' ', g$unit[i]), size = 3.25) +
          theme(legend.position   = c(g$lgd1[i], g$lgd2[i]),
                legend.direction  = 'horizontal',
                legend.title      = element_blank(),
                axis.text.x       = element_text(size = 11),
                axis.text.y       = element_text(size = 11),
                axis.title.y      = element_text(size = 12),
                axis.title.x      = element_blank(),
                strip.text        = element_text(size = 12))

    ggsave(filename = paste0(path, '/', g$name[i], '_', year, '.png'),
           plot = pl, width = 11, height = 8.5, units = 'in', dpi = 300)

  }

  ts <- data.frame(ts[[1]][, c(1, 3, 4)], year = year, tmpC = ts[[1]]$valu,
                   do_c = ts[[2]]$valu, do_s = ts[[3]]$valu)
  
  names(ts)[2] <- 'dte'
  
  return(ts)
  
}

# Graph all parameter time series; no analysis
graph_all <- function(mOut = NULL) { # Specify directory where outputs are.
  
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
  
}

addZ <- function(v) {
  
  # Function to add a leading 0 to a vector of #s if a number is less than 10
  # This is for vectors of minutes, hours, days, and months where there are only
  # one or two digits
  
  ifelse(v < 10, paste0(0, v), as.character(v))
  
}
