# Function that serves as wrapper to analyzing DO and temperature data
DO_analysis <- function(scen = NULL, wudy = NULL, strD = NULL) {
  
  fils <- paste0('C:/siletz_tmdl/02_outputs/02_q2k/', scen)

  pars <- c('Temp', 'DissO2', 'DOsat')
  
  # mOut <- read_q2k_out_2(mOut = fils, pars = pars, strD = strD, wudy = wudy)
  
  mOut <- readRDS('C:/siletz_tmdl/02_outputs/02_q2k/YR17/temp.RData')
  
  # saveRDS(mOut, 'C:/siletz_tmdl/02_outputs/02_q2k/YR17/temp.RData')

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
read_headers <- function(mOut = NULL) {
  
  if (substr(mOut, nchar(mOut), nchar(mOut)) != '/') {mOut <- paste0(mOut, '/')}
  
  exts <- c('a', 'b', 'c', 'd')
  
  mOut <- paste0(mOut, 'dynamic_MC', exts, '.txt')
  
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
  
  # Read in the header column names and file metadata
  hdrs <- read_headers(mOut)
  
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
    temp <- readLines(paste0(mOut, 'dynamic_MC', unique(keep$fils)[i], '.txt'))
    
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
    
    lgnd <- paste0(c('Fewer', '  More'), ' than ', g$asmP,
                   ' days of predictions')
    
    pl <- pl + scale_linetype_manual(values = c(6, 1), labels = lgnd) +  
          geom_segment(aes(x = g$dte1, xend = g$dte2, y = g$stnd, yend = g$stnd),
                       color = 'darkblue', linetype = 2) +
          geom_text(data = plce, label = plce$wrds, hjust = g$hjst,
                    vjust = g$vjst, size = 3.25) +
          theme(legend.position   = c(g$lgd1, g$lgd2),
                legend.direction  = 'horizontal',
                legend.background = element_blank(),
                legend.title      = element_blank(),
                axis.text.x       = element_text(size = 11),
                axis.text.y       = element_text(size = 11),
                axis.title.y      = element_text(size = 12),
                axis.title.x      = element_blank(),
                strip.text        = element_text(size = 12))
    
  }
  
  ggsave(filename = paste0(path, '/', scen, '_', g$name, '.png'), plot = pl,
         width = 11, height = 8.5, units = 'in', dpi = 300)
  
}