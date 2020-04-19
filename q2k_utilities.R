#_______________________________________________________________________________
# MODIFY Q2K INPUTS IN MODEL FILE
modify_q2k_inputs <- function(name = NULL, ctrF = NULL) {
  
  options(scipen = 999)
  
  wDir <- 'C:/siletz_tmdl/03_models/02_q2k/'

  for (x in 1 : 2) { # Iterate through each period 

    seas <- ifelse(x == 1, 'cw', 'sp')
  
    # READ IN THE Q2K TEMPLATE FILES FOR MODIFICATION  
    q2kF <- readLines(paste0(wDir, seas, '/slz_q2k_wq_', seas, '.tpr'))

    # ORGANIZE & MODIFY INITIAL CONDITIONS
    q2kF <- mod_initC(q2kF, name, seas)
    
    # ORGANIZE THE MET DATA
    q2kF <- mod_metBC(q2k = q2kF, name, seas)
    
    # ORGANIZE & MODIFY HEADWATER CONDITIONS
    q2kF <- mod_hwBC(q2k = q2kF, name, seas)
    
    # ORGANIZE & MODIFY TRIBUTARY INFLOW CONDITIONS
    q2kF <- mod_infBC(q2k = q2kF, name, seas)
  
    # WRITE TO Q2K FILE - WHEN MODIFYING PARAMETERS MODIFY THE Q2K FILE
    writeLines(q2kF, paste0(wDir, seas, '/slz_q2k_wq.q2k'))

  }
}

#_______________________________________________________________________________
# MODIFY Q2K INITIAL CONDITIONS IN MODEL FILE
mod_initC <- function(q2k = NULL, name = NULL, seas = NULL) {
  
  # Load the initial conditions
  intC <- read.csv(paste0('C:/siletz_tmdl/01_inputs/02_q2k/bc_intC/', name,
                          '_intC_', seas, '.csv'), stringsAsFactors = F)

  bbio <- paste0('x_bbio', addZ(1 : 10), '_____x')
  
  tmp1 <- tmp2 <- NULL
  
  for (i in 1 : nrow(intC)) {
    
    tmp1 <- append(tmp1, paste0(round(intC$tmp_dgC[i], 3), ',',
                                round(intC$pH[i], 3), ',',
                                bbio[i], ',', 370, ',', 51))

    tmp2 <- append(tmp2, paste0(round(intC$cnd_uSc[i], 3), ',',
                                round(intC$iss_mgL[i], 3), ',',
                                round(intC$do_mgL[i], 3), ',',
                                round(intC$cs_mgL[i], 3), ',',
                                round(intC$cf_mgL[i], 3), ',',
                                round(intC$orn_ugL[i], 3), ',',
                                round(intC$nh3_ugL[i], 3),  ',',
                                round(intC$nox_ugL[i], 3), ',',
                                round(intC$orp_ugL[i], 3),  ',',
                                round(intC$po4_ugL[i], 3), ',',
                                round(intC$phy_ugL2[i], 3), ',',
                                round(intC$oss_mgL[i], 3), ',',
                                round(intC$bct_cfu[i], 3),  ',',
                                round(intC$gen_na[i], 3),  ',',
                                round(intC$alk_mgL[i], 3)))

  }
  
  temp <- append(tmp1, tmp2)
  
  # Identify the line where initial conditions go
  repL <- which(q2k == 'x_intCnd_____x')
  
  # Replace those lines
  q2k <- append(q2k[1 : (repL - 1)], c(temp, q2k[(repL + 1) : length(q2k)]))
  
  return(q2k)
  
}

#_______________________________________________________________________________
# MODIFY Q2K MET BOUNDARY CONDITIONS IN MODEL FILE
mod_metBC <- function(q2k = NULL, name = NULL, seas = NULL) {
  
  # Load the met boundary conditions
  metC <- data.frame(dir = paste0('mt_', c('eShd', 'Ta',   'Td',   'wnd',  'CC')),
                     fil =               c('eShd', 'airT', 'dwpT', 'wndS', 'cCov'),
                     stringsAsFactors = F)
  
  mt <- list()

  pth <- "C:/siletz_tmdl/01_inputs/02_q2k/"
  
  for (i in 1 : nrow(metC)) {
    mt[[i]] <- read.csv(paste0(pth, metC$dir[i], '/', name, '_', metC$fil[i],
                               '_', seas, '.csv'), stringsAsFactors = F)
  }
  
  # Clean  up and organize the data
  names(mt) <- metC$fil
  
  # MODIFY MET FOR REPLACEMENT   
  tmp1 <- tmp2 <- tmp3 <- tmp4 <- NULL
  
  for (n in 1 : (as.integer(length(mt[[1]]) / 5) + 1)) { # 5-TS BLOCK LOOP

    # Set the indeces of the 5-day block
    if (n != (as.integer(length(mt[[1]]) / 5) + 1)) {
      
      indx <- (n * 5 - 4) : (n * 5)              # Calculate the sets of indeces
      
    } else { # For the possibly incomplete last set 
      
      mdls <- 5 * (length(mt[[1]]) / 5 - as.integer(length(mt[[1]]) / 5))
      
      indx <- (n * 5 - 4) : (n * 5 - (5 - mdls)) # Calculate the sets of indeces
      
    }

    tmp3 <- NULL
    
    for (o in indx) {
      
      tmp2 <- NULL
      
      for (p in 1 : nrow(mt[[1]])) {
        
        tmp1 <- paste0(mt[[1]][p, o], ',', mt[[2]][p, o], ',', mt[[3]][p, o], ',',
                       mt[[4]][p, o], ',', mt[[5]][p, o], ',', 0)
        
        tmp2 <- append(tmp2, tmp1) # Constitutes a 10-line block (of reaches)
        
      }
      
      tmp3 <- append(tmp3, tmp2) # Constitutes a 50-line block of reach/TS
      
    }
    
    tmp4 <- append(tmp4, tmp3)
    
  }

  # Identify the line where initial conditions go
  repL <- which(q2k == 'x_metbcs_____x')
  
  # Replace those lines
  q2k <- append(q2k[1 : (repL - 1)], c(tmp4, q2k[(repL + 1) : length(q2k)]))
  
  return(q2k)

}

#_______________________________________________________________________________
# MODIFY Q2K HEADWATER BOUNDARY CONDITIONS IN MODEL FILE
mod_hwBC <- function(q2k = NULL, name = NULL, seas = NULL) {
  
  # Load the met boundary conditions
  hdwr <- read.csv(paste0('C:/siletz_tmdl/01_inputs/02_q2k/bc_hdwr/', name,
                          '_hdwr_', seas, '.csv'), stringsAsFactors = F)

  # Remove rows 19 through 36 (They're for tailwater which I didn't use)
  hdwr <- hdwr[1 : 18, ]; row.names(hdwr) <- hdwr[, 1]; hdwr <- hdwr[, -1]
  
  tmp2 <- NULL
  
  for (n in 1 : length(hdwr)) { # Iterate through each time step
    
    tmp1 <- NULL
    
    for (o in 1 : nrow(hdwr)) { # Iterate through each parameter & concatenate
      
      if (o == 1) {
        tmp1 = paste0(hdwr[o, n], ',')
      } else if (o != nrow(hdwr)) {
        tmp1 <- paste0(tmp1, hdwr[o, n], ',')
      } else {
        tmp1 <- paste0(tmp1, hdwr[o, n], ',\"\"')
      }

    }

    tmp2 <- append(tmp2, tmp1)
    
  }

  # Identify the line where initial conditions go
  repL <- which(q2k == 'x_hwinfl_____x')
  
  # Replace those lines
  q2k <- append(q2k[1 : (repL - 1)], c(tmp2, q2k[(repL + 1) : length(q2k)]))
  
  return(q2k)
  
}

#_______________________________________________________________________________
# MODIFY Q2K TRIB INFLOW BOUNDARY CONDITIONS IN MODEL FILE
mod_infBC <- function(q2k = NULL, name = NULL, seas = NULL) {
  
  # Load the met boundary conditions
  infw <- read.csv(paste0('C:/siletz_tmdl/01_inputs/02_q2k/bc_infw/', name,
                          '_infw_', seas, '.csv'), stringsAsFactors = F)
  
  # Replace the dates with time step indeces
  for (i in unique(as.numeric(infw$Reach))) {
    
    cond <- which(as.numeric(infw$Reach) == i)
    
    infw[cond, 2] <- (1 : nrow(infw[cond, ])) - 1
    
  }
  
  for (i in 1 : length(infw)) {
    if (typeof(infw[, i]) != 'character') {
      infw[, i] <- as.character(round(infw[, i], 5))
    }
  }
   
  # Add a column of "
  infw$dmmy <- '\"\"'
  
  # Condense each row into one string seperate items by commas
  infw <- apply(infw, 1, paste, collapse = ',')
  
  # Identify the line where initial conditions go
  repL <- which(q2k == 'x_trbinf_____x')
  
  # Replace those lines
  q2k <- append(q2k[1 : (repL - 1)], c(infw, q2k[(repL + 1) : length(q2k)]))
  
  return(q2k)

}

#_______________________________________________________________________________
# MODIFY Q2K PARAMETERS IN MODEL FILE
modify_q2k_pars <- function() {
  
  wDir <- 'D:/siletz_q2k/08_pest/03_wq'
  
  # READ IN DATA AND ORGANIZE
  # READ IN THE PARAMETER VALUE FILE (.prv) FILE WITH PEST MODIFIED PAR VALUES
  parV <- readLines(paste0(wDir, '/slz_q2k_wq.prv'))
  
  # Remove the first lines of the parameter value file
  parV <- parV[-which(substr(parV, 1, 2) == "#{3}")]
  
  # Parse the parameter values from the parameter name
  parV <- data.frame(do.call("rbind", strsplit(parV, ",")), stringsAsFactors = F)
  
  # Coerce the second column to numbers
  parV$X2 <- as.numeric(parV$X2)
  
  # Change the names of the variables to have the x_parnme_____x
  parV$X1 <- paste0('x_', tolower(parV$X1), '_____x')
  
  # READ IN THE Q2K MODEL FILES FOR MODIFICATION
  q2k1 <- readLines(paste0(wDir, '/01_cw_cal/slz_q2k_wq_CW.tpr'))
  q2k2 <- readLines(paste0(wDir, '/02_sp_val/slz_q2k_wq_SP.tpr'))
  
  # Find these in the tpr file and replace with the value
  for (i in 1 : nrow(parV)) {
    q2k1 <- gsub(parV$X1[i], parV$X2[i], q2k1)
    q2k2 <- gsub(parV$X1[i], parV$X2[i], q2k2)
  }
  
  # WRITE NEW MODEL FILE
  q2kF1 <- file(paste0(wDir, '/01_cw_cal/slz_q2k_wq.q2k'))
  q2kF2 <- file(paste0(wDir, '/02_sp_val/slz_q2k_wq.q2k'))
  
  # Write output .q2k file
  writeLines(text = q2k1, con = q2kF1)
  writeLines(text = q2k2, con = q2kF2)
  
  close(q2kF1); close(q2kF2)

}

#_______________________________________________________________________________
# READ THE Q2K CONTROL FILE
read_ctrF_Q <- function() {
  
  ctrF <- readLines("C:/siletz_tmdl/03_models/q2k_ctrl.csv")
  
  ctrF <- ctrF[-which(substr(ctrF, 1, 3) == '###' | ctrF == '')]
  
  ctrF <- strsplit(ctrF, ',')
  
  for (i in 1 : length(ctrF)) {
    names(ctrF)[i] <- ctrF[[i]][1]; ctrF[[i]] <- ctrF[[i]][2]
  }
  
  # Find the indeces of the mod met vectors
  mods <- which(names(ctrF) %in% c('mdTa', 'mdTd', 'mdCc', 'mdWs'))
  
  for (i in 1 : length(mods)) {
  
    if (ctrF[[mods[i]]] != 'FALSE') {
      
      ctrF[[mods[i]]] <- gsub('\\(|\\)', '', ctrF[[mods[i]]])
      
      ctrF[[mods[i]]] <- strsplit(ctrF[[mods[i]]], ';')
      
      ctrF[[mods[i]]] <- as.numeric(ctrF[[mods[i]]][[1]])
      
    }
  }

  return(ctrF)
  
}

#_______________________________________________________________________________
# ADD WARM UP DAYS TO INPUT DATA SETS
add_warm_up <- function(df = NULL, nday = NULL) {
  
  if (nday != 0) {
    
    # Rename the dates column for consistency
    names(df)[1] <- 'date'
    
    # Chunk out the first day
    temp <- df[1 : 24, ]
    
    # Create the data frame of repeating first days
    for (i in 1 : (nday - 1)) {temp <- rbind(temp, df[1 : 24, ])}
    
    df <- rbind(temp, df)
    
    # Recalculate the date/times of each data-frame
    df$date <- seq(df$date[1] - nday * 86400, df$date[nrow(df)], 3600)
    
  }
  
  return(df)
  
}

#_______________________________________________________________________________
# MODIFY MET INPUT DATA SETS
modify_met_df <- function(df = NULL, strD = NULL, endD = NULL, mdfy = NULL) {
  
  # Accepts an input dataframe and modifies it by a set of reach-specific modifiers
  
  df <- data.frame(t(df), stringsAsFactors = F)
  
  df$date <- as.POSIXct(row.names(df), 'X%Y.%m.%d.%H.%M.%S',
                        tz = 'America/Los_Angeles')
  
  dtes <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  cond <- which(df$date >= strD & df$date <= endD)
  
  temp <- df[cond, 1 : (length(df) - 1)]
  
  for (i in 1 : length(temp)) {temp[, i] <- temp[, i] * mdfy[i] / 100} 
  
  df[cond, 1 : (length(df) - 1)] <- temp
  
  df <- data.frame(t(df[, 1 : 10]), stringsAsFactors = F)
  
  return(df)
  
}

# ADD A LEADING ZERO 
addZ <- function(v) {
  
  # Function to add a leading 0 to a vector of #s if a number is less than 10
  # This is for vectors of minutes, hours, days, and months where there are only 
  # one or two digits
  
  ifelse(v < 10, paste0(0, v), as.character(v))
  
}

copy_inputs <- function(inpt = NULL, scnN = NULL, scnO = NULL) {
  
  # Function to copy input files from previous scenarios, specify the input file
  # type (inpt; e.g., solar), new scenario name (scnN; e.g., scen_004), and 
  # previous scenario name (scnO; e.g., scen_002)
  typs <- data.frame(inpt = c('HW', 'INF', 'IC', 'CC', 'ES', 'TA', 'TD', 'WND'),
                     fldr = c('bc_hdwr', 'bc_infw', 'bc_intC', 'mt_CC', 'mt_eShd',
                              'mt_Ta', 'mt_Td', 'mt_wnd'),
                     fils = c('hdwr', 'infw', 'intC', 'cCov', 'eShd', 'airT',
                              'dwpT', 'wndS'),
                     stringsAsFactors = F)
  
  typs <- typs[which(typs$inpt %in% inpt), ]
  
  dir <- 'C:/siletz_tmdl/01_inputs/02_q2k/'
  
  for (i in 1 : nrow(typs)) {

    file.copy(from = c(paste0(dir, typs[i, 2], '/', scnO, '_', typs[i, 3],
                              c('_cw', '_sp'), '.csv')),
              to   = c(paste0(dir, typs[i, 2], '/', scnN, '_', typs[i, 3],
                              c('_cw', '_sp'), '.csv')),
              overwrite = T)
    
  }
}


