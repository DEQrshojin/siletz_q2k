suppressMessages(library(lubridate)); suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))

source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/cal_functions_q2k.R')
source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/proc_results.R')
source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')

# Variables
scen <- unlist(read_ctrF_H()[1])

pth <- paste0('C:/siletz_tmdl/02_outputs/02_q2k/', scen)

year <- 2004 : 2017

yr <- paste0("YR", addZ(year - 2000))

# Create the folders for outputs (if necessary)
if (!file.exists(pth)) {dir.create(pth)}

if (!file.exists(paste0(pth, '/figures'))) {dir.create(paste0(pth, '/figures'))}

for (i in 1 : length(year)) {
  
  a <- Sys.time()
  
  # Process the results
  data <- DO_analysis(scen = scen, wudy = 7, strD = paste0(year[i], '-07-08'))
  
  for (j in 1 : 8) {
    graph_output(df = data[[j]], scen = scen, path = paste0(pth, '/figures'), n = j)
  }

  data[['daily_stats']] <- graph_ts(df = data[['daily_stats']], scen = scen,
                                    path = paste0(pth, '/figures'))

  b <- round(Sys.time() - a, 2); c <- units(b)
  
  if (i == 1) {
    
    rslt <- data
    
  } else { 
    
    for (k in 1 : length(data)) { 
      
      rslt[[k]] <- rbind(rslt[[k]], data[[k]])
      
    }
  }
  
  cat(paste0('Processing ', year[i], ' results completed in ', b, ' ', c, '.\n\n'))

}

# Run DO summary
rslt <- DO_summary(rslt)

# Save results object the results out:
saveRDS(object = rslt, file = paste0(pth, '/results_summary.RData'))
