suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))

rm(list = ls()); cat('\014')

source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/cal_functions_q2k.R')
source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/proc_results.R')
source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')

scen <- unlist(read_ctrF_H()[1])

pth <- paste0('C:/siletz_tmdl/02_outputs/02_q2k/', scen)

if (!file.exists(pth)) {dir.create(pth)}

# Process the results
data <- DO_analysis(scen = scen, wudy = 7, strD = '2017-07-08')

for (i in 1 : 8) {

  write.csv(file = paste0(pth, '/', scen, '_', names(data)[i], '.csv'),
            x = data[[i]], row.names = F)

  graph_output(df = data[[i]], scen = scen, path = pth, n = i)

}
