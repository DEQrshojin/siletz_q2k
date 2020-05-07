source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/q2k_utilities.R')

source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')

suppressMessages(library(lubridate))

ctrF <- read_ctrF_Q(); nNme <- read_ctrF_H()

modify_q2k_inputs(name = nNme$name, ctrF = ctrF)

cat('4. Model Q2K file met and BC inputs updated.\n\n')
