source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/q2k_utilities.R')
source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')

# BUILD THE INPUT BATCH FILE ___________________________________________________
# Read in the control file and file name
ctrF <- read_ctrF_Q(); nNme <- read_ctrF_H()

# Parse the lines from the control file that pertain to inflow BC inclusion
bcsW <- which(names(ctrF) %in% c('hspf', 'mntr', 'dvrs', 'stpI'))

tmp1 <- c('@echo off', '')

# Add batch file line if ANY inflow BC changes are included 
if (length(which(unlist(ctrF[bcsW]) == 'TRUE')) != 0) {
  tmp1 <- append(tmp1, c(paste0('Rscript C:\\siletz_tmdl\\04_scripts\\02_q2k\\',
                                '02_R\\bcs_input_q2k.R'), ''))
}

# Write the "Create Input Files" batch file
writeLines(tmp1, 'C:/siletz_tmdl/03_models/02_q2k/tmp1.bat')

# BUILD THE MODIFY Q2K INPUT AND PARAMETER BATCH FILE __________________________
tmp2 <- c('@echo off', '',
          'Rscript C:\\siletz_tmdl\\04_scripts\\02_q2k\\02_R\\mod_q2k_inputs.R', '',
          'Rscript C:\\siletz_tmdl\\04_scripts\\02_q2k\\02_R\\mod_q2k_pars.R')

writeLines(tmp2, 'C:/siletz_tmdl/03_models/02_q2k/tmp2.bat')

# BUILD THE MODEL RUN BATCH FILE _______________________________________________
tmp3 <- c('@echo off', '',
          'C:\\siletz_tmdl\\03_models\\02_q2k\\qual2kw6.exe')

writeLines(tmp3, 'C:/siletz_tmdl/03_models/02_q2k/tmp3.bat')

# BUILD THE MOVE AND ARCHIVE BATCH FILE ________________________________________
tmp4 <- c('@echo off', '',
          'Rscript C:\\siletz_tmdl\\04_scripts\\02_q2k\\02_R\\move_q2k_files.R')

writeLines(tmp4, 'C:/siletz_tmdl/03_models/02_q2k/tmp4.bat')

# BUILD THE PROCESS RESULTS BATCH FILE
tmp5 <- c('@echo off', '',
          'Rscript C:\\siletz_tmdl\\04_scripts\\02_q2k\\02_R\\ctrl_results.R')

writeLines(tmp5, 'C:/siletz_tmdl/03_models/02_q2k/tmp5.bat')

cat('2. QUAL2Kw control batch files written.\n\n')
