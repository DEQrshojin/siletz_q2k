source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')

nNme <- read_ctrF_H(); nNme <- nNme$name

wDir <- 'C:/siletz_tmdl/03_models/02_q2k/'

# Move the output files
oDir <- 'C:/siletz_tmdl/02_outputs/02_q2k/'

dir.create(paste0(oDir, nNme))

mveF <- c('dynamic_MCa.txt', 'dynamic_MCb.txt', 'dynamic_MCc.txt',
          'dynamic_MCd.txt', 'slz_q2k_wq.out')

delF <- c('dynamic_HTS.txt', 'dynamic_STSa.txt', 'dynamic_STSb.txt')

# Delete files
file.remove(paste0(wDir, '/', delF))

# Move files
fFil <- paste0(wDir, '/', mveF)

tFil <- paste0(oDir, nNme, '/', mveF)

file.rename(fFil, tFil)

# Archive the Q2K files
file.rename(paste0(wDir, '/slz_q2k_wq.q2k'),
            paste0(wDir, 'archive/', nNme, '_slz_q2k_wq.q2k'))
