rm(list = ls()); cat('\014')

source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')

nNme <- read_ctrF_H(); nNme <- nNme$name

# FILE/DIRECTOR SPECIFICATIONS
fDir <- paste0('C:/siletz_tmdl/02_outputs/02_q2k/', nNme)
t1Dr <- 'D:/siletz_q2k/08_pest/03_wq/01_cw_cal'
t2Dr <- 'D:/siletz_q2k/08_pest/03_wq/02_sp_val'
fles <- c('dynamic_MCa.txt', 'dynamic_MCb.txt', 'dynamic_MCc.txt',
          'dynamic_MCd.txt', 'slz_q2k_wq.out')
cwFl <- paste0('cw_', fles); spFl <- paste0('sp_', fles)

# CW
file.copy(paste0(fDir, '/', cwFl), t1Dr)
file.rename(paste0(t1Dr, '/', cwFl), paste0(t1Dr, '/', fles))

# SP
file.copy(paste0(fDir, '/', spFl), t2Dr)
file.rename(paste0(t2Dr, '/', spFl), paste0(t2Dr, '/', fles))


