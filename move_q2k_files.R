rm(list = ls()); cat('\014')

suppressMessages(library(lubridate))

source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')

source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/q2k_utilities.R')

nNme <- read_ctrF_H(); nNme <- nNme$name; year <- read_ctrF_Q()

wDir <- 'C:/siletz_tmdl/03_models/02_q2k'

# Move the output files
nDir <- paste0('C:/siletz_tmdl/02_outputs/02_q2k/', nNme)

if (!file.exists(nDir)) {dir.create(nDir)}

base <- paste0('dynamic_MC', c('a', 'b', 'c', 'd'))

mvF1 <- c(paste0(base, '.txt'), 'slz_q2k_wq.out')

mvF2 <- c(paste0(base, '_', which_year(year$str1), '.txt'),
          paste0('slz_q2k_wq', '_', which_year(year$str1), '.out'))

View(mvF1); View(mvF2)

delF <- c('dynamic_HTS.txt', 'dynamic_STSa.txt', 'dynamic_STSb.txt')

# Delete files
invisible(file.remove(paste0(wDir, '/', delF)))

# Move files
fFil <- paste0(wDir, '/', mvF1)

tFil <- paste0(nDir, '/', mvF2)

invisible(file.rename(fFil, tFil))

# Archive the Q2K files
if (!file.exists(paste0(wDir, '/archive/', nNme))) {
  dir.create(paste0(wDir, '/archive/', nNme))
}

invisible(
  file.rename(paste0(wDir, '/slz_q2k_wq.q2k'),
              paste0(wDir, '/archive/', nNme, '/', which_year(year$str1),
                     '_slz_q2k_wq.q2k'))
)

