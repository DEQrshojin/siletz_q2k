rm(list = ls()); cat('\014')

wDir <- 'C:/siletz_tmdl/03_models/02_q2k'

# __________________________________________________________________________----
# READ IN DATA AND ORGANIZE ----
# READ IN THE PARAMETER VALUE FILE (.prv) FILE WITH PEST MODIFIED PAR VALUES
parV <- readLines(paste0(wDir, '/slz_q2k_wq.prv'))

# Remove the first lines of the parameter value file
parV <- parV[-which(substr(parV, 1, 3) == "###")]

# Parse the parameter values from the parameter name
parV <- data.frame(do.call("rbind", strsplit(parV, ",")), stringsAsFactors = F)

# Coerce the second column to numbers
parV$X2 <- as.numeric(parV$X2)

# Change the names of the variables to have the x_parnme_____x
parV$X1 <- paste0('x_', tolower(parV$X1), '_____x')

# __________________________________________________________________________----
# READ IN THE Q2K MODEL FILES FOR MODIFICATION
q2k1 <- readLines(paste0(wDir, '/slz_q2k_wq.q2k'))

# Find these in the tpr file and replace with the value
for (i in 1 : nrow(parV)) {q2k1 <- gsub(parV$X1[i], parV$X2[i], q2k1)}

# WRITE NEW MODEL FILE
q2kF1 <- file(paste0(wDir, '/slz_q2k_wq.q2k'))

# Write output .q2k file
writeLines(text = q2k1, con = q2kF1)

close(q2kF1)
