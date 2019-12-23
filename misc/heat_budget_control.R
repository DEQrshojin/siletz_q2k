rm(list = ls())

source('D:/siletz_q2k/04_scripts/misc/q2k_flux_functions.R')

# Counter
countFil <- file('D:/siletz_q2k/08_pest/02_temp/t_cntr.txt')

# Write counter to counter file
m <- as.numeric(readLines(countFil)); n <- m + 1

writeLines(as.character(n), countFil); close(countFil)

# -----------------------------------------------------------------------------
# Make a change in the model, set date to 07/22/2017, run the model
# -----------------------------------------------------------------------------

# Change the output file name
change_output_names(date = '2017-07-22', m = m)

# -----------------------------------------------------------------------------
# Set date to 08/01/2017, run the model
# -----------------------------------------------------------------------------

# Change the output file name
change_output_names(date = '2017-08-01', m = m)

# Run the comparison analysis
suppressMessages(compare_heat_budgets(it = m))
