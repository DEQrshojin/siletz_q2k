library(ggplot2)

base <- read.csv("C:/siletz_tmdl/01_inputs/02_q2k/bc_infw/base/infw_YR04.csv")

s002 <- read.csv("C:/siletz_tmdl/01_inputs/02_q2k/bc_infw/S002/infw_YR04.csv")

s003 <- read.csv("C:/siletz_tmdl/01_inputs/02_q2k/bc_infw/S003/infw_YR04.csv")

base$date <- s002$date <- s003$date <- as.POSIXct(base$date, '%Y-%m-%d %H:%M:%S',
                                                  tz = 'America/Los_Angeles')

base$scen <- 'base'; s002$scen <- 's002'; s003$scen <- 's003'

data <- rbind(base, s002, s003)

data <- data[, c(22, 1, 2, 4, 11, 12, 13, 14, 15, 17)]

path <- 'C:/siletz_tmdl/05_misc/data/'

for(i in 4 : 10) {

  pl <- ggplot(data, aes(x = date, y = data[, i], color = scen)) + geom_line() +
        theme_classic() + facet_wrap(.~ Reach, ncol = 2) + ylab(names(data)[i]) +
        scale_y_log10()
  
  ggsave(filename = paste0(path, names(data)[i], '.png'), plot = pl, width = 11,
         height = 8.5, units = 'in', dpi = 300)

}


