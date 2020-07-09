library(ggplot2); library(reshape2)

rm(list = ls()); cat('\014')

scen <- c('base', 'S014', 'S015', 'S016')

path <- 'C:/siletz_tmdl/01_inputs/02_q2k/mt_eShd'

data <- list()

for (i in 1 : length(scen)) {
  data[[i]] <- data.frame(t(read.csv(paste0(path, '/', scen[i], '/eShd_YR04.csv'))))
}

names(data) <- scen

data <- data.frame(date = as.POSIXct(row.names(data$base), 'X%Y.%m.%d.%H.%M.%S',
                                     tz = 'America/Los_Angeles'),
                   base_R2 = data[['base']]$X2, S014_R2 = data[['S014']]$X2,
                   S015_R2 = data[['S015']]$X2, S016_R2 = data[['S016']]$X2,                   
                   base_R4 = data[['base']]$X4, S014_R4 = data[['S014']]$X4,
                   S015_R4 = data[['S015']]$X4, S016_R4 = data[['S016']]$X4,
                   base_R8 = data[['base']]$X8, S014_R8 = data[['S014']]$X8,
                   S015_R8 = data[['S015']]$X8, S016_R8 = data[['S016']]$X8,
                   stringsAsFactors = F)

data <- melt(data, id.vars = 'date', value.name = 'ES', variable.name = 'ScenRch')

data$scen <- substr(data$ScenRch, 1, 4); data$rch <- substr(data$ScenRch, 6, 7)

# windows(12, 12)

xlms <- as.POSIXct(c('2004-07-01', '2004-07-15'), '%Y-%m-%d', tz = 'America/Los_Angeles')

pl <- ggplot(data, aes(x = date, y = ES, color = scen)) + geom_line() +
      theme_classic() + scale_x_datetime(limits = xlms) +
      facet_wrap(.~ rch, ncol = 1); pl

ggsave(plot = pl, filename = 'C:/siletz_misc/memo/figures/v2/ES_comparisons.png',
       width = 11, height = 8.5, dpi = 300, units = 'in')

# Hmmm...try a boxplot
pl <- ggplot(data, aes(x = scen, y = ES, group = scen)) + geom_boxplot() +
      theme_classic() + facet_wrap(.~ rch, ncol = 1); pl

