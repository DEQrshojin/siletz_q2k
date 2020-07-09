library(ggplot2); library(gridExtra); library(dplyr); library(reshape2)
library(lubridate); library(plotly)

rm(list = ls()); cat('\014')

#

# READ IN DATA ----
scns <- data.frame(nmbr = c('base', 'S002', 'S014', 'S017', 'S018'),
                   name = c('1', '2', '3', '4', '5'),
                   stringsAsFactors = F)

path <- 'C:/siletz_tmdl/02_outputs/02_q2k/'; data <- list()

for (n in 1 : nrow(scns)) {
  data[[n]] <- readRDS(paste0(path, scns[n, 1], '/results_summary.RData'))
}

names(data) <- scns$nmbr

# Save for later if needed
# saveRDS(data, 'C:/siletz_tmdl/02_outputs/02_q2k/scen_results_20200527_w_SPV.RData')
# # 
data <- readRDS('C:/siletz_tmdl/02_outputs/02_q2k/scen_results_20200527_w_SPV.RData')

# 3D PLOT ---- 
# Combine all of the Spawning DO excursions into one table
for (i in 1 : length(data)) {
  data[[i]]$sp_DO_7D_C$pars <- 'Conc'; data[[i]]$sp_DO_7D_S$pars <- 'Sat'
  data[[i]]$sp_DO_7D_C$scen <- data[[i]]$sp_DO_7D_S$scen <- names(data)[i]
  names(data[[i]]$sp_DO_7D_C)[3] <- names(data[[i]]$sp_DO_7D_S)[3] <- 'valu' 
  if (i == 1) {
    spDO <- rbind(data[[i]]$sp_DO_7D_C, data[[i]]$sp_DO_7D_S)
  } else {
    spDO <- rbind(spDO, data[[i]]$sp_DO_7D_C, data[[i]]$sp_DO_7D_S)
  }
}

# Remove valu and flag
spDO <- dcast(spDO[, c(1, 2, 4, 6, 7)], rch + dte + scen ~ pars, value.var = 'stat',
              fun.aggregate = mean)

spDO$year <- year(spDO$dte) 

spDO$excr <- ifelse(spDO$Conc < 11 & spDO$Sat < 95, 1, 0)

doEx <- spDO %>% group_by(scen, year, rch) %>% summarize(excr = sum(excr))

# Change 0s to NAs for plotting
doEx$excr <- ifelse(doEx$excr == 0, NA, doEx$excr)

scns <- data.frame(nmbr = c('base', 'S002', 'S014', 'S017', 'S018'),
                   name = c('1', '2', '3', '4', '5'),
                   stringsAsFactors = F)

doEx <- merge(doEx, scns, by.x = 'scen', by.y = 'nmbr')

doEx <- doEx[order(doEx$name), ] 

# Try plotting 3D with x = reach, y = year, z = scen 
pl <- plot_ly(doEx, x = ~rch, y = ~year, z = ~name, color = ~excr, size = ~excr,
              marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 25)) %>%
              add_markers()

pl <- pl %>% layout(scene = list(xaxis = list(title = 'Reach', dtick = 1),
                                 yaxis = list(title = 'Year', dtick = 1, range = c(2017, 2004)),
                                 zaxis = list(title = 'Scenario', hjust = 0)))

pl

# TABLE 7 - TOTAL EXCURSIONS ----
# Count the number of excursions
excr <- data.frame(
  matrix(data = NA, nrow = length(data), ncol = 4,
         dimnames = list(names(data), names(data[["base"]][["excr"]])))
)

for (i in 1 : length(data)) {
  for (j in 1 : length(data[[i]][['excr']])) {
    excr[i, j] <- nrow(data[[i]][['excr']][[j]])
  }
}

# write.csv(x = excr, file = 'C:/siletz_misc/memo/tables/excr_summ.csv',
#           row.names = T)

# TABLES 8 AND 9 - EXCURSION DAYS PER YEAR AND REACH ----
# Change NAs back to 0 for analysis
doEx$excr <- ifelse(is.na(doEx$excr), 0, doEx$excr)

# Isolate scenario by year
tabY <- dcast(doEx, year ~ name, value.var = 'excr', fun.aggregate = sum)

write.csv(tabY, file = 'C:/siletz_misc/memo/tables/scenario_x_year.csv',
          row.names = T)

# Isolate scenario by reach
tabR <- dcast(doEx, rch ~ name, value.var = 'excr', fun.aggregate = sum)

write.csv(tabR, file = 'C:/siletz_misc/memo/tables/scenario_x_reach.csv',
          row.names = T)

# FIGURES 7 & 8 DO SAT 2006 & 2014 ----
rm(list = ls()); cat('\014')

scen <- list(Nutrients = c('base', 'S002', 'S003', 'S004', 'S005'),
             Shade     = c('base', 'S014', 'S015', 'S016'),
             Combined  = c('base', 'S002', 'S014', 'S017'))

scns <- data.frame(nmbr = c('base', 'S002', 'S003', 'S004', 'S005',
                            'S014', 'S015', 'S016', 'S017'),
                   name = c('1', '2a', '2b', '2c', '2d', '3a', '3b', '3c', '4'),
                   stringsAsFactors = F)

path <- 'C:/siletz_tmdl/02_outputs/02_q2k/'

rchs <- c(4, 8)

year <- c(2006, 2014)

for (n in 1 : length(scen)) {
  
  data <- list()
  
  # READ IN THE DATA AND ASSIGN PLOT MARKERS
  for (i in 1 : length(scen[[n]])) {
    
    data[[i]] <- readRDS(paste0(path, scen[[n]][i], '/results_summary.RData'))
    
    data[[i]]$sp_DO_7D_S$scen <- data[[i]]$sp_DO_7D_C$scen <- scen[[n]][i]
    
    names(data[[i]]$sp_DO_7D_S)[3] <- names(data[[i]]$sp_DO_7D_C)[3] <- 'valu'
    
    data[[i]]$sp_DO_7D_S$pars <- 'DOSat'; data[[i]]$sp_DO_7D_C$pars <- 'DOCnc'
    
    if(i == 1) {
      
      temp <- rbind(data[[i]]$sp_DO_7D_S, data[[i]]$sp_DO_7D_C)
      
    } else {
      
      temp <- rbind(temp, data[[i]]$sp_DO_7D_S, data[[i]]$sp_DO_7D_C)
      
    }
    
  }
  
  temp$group <- names(scen)[n] 
  
  if (n == 1) {spDO <- temp} else {spDO <- rbind(spDO, temp)}
  
}

spDO$year <- year(spDO$dte)

# Isolate reaches and years
spDO <- spDO[which(spDO$rch %in% rchs & spDO$year %in% year), ]

spDO <- merge(spDO, scns, by.x = 'scen', by.y = 'nmbr')

spDO$group <- factor(spDO$group, levels(factor(spDO$group))[c(2, 3, 1)])

path <- 'C:/siletz_misc/memo/figures/v2/'; figN <- 7 : 8

spDO <- spDO[which(month(spDO$dte) == 9), ]

spDO$rch <- paste0('Reach ', spDO$rch)

names(spDO)[length(spDO)] <- 'Scenario'

ann1 <- 'DO standard = 11 mg/L'

ann2 <- 'DO standard =\n95% Saturation'

clrs <- c('black', 'darkred', 'darkgreen', 'darkorchid', 'burlywood4', 'seagreen1',
          'seagreen3', 'seagreen', 'darkslateblue')

for (i in 1 : 2) {
  
  temp <- spDO[which(spDO$year == year[i]), ]
  
  if(i == 1) {
    lblD <- max(temp$dte); hjst <- 1
  } else {
    lblD <- min(temp$dte); hjst <- 0    
  }
  
  p1 <- ggplot(temp[which(temp$pars == 'DOCnc'), ], aes(x = dte, y = stat, color = Scenario)) +
        geom_line() + theme_bw() + geom_hline(yintercept = 11, linetype = 'dashed') +
        scale_y_continuous(limits = c(9, 11.5), breaks = seq(9, 11.5, 0.5)) +
        scale_x_datetime(date_breaks = '7 days', date_labels = '%m-%d') +
        annotate(geom = 'text', x = lblD, y = 11.1, label = ann1,
                 vjust = 0, hjust = hjst, size = 2.8) +
        ylab('DO Concentration (mg/L)') + theme(axis.title.x = element_blank()) +
        theme(legend.position = c(0.25, 0.88), legend.direction = 'horizontal',
              legend.background = element_rect(fill = 'transparent'),
              legend.title = element_blank(), legend.text = element_text(size = 8),
              legend.key.size = unit(0.75, "line"),
              strip.background = element_rect(fill = 'white'),
              strip.background.y = element_blank(), strip.text.y = element_blank()) +
        scale_color_manual(values = clrs) + facet_grid(group ~ rch); p1

  p2 <- ggplot(temp[which(temp$pars == 'DOSat'), ], aes(x = dte, y = stat, color = Scenario)) +
        geom_line() + theme_bw() + geom_hline(yintercept = 95.0, linetype = 'dashed') +
        scale_y_continuous(limits = c(94, 97), breaks = 94 : 97) +
        scale_x_datetime(date_breaks = '7 days', date_labels = '%m-%d') +
        annotate(geom = 'text', x = lblD, y = 94.9, label = ann2,
                 vjust = 1, hjust = hjst, size = 2.8) +
        theme(axis.title.x = element_blank(),legend.position = 'none',
              strip.background = element_rect(fill = 'white')) +
        ylab('DO Saturation (%)') + scale_color_manual(values = clrs) +
        facet_grid(group ~ rch); p2
  
  pl <- grid.arrange(p1, p2, ncol = 2)

  ggsave(filename = paste0(path, 'fig_0', figN[i], '_scen_comp_', year[i], '.png'),
         plot = pl, width = 11, height = 8.5, units = 'in', dpi = 300)
  
}

# FIGURES 9 & 10 LONGITUDINAL PLOTS ----
# Use the data from line 44
spDO_BU <- spDO

spDO <- spDO_BU

# Isolate minimum results for each year for each reach; want to find a minimum
# in a reach that doesn't necessarily fall of the days where the minimum occurs
# for reaches 4 and 8. 
DOmi <- spDO %>% group_by(scen, year, rch) %>% summarize(DO_c = min(Conc),
                                                         DO_s = min(Sat))
DOmi <- melt(DOmi, id.vars = c('scen', 'year', 'rch'), value.name = 'valu',
             variable.name = 'pars')

# Boxplot of seasonal minimums per year grouped by reach, faceted by scenario
DOmi <- merge(DOmi, scns, by.x = 'scen', by.y = 'nmbr')

DOmi$name <- paste0('Scenario ', DOmi$name)

path <- 'C:/siletz_misc/memo/figures/v2/'

ann1 <- 'DO standard = 11 mg/L'

ann2 <- 'DO standard =\n95% Saturation'

# Figure 9 - DO Concentration
pl <- ggplot(DOmi, aes(x = rch, y = DO_c, group = rch)) + geom_boxplot() +
      annotate(geom = 'text', x = 2, y = 11.1, label = ann1, size = 2.7, vjust = 0) +
      scale_y_continuous(limits = c(8, 12), breaks = 8 : 12) +
      scale_x_reverse(breaks = 10 : 1) + theme_classic() +
      ylab('DO Concentration (mg/L)') + xlab('QUAL2Kw Reach') +
      geom_hline(yintercept = 11.0, linetype = 'dashed') + facet_wrap(.~ name)

ggsave(filename = paste0(path, 'fig_9_long_scen_comp_DOCnc.png'),
       plot = pl, width = 11, height = 8.5, units = 'in', dpi = 300)

# Figure 10 - DO Sat
pl <- ggplot(DOmi, aes(x = rch, y = DO_s, group = rch)) + geom_boxplot() +
      annotate(geom = 'text', x = 2, y = 94.6, label = ann2, size = 2.7) +
      scale_x_reverse(breaks = 10 : 1) + theme_classic() +
      scale_y_continuous(limits = c(93, 99), breaks = 93 :99) +
      ylab('DO Saturation (%)') + xlab('QUAL2Kw Reach') +
      geom_hline(yintercept = 95.0, linetype = 'dashed') + facet_wrap(.~ name)

ggsave(filename = paste0(path, 'fig_10_long_scen_comp_DOSat.png'),
       plot = pl, width = 11, height = 8.5, units = 'in', dpi = 300)

