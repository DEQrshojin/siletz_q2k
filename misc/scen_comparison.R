library(ggplot2); library(gridExtra); library(dplyr); library(reshape2)

rm(list = ls()); cat('\014')

# SCENARIOS
scen <-           c('scen_005', 'scen_021', 'scen_026', 'scen_027', 'scen_031', 'scen_036', 'scen_037')

# SEASONS AND SCENARIOS
seas <- list(cw = c('scen_005', 'scen_021', 'scen_026',                                     'scen_037'),
             sp = c('scen_005', 'scen_021',             'scen_027', 'scen_031', 'scen_036'            ))

cols <- c('scen_005' = 'burlywood',
          'scen_021' = 'darkslategray',
          'scen_026' = 'orangered',
          'scen_027' = 'navyblue',
          'scen_031' = 'orange3',
          'scen_036' = 'green4',
          'scen_037' = 'olivedrab4')

# STANDARDS: METRICS AND CRITERIA
stnd <- data.frame(mtrc = c('cw_DO_30DM_C', 'cw_DO_30DM_S', 'cw_DO_7DI_C',
                            'cw_DO_ABSM_C', 'cw_T_7DADM', 'sp_DO_7D_C',
                            'sp_DO_7D_S', 'sp_T_7DADM'),
                   stnd = c(8, 90, 6.5, 6, 16, 11, 95, 13), stringsAsFactors = F)

# Initialize the data object
data <- data.frame(
  matrix(data = NA, nrow = 0, ncol = 8,
         dimnames = list(NULL, c('scen', 'seas', 'date', 'rchs', 'valu',
                                 'flg1', 'mtrc', 'crit')))
)

# Start reading in the data
for (i in 1 : 2) {
  
  for (j in 1 : length(seas[[i]])) {
    
    path <- paste0('C:/siletz_tmdl/02_outputs/02_q2k/', seas[[i]][j], '/figures/')

    for (k in 1 : nrow(stnd)) {
      
      # Remove the results from the opposite season
      if (substr(stnd[k, 1], 1, 2) == names(seas)[i]) {
        
        # Read in data
        tmp1 <- read.csv(paste0(path, seas[[i]][j], '_', stnd[k, 1], '.csv'),
                         stringsAsFactors = F)
        
        tmp1 <- tmp1[which(tmp1$rch != 0), ] # Remove reach 0
        
        # Assign a season and scenario, if scenario is in both, it's CW
        tmp1$scen <- seas[[i]][j]; tmp1$seas <- names(seas)[i]
        
        # Assign the standard info: metric and criterion
        tmp1$mtrc <- stnd[k, 1]; tmp1$stnd <- stnd[k, 2]
        
        # Reorganize the df
        tmp1 <- tmp1[, c(6, 7, 2, 1, 4, 5, 8, 9)]; names(tmp1) <- names(data)
        
        data <- rbind(data, tmp1)
        
      }
    }
  }
}

data$date <- as.POSIXct(data$date, '%Y-%m-%d', tz = 'America/Los_Angeles')

# Isolate the parameter
data$pars <- ifelse(
  substr(data$mtrc, 4, 4) == 'T', 'Temp',
  paste0('DO_', substr(data$mtrc, nchar(data$mtrc), nchar(data$mtrc)))
)

# Isolate the excursions - deal with DO Concentration and Saturation first
exD1 <- data[which(data$pars != 'Temp' & !(data$crit %in% c(6.5, 6.0))), ]

tmp2 <- cbind(dcast(exD1, scen + date + rchs + seas ~ pars, value.var = 'valu',
                    fun.aggregate = mean),
              dcast(exD1, scen + date + rchs + seas ~ pars, value.var = 'crit',
                    fun.aggregate = mean))

tmp2 <- tmp2[, c(1, 2, 3, 4, 5, 6, 11, 12)]

tmp2$flg2 <- ifelse((tmp2$DO_C < tmp2$DO_C.1 & tmp2$DO_S < tmp2$DO_S.1), 1, 0)

tmp2 <- melt(tmp2[, -(7 : 8)], id.vars = c('scen', 'date', 'rchs', 'seas', 'flg2'),
             value.name = 'valu', variable.name = 'pars')

exD1 <- exD1 %>% arrange(scen, rchs, date)

tmp2 <- tmp2 %>% arrange(scen, rchs, date)

# Check the order to do the column bind: valu should be the same
a <- which(exD1$valu != tmp2$valu) # All Good!

exD1 <- cbind(exD1, flg2 = tmp2$flg2)

# Now do the CW 7D min mean & Abs min
exD2 <- data[which(data$pars == 'DO_C' & data$crit %in% c(6.5, 6.0)), ]

exD2$flg2 <- ifelse(exD2$valu < exD2$crit, 1, 0)

# Temperature
ex_T <- data[which(data$pars == 'Temp'), ]

ex_T$flg2 <- ifelse(ex_T$valu > ex_T$crit, 1, 0)

data <- rbind(exD1, exD2, ex_T)

data <- data %>% arrange(scen, mtrc, date)

# So based on the results, remove all but reach 8
data <- data[which(data$rchs == 8), ]

# GRAPHS ----
# Read in result specs (pars, criteria, stat base, and graphing)
g <- read.csv('C:/siletz_tmdl/02_outputs/02_q2k/graph_specs_III.csv',
              stringsAsFactors = F)

# Convert dates to POSIX
for (x in c(8 : 10, 21)) {
  g[, x] <- as.POSIXct(g[, x], '%m/%d/%Y', tz = 'America/Los_Angeles')
}

# Create a DF with NAs in valu for the legend panel
lgnd <- data[which(data$date %in% c(g$dte1, g$dte2) & data$pars == 'DO_S'), ]

lgnd$valu <- NA

p <- list()

for (i in 1 : nrow(stnd)) {
  
  temp <- data[which(data$mtrc == stnd[i, 1] ), ]
  
  r <- g[which(g$name == stnd[i, 1]), ]
  
  plce <- data.frame(wrds = paste0('Standard = ', r$stnd, ' ', r$unit),
                     date  = r$dteP, valu = r$stdP, scen = 'Black')
  
  # GRAPH, PART 1 - Essential components (not comparing to standard)
  p[[i]] <- ggplot(temp, aes(x = date, y = valu, color = scen)) +
            scale_color_manual(values = cols) + 
            geom_line(aes(linetype = as.factor(flg1)), size = 0.6) +
            theme_classic() + ylab(r$ylab) + # ggtitle() + 
            scale_y_continuous(limits = c(r$ymin, r$ymax),
                               breaks = seq(r$ymin, r$ymax, r$step)) +    
            scale_x_datetime(limits = c(r$dte1, r$dte2), date_breaks = '7 days',
                             date_labels = '%m-%d') +
            geom_hline(yintercept = r$stnd, color = 'black', linetype = 2) +
            geom_text(data = plce, label = plce$wrds, hjust = r$hjst,
                      color = 'black', vjust = r$vjst, size = 3.25) +
            annotate(geom = 'text', x = r$tltX, y = r$tltY, label = r$desc,
                     vjust = 1, size = 3.5) +
            theme(legend.position = 'none',
                  axis.text.y = element_text(size = 9),
                  axis.text.x = element_text(size = 9),
                  axis.title.y = element_text(size = 10),
                  axis.title.x = element_blank())
  
  if (!is.na(r$asmP)) {p[[i]] <- p[[i]] + scale_linetype_manual(values = c(5, 1))}

}

ltxt <- c('Fewer days than required to calculate statistic',
          'Sufficient number of days to calculate statistic')

lNll <- as.POSIXct(c("1/1/2000", "1/1/2001"), format = "%m/%d/%Y", 
                  tz = "America/Los_Angeles")

p[[9]] <- ggplot(lgnd, aes(x = date, y = valu, color = scen)) + 
          geom_line(aes(linetype = as.factor(flg1))) + theme_minimal() +
          scale_color_manual(values = cols) + 
          scale_x_datetime(limits = lNll) +
          theme(panel.grid = element_blank(), panel.border = element_blank(),
                axis.title = element_blank(), axis.text = element_blank(),
                axis.ticks = element_blank(), legend.box = "horizontal",
                legend.title = element_blank(), legend.position = c(0.5, 0.5),
                legend.direction = "vertical") +
          scale_linetype_manual(values = c(5, 1), labels = ltxt) +
          guides(col = guide_legend(ncol = 5))

layO <- matrix(data = c(1, 3, 5, 7, 9, 2, 4, 6, 8, 9), nrow = 5)

plot <- grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[6]], p[[7]], p[[5]],
                     p[[8]], p[[9]], layout_matrix = layO,
                     heights = c(2.6, 2.6, 2.6, 2.6, 0.6))

ggsave(filename = 'C:/siletz_tmdl/02_outputs/02_q2k/memo/test.png', plot = plot,
       width = 11, height = 8.5, units = 'in', dpi = 300)

