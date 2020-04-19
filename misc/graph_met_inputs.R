#2345678901234567890123456789012345678901234567890123456789012345678901234567890
library(reshape2); library(ggplot2)

rm(list = ls()); cat('\014')

source('C:/siletz_tmdl/04_scripts/02_q2k/02_R/cal_functions_q2k.R')

scen <- 'scen_022' #c('scen_005', 'scen_006', 'scen_009', 'scen_018', 'scen_019', 'scen_020')
yrs  <- 2004 # c(2017, 2014, 2015, 2005, 2007, 2012)
dtes <- data.frame(strD_cw = paste0(yrs, '-07-11'),
                   endD_cw = paste0(yrs, '-08-29'),
                   strD_sp = paste0(yrs, '-09-08'),
                   endD_sp = paste0(yrs, '-10-16'),
                   stringsAsFactors = F)
seas <- c('cw', 'sp')
pth <- 'C:/siletz_tmdl/01_inputs/02_q2k/mt_figures/'

for (i in 1 : length(scen)) { # Loop through scenarios

  for (j in 1 : 2) {          # Lopo through seasons

    # LOAD MET DATA
    mtDt <- add_met(scen[i], dtes[i, 2 * j - 1], dtes[i, 2 * j], seas[j])

    # Make Cloud Cover percentage from 0 to 100
    mtDt$cCov <- round(mtDt$cCov * 100, 0)

    mtDt <- melt(mtDt, id.vars = c('date', 'rch'), value.name = 'val',
                 variable.name = 'par')

    # Switch out the abbrviated names with the full variable name
    swtc <- data.frame(one = unique(mtDt$par),
                       two = c('(A) Air Temperature (oC)',
                               '(C) Cloud Cover (%)',
                               '(B) Dew Point Temp (oC)',
                               '(D) Wind Speed (m/s)'))

    mtDt <- merge(mtDt, swtc, by.x = 'par', by.y = 'one', all = T)

    mtDt$rch <- gsub('R', 'Reach ', mtDt$rch)

    # Create the met data plot
    pl <- ggplot(data = mtDt, aes(x = date, y = val, color = rch)) +
          geom_line() + xlab(NULL) + ylab(NULL) + theme_bw() +
          scale_x_datetime(breaks = "2 weeks") +
          scale_y_continuous(breaks = scales::pretty_breaks(7), limits = c(0, NA)) +
          facet_wrap(.~ two, strip.position = "left", ncol = 1, scales = 'free') +
          guides(col = guide_legend(ncol = 5)) +
          theme(strip.background = element_blank(), strip.placement = "outside",
                legend.position = 'bottom', legend.title = element_blank(),
                legend.text  = element_text(size = 13),
                strip.text   = element_text(size = 13),
                axis.text.y  = element_text(size = 11),
                axis.text.x  = element_text(size = 11),
                axis.title.y = element_text(size = 13),
                axis.title.x = element_text(size = 13))

    ggsave(filename = paste0(pth, scen[i], '_met_data_', seas[j], '.png'),
           plot = pl, width = 8.5, height = 11, dpi = 300, units = "in")

  }
}

