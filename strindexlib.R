library(mnormt)
library(mvtnorm)
library(corpcor)
library(nloptr)

library(gridExtra)
library(ggplot2)
library(mailR)
library(googlesheets)

library(hash)
library(doMC)
library(data.table)
library(foreach)
library(xts)
library(tseries)
registerDoMC(cores=5)

R_STATE_DATA_MASK = '/home/aslepnev/webhub/strtelestate_current_name.RData'
COB_CTL <<- list(xtol_rel=1e-8, maxeval=5000)
UNI_FILENAMES <<- hash()
UNI_FILENAMES['it10'] = '/home/aslepnev/git/ej/it_top10_uni.RData'

