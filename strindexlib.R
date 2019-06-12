library(knitr)
library(mnormt)
library(mvtnorm)
library(corpcor)
library(nloptr)
library(RColorBrewer)
library(gridExtra)
library(ggplot2)
library(lubridate)
#library(mailR)
library(googlesheets)

library(hash)
library(doMC)
library(data.table)
library(foreach)
library(xts)
library(tseries)
library(xtable)
library(reshape2)
library(ggplot2)
#library(ggthemes)
#library(mgcv)

setDTthreads(1)
registerDoMC(cores=15)

R_STATE_DATA_MASK = '/home/aslepnev/webhub/strtelestate_current_name.csv'
COB_CTL <<- list(xtol_rel=1e-8, maxeval=5000)

#source('/home/aslepnev/git/ej/novo_all_libraries.R')
#source('/home/aslepnev/git/ej/novo_data_preload.R')
#source('/home/aslepnev/git/ej/novo_data_generate_uni.R')

source('/home/aslepnev/git/ej/novo_google_func.R')
source('/home/aslepnev/git/ej/novo_google_wo_optimizer.R')

source('/home/aslepnev/git/ej/novo_basic_quant_func.R')
source('/home/aslepnev/git/ej/novo_quant_func.R')
source('/home/aslepnev/git/ej/novo_advanced_quant_func.R')

source('/home/aslepnev/git/ej/novo_static_prep.R')
source('/home/aslepnev/git/ej/novo_uni_func.R')

source('/home/aslepnev/git/ej/novo_output_lib.R')
source('/home/aslepnev/git/ej/novo_product_reports.R')
source('/home/aslepnev/git/ej/novo_index_builders.R')
source('/home/aslepnev/git/ej/novo_screen_functions.R')
source('/home/aslepnev/git/ej/novo_python_workers.R')
source('/home/aslepnev/git/ej/novo_latex_func.R')


