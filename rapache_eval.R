library(xts)
library(foreach)
library(RCurl)
library(lubridate)
library(googlesheets)
library(dplyr)
library(data.table)
library(xts)
library(corpcor)
library(doMC)
library(mvtnorm)
library(bizdays)
library(xtable)
registerDoMC(cores=5)

create.calendar(name='mycal', weekdays=c('saturday', 'sunday'))
bizoff = bizdays::offset

source('/home/aslepnev/git/ej/webfunc.R')
#source('/home/aslepnev/git/ej/strfunc.R')

source('/home/aslepnev/git/ej/cme_preload.R')
source('/home/aslepnev/git/ej/cmefunc.R')

source('/home/aslepnev/git/ej/novo_uni_func.R')
source('/home/aslepnev/git/ej/novo_quant_func.R')
source('/home/aslepnev/git/ej/novo_data_preload.R')
source('/home/aslepnev/git/ej/novo_google_func.R')
