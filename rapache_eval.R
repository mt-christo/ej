library(xts)
library(foreach)
library(data.table)
library(RCurl)
library(lubridate)

s <<- get(load('/home/aslepnev/git/ej/cmedata1.RData'))

source('/home/aslepnev/git/ej/strfunc.R')

source('/home/aslepnev/git/ej/cmefunc.R')
source('/home/aslepnev/git/ej/cme_preload.R')
