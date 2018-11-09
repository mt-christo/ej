library(googlesheets)
library(dplyr)
library(data.table)
library(xts)

u = as.data.table(get(load('/home/aslepnev/git/ej/uni.RData')))
u$sigma = abs(rnorm(nrow(u))*0.1)  # TODO
p = get(load('/home/aslepnev/git/ej/uniprc.RData'))

s = gs_key('1y9KUgukyEvfjAVaDYCHPf1rkFn83q8LWS_0tybz2pIM')
w = as.data.frame(gs_read(s, 'Sheet1', range='A1:A1000', col_names=FALSE))[,1]
idx = match(w, u[, gsub(' Equity', '', SecName)])

