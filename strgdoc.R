library(googlesheets)
library(dplyr)
library(data.table)
library(xts)
library(corpcor)
library(doMC)
registerDoMC(cores=7)

source('/home/aslepnev/git/ej/stropt.R')
source('/home/aslepnev/git/ej/strfunc.R')

#u$sigma = abs(rnorm(nrow(u))*0.1)  # TODO
D = get(load('/home/aslepnev/webhub/zacks_data.RData'))
p = D$p
u = D$u

gs_auth(token = '/home/aslepnev/git/ej/gdoc_doc.R')
s = gs_key('1y9KUgukyEvfjAVaDYCHPf1rkFn83q8LWS_0tybz2pIM')
#hrange = as.data.frame(gs_read(s, 'hist', range='A1:Z1000', col_names=FALSE))
#NAMES = as.character(hrange[1,])
#DIVS = as.numeric(hrange[2,])
#SIGMAS = as.numeric(hrange[3,])
#quotes = data.table(hrange[-(1:3),])[, lapply(.SD, as.numeric)]

#gs_edit_cells(s, ws = 'wo-optimizer', anchor = "A8", input = u[1:100,1:2], byrow = TRUE, col_names=FALSE)
rparams = as.data.frame(gs_read(s, 'wo-optimizer', range='A2:B4', col_names=FALSE))
params = as.list(as.numeric(gsub('%','',rparams[,2])))
names(params) = gsub(':','',rparams[,1])

SIZE = params[['BASKET SIZE']]
COUPON = params[['COUPON']]*0.01
RFR = params[['RFR']]*0.01
BARRIERS = as.data.frame(gs_read(s, 'wo-optimizer', range='D3:D10', col_names=FALSE))[,1]
TTM = length(BARRIERS)
TAIL = 120

UNI = as.data.table(gs_read(s, 'wo-optimizer', range='B7:D1000', col_names=FALSE))[-1,]
colnames(UNI) = c('ticker', 'sigma', 'dividend')
UNI[, ticker:=foreach(x=strsplit(UNI$ticker,' '), .combine=c)%do%x[1]]
UNI = UNI[UNI%in%u[, ticker]]

u = u[match(UNI$ticker, u$ticker), ]
h = diff(log(p[, match(UNI$ticker, colnames(p))]))
h = tail(h[rowSums(is.na(h)) == 0, ], TAIL)
SIGMAS = ifelse(is.na(UNI$sigma), foreach(i=1:ncol(h),.combine=c)%do%{ sd(h[,i])*sqrt(252) }, UNI$sigma)
DIVS = ifelse(is.na(UNI$dividend), 0, UNI$dividend)
COR_MAT_ALL = cor(h)

cmb = combn(1:nrow(u), 3)

res = foreach(i = 1:ncol(cmb))%dopar%{
    if(i%%1000==0) print(i)
    list(basket=cmb[,i], price=wo_calculate_an(TTM, BARRIERS, SIGMAS[cmb[,i]], COR_MAT_ALL[cmb[,i], cmb[,i]], RFR, DIVS[cmb[,i]], COUPON))
}

prc = array(0, length(res))
for(i in 1:length(res)) prc[i] = res[[i]]$price


