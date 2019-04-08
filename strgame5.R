library(devtools)
# install_github('jennybc/googlesheets')

source('/home/aslepnev/git/ej/stropt.R')
source('/home/aslepnev/git/ej/strfunc.R')

p = D$p
u = D$u
s = refresh_gs_auth()

load_wo_optimizer_params_gdoc = function(g_token){
}

s = refresh_s()

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
BARRIERS = as.matrix(as.data.frame(gs_read(s, 'wo-optimizer', range='E3:I20', col_names=FALSE)))
TTM = ncol(BARRIERS)
TAIL = 120

DICTIONARY = as.data.table(gs_read(s, 'wo-universe', range='A1:F1000', col_names=TRUE))
UNI = as.data.table(gs_read(s, 'wo-optimizer', range='B8:B1000', col_names=FALSE))[, .(ticker=X1)]
UNI = DICTIONARY[UNI, on='ticker'][, ':='(ivol=ivol*0.01, div=div*0.01)]
UNI$ticker = foreach(x=strsplit(UNI$ticker,' '), .combine=c)%do%x[1]
UNI = UNI[ticker%in%u$ticker, ][, head(.SD, 1), by='ticker']  # u = u[match(UNI$ticker, u$ticker), ]

h = diff(log(p[, match(UNI$ticker, colnames(p))]))
h = h[, !is.na(tail(h, TAIL)[1,])]  # TAIL-ago returns exist
UNI = UNI[ticker%in%colnames(h), ]  # only TAIL-ago existing tickers are left in UNI

# Filtering
#return_2018 = exp(t(t(colSums(h[index(h)>'2017-01-01' & index(h)<'2019-01-01', ])))) - 1
#return_2018 = data.table(ticker=rownames(return_2018), rt=as.numeric(return_2018))
#u = u[return_2018[rt> -0.1, ticker], on='ticker']
#h = h[, u$ticker]

# ADDING CUSTOM ETFS
ext_h = function(filename, vcp, start_dt = '1900-01-01'){
    d = get(load(filename))
    res = foreach(x=d,.combine=rbind)%do%x$h
    res = exp(cumsum(res[index(res)>start_dt]))
    res
}

h_etf = foreach(filename=Sys.glob('/home/aslepnev/data/idx4_custom*'),.combine=cbind)%do%ext_h(filename, 0, '2009-04-01')
etf_tickers = paste0('GRINDEX',1:ncol(h_etf))
colnames(h_etf) = etf_tickers
h_etf = diff(log(h_etf))
h_etf = tail(h_etf[rowSums(is.na(h_etf))==0 & index(h_etf)%in%index(h), ], TAIL)

UNI = rbind(UNI, data.table(name='', ticker=etf_tickers, div=NA, ivol=NA, gics_code=0, country=''))
h = cbind(h, h_etf)
# -- CUSTOM ETFS

h = tail(h[rowSums(is.na(h)) == 0, ], TAIL)  # last TAIL returns are taken
SIGMAS = ifelse(is.na(UNI$ivol), foreach(i=1:ncol(h),.combine=c)%do%{ sd(h[,i])*sqrt(252) }, UNI$ivol)
DIVS = ifelse(is.na(UNI$div), 0, UNI$div)
COR_MAT_ALL = cor(h)

cmb = combn(1:nrow(UNI), SIZE)
idx = 1:ncol(cmb)
all_tickers = UNI[, ticker]; idx = c(); for(i in 1:ncol(cmb)) if(sum(etf_tickers%in%all_tickers[cmb[,i]])>0) idx[length(idx)+1] = i
#all_gics = UNI[, gics_code]; idx2 = c(); for(i in idx) if(length(unique(all_gics[cmb[,i]]))>=4) idx2[length(idx2)+1] = i
all_gics = UNI[, gics_code]; idx2 = c(); for(i in idx) if(sum(is.na(all_gics[cmb[,i]]))==0 && length(unique(all_gics[cmb[,i]]))>=4) idx2[length(idx2)+1] = i

for(bi in 1:nrow(BARRIERS)){
    barriers_in = as.numeric(BARRIERS[bi, ])
    res = foreach(i = idx2)%dopar%{
        if(i%%1000==0) print(i)
        list(basket=cmb[,i], price=wo_calculate_an(TTM, barriers_in, SIGMAS[cmb[,i]], COR_MAT_ALL[cmb[,i], cmb[,i]], RFR, DIVS[cmb[,i]], COUPON))
    }

    prc = array(0, length(res))
    for(i in 1:length(res)) if(!is.null(res[[i]])) prc[i] = res[[i]]$price
    prc = prc[prc > 0]
    b_sign = paste(barriers_in, collapse='-')
    save(res, file=paste0('/home/aslepnev/webhub/wo_opt_', b_sign, '.RData'))

    s = refresh_s()
    tryCatch({gs_ws_delete(s, ws=b_sign)}, error=function(e) {})
    s = refresh_s()
    gs_ws_new(s, ws_title=b_sign, row_extent=500, col_extent=25)
    s = refresh_s()
    cheapest = as.data.table(foreach(i=order(prc)[1:500],.combine=rbind)%do%c(UNI[res[[i]]$basket, ticker], prc[i]))
    gs_edit_cells(s, ws=b_sign, anchor = "A1", input = cheapest, byrow = TRUE, col_names=FALSE)
}

prc = array(0, length(res))
for(i in 1:length(res)) if(!is.null(res[[i]])) prc[i] = res[[i]]$price
prc = prc[prc>0]
save(res, file='/home/aslepnev/webhub/wo_opt1.RData')

min(prc)
cheap_i = res[[which.min(prc)]]$basket
UNI[cheap_i, ticker]
SIGMAS[cheap_i]
COR_MAT_ALL[cheap_i, cheap_i]
COUPON

cheapest = as.data.table(foreach(i=order(prc)[1:500],.combine=rbind)%do%c(UNI[res[[i]]$basket, ticker], prc[i]))
gs_edit_cells(s, ws = 'wo-optimizer', anchor = "F3", input = cheapest, byrow = TRUE, col_names=FALSE)










