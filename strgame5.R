h = NOVO_UNI$h_usd
u = NOVO_UNI$equity
RFR = 0.01*as.numeric(tail(NOVO_UNI$libors, 1))
setkey(params, name)

SIZE = params['SIZE', as.numeric(value)]
BARRIERS = params['STRIKE', as.numeric(value)]
TTM = length(BARRIERS)
TAIL = 120
COUPON = 1
UNI = u[ticker%in%params['UNI', value], ]

h = h[, UNI$ticker]
h = h[, !is.na(tail(h, TAIL)[1,])]  # TAIL-ago returns exist
UNI = UNI[ticker%in%colnames(h), ][, ':='(ivol=NA, div=NA)]  # only TAIL-ago existing tickers are left in UNI

# Filtering
#return_2018 = exp(t(t(colSums(h[index(h)>'2017-01-01' & index(h)<'2019-01-01', ])))) - 1
#return_2018 = data.table(ticker=rownames(return_2018), rt=as.numeric(return_2018))
#u = u[return_2018[rt> -0.1, ticker], on='ticker']
#h = h[, u$ticker]

h = tail(h[rowSums(is.na(h)) == 0, ], TAIL)  # last TAIL returns are taken
SIGMAS = ifelse(is.na(UNI$ivol), constituent_vols(h, 252), UNI$ivol)
DIVS = ifelse(is.na(UNI$div), 0, UNI$div)
COR_MAT_ALL = cor(h)

cmb = combn(1:nrow(UNI), SIZE)
idx = 1:ncol(cmb)
#all_tickers = UNI[, ticker]; idx = c(); for(i in 1:ncol(cmb)) if(sum(etf_tickers%in%all_tickers[cmb[,i]])>0) idx[length(idx)+1] = i
##all_gics = UNI[, gics_code]; idx2 = c(); for(i in idx) if(length(unique(all_gics[cmb[,i]]))>=4) idx2[length(idx2)+1] = i
sectors = UNI[, sector]
idx2 = c()
for(i in idx) if(sum(is.na(sectors[cmb[,i]]))==0 && length(unique(sectors[cmb[,i]]))>=4) idx2[length(idx2)+1] = i

#for(bi in 1:nrow(BARRIERS)){
#    barriers_in = as.numeric(BARRIERS[bi, ])
cheapest_baskets = function(barriers_in){
    res = foreach(i = idx2)%dopar%{
        if(i%%100==0) print(i)
        list(basket=cmb[,i], price=wo_calculate_an(TTM, barriers_in, SIGMAS[cmb[,i]], COR_MAT_ALL[cmb[,i], cmb[,i]], RFR, DIVS[cmb[,i]], COUPON))
    }

    prc = array(0, length(res))
    for(i in 1:length(res)) if(!is.null(res[[i]])) prc[i] = res[[i]]$price
    res = res[order(prc)[1:100]]
    return(res)
}

baskets = cheapest_baskets(BARRIERS)
res = as.data.table(foreach(b=baskets,.combine=rbind)%do%c(UNI[b$basket, ticker], round(b$price, 3)))








