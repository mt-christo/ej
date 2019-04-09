setwd('/home/aslepnev/git/ej')

source('strindexlib.R')
library(quantmod)

D = prep_data(TRUE)
H = D$h; U = D$u
rebal_dates = as.Date(index(apply.yearly(H[,1],FUN=length)))
U = prorate_universe_multiple(U, H, as.Date("2018-03-01"), rebal_dates)
# u=U; h=D$h; screen_func=screen_momentum; screen_params=list(N=5); vc_params=list(window=20, level=0.05, max_weight=2); weights=array(1/screen_params$N, screen_params$N)
IDX = build_index(U, H, rebal_dates, screen_func=screen_momentum, screen_params=list(N=5), vc_params=list(window=20, level=0.05, max_weight=2), weights=array(0.2,5))
plot(IDX)






#library(RCurl)
#library(XML)
#library(rlist)
#library(rjson)

library(data.table)
library(quantmod)
library(foreach)
library(stringr)

u = fread('/home/aslepnev/git/ej/zacks.csv')
colnames(u) = c('name','ticker','mcap')
#i = 1
#p = foreach(x=u$Ticker,.errorhandling='pass')%do%{ Sys.sleep(1.1); print(i); i=i+1; get(getSymbols(x))[,paste0(x,'.Adjusted')] }
#save(p, file='/home/anton/git/ej/zacks_yhoo.RData')
p = get(load('/home/aslepnev/webhub/zacks_yhoo.RData'))
p = foreach(x=p[5<foreach(x=p,.combine=c)%do%length(x)],.combine=cbind)%do%x
colnames(p) = gsub('[.]Adjusted', '', colnames(p))
#ped = rbindlist(foreach(x=colanmes(pe))%do%as.data.table('dt'=index(
D = list(u=u[ticker%in%colnames(p),], p=p[,u[ticker%in%colnames(p), ticker]])  # 'u' and 'h' match
save(D, file='/home/aslepnev/webhub/zacks_data.RData')



e = fread('/home/anton/Downloads/finviz.csv')[order(Volume, decreasing=TRUE),][1:200,]
pe = foreach(x=e$Ticker,.errorhandling='pass')%do%{ Sys.sleep(1.1); print(i); i=i+1; get(getSymbols(x))[,paste0(x,'.Adjusted')] }
save(pe, file='/home/anton/git/ej/etf_yhoo.RData')


p = get(load('/home/aslepnev/webhub/zacks_yhoo.RData'))

e = fread('/home/anton/Downloads/finviz.csv')[order(Volume, decreasing=TRUE),][1:200, dt:=as.Date('2018-12-17')][,-1]
colnames(e) = c('ticker','name','sector','industry','country','mcap','volume','dt')
pe = get(load('/home/anton/git/ej/etf_yhoo.RData'))
pe = foreach(x=pe,.combine=cbind)%do%x
colnames(pe) = gsub('[.]Adjusted', '', colnames(pe))
#ped = rbindlist(foreach(x=colanmes(pe))%do%as.data.table('dt'=index(
D = list(u=e[ticker%in%colnames(pe),], h=pe[,e[ticker%in%colnames(pe), ticker]])  # 'u' and 'h' match
save(D, file='/home/anton/git/ej/finviz_etf_uni.RData')




rebal_dates = as.Date(index(apply.quarterly(D$h[,1],FUN=length)))
rebal_dates = as.Date(index(apply.monthly(D$h[,1],FUN=length)))
# u=U; h=D$h; screen_func=screen_momentum; screen_params=list(N=5); vc_params=list(window=20, level=0.05, max_weight=2); weights=array(1/screen_params$N, screen_params$N)
IDX = build_index(U, H, rebal_dates, screen_func=screen_momentum, screen_params=list(N=5), vc_params=list(window=20, level=0.05, max_weight=2), weights=array(0.2,5))
plot(IDX)








t1=foreach(x=paste0('test_funds',0:9,'1.csv'))%do%fread(paste0('/home/anton/',x),header=FALSE)
t2=foreach(x=paste0('test_funds',0:9,'2.csv'))%do%fread(paste0('/home/anton/',x),header=FALSE)
t3=foreach(x=paste0('test_funds',0:9,'3.csv'))%do%fread(paste0('/home/anton/',x),header=FALSE)
t = rbindlist(foreach(i=1:length(t1))%do%cbind(t1[[i]], t3[[i]]))
colnames(t) = c('ticker','name','asset_class','strategy','region','geography','category','focus','niche','inverse','leveraged',
                'etn','underlying_index','provider','selection_criteria','weighting_scheme','actove_per_sec')
match(u$ticker, t$ticker)
match(t$ticker, u$ticker)

pet = foreach(x=t$ticker,.errorhandling='pass')%do%{ Sys.sleep(1.1); print(i); i=i+1; get(getSymbols(x))[,paste0(x,'.Adjusted')] }
save(pet, file='/home/anton/git/ej/pet_yhoo.RData')

pet=foreach(

t = get(load('/home/aslepnev/git/ej/etf_com_universe.RData'))
pet = get(load('/home/aslepnev/git/ej/etf_com_hist.RData'))
D = get(load('/home/aslepnev/git/ej/etf_com_DATA.RData'))








u = fread('/home/aslepnev/git/ej2/etf_uni_sacha.csv')
colnames(u) = c('ticker', 'name', 'mcap', 'mcap_focus2', 'style', 'geo_focus', 'geo_focus2', 'ind_focus', 'mcap_focus', 'ind_group', 'industry')
u$ticker = as.character(t(as.data.table(strsplit(u$ticker, ' ')))[, 1])

i=1
h = foreach(x=u$ticker,.errorhandling='pass')%do%{ Sys.sleep(1.1); print(i); i=i+1; get(getSymbols(x))[,paste0(x,'.Adjusted')] }
save(h, file='/home/aslepnev/webhub/sacha_etf_yhoo_raw.RData')
h1 = foreach(x=h[5<foreach(x=h,.combine=c)%do%length(x)],.combine=cbind)%do%x
colnames(h1) = gsub('[.]Adjusted', '', colnames(h1))
u = u[ticker%in%colnames(h1), ]
D = list(u=u, h=h1[,u$ticker])  # 'u' and 'h' match
save(D, file='/home/aslepnev/webhub/sacha_etf_yhoo.RData')











u = fread('/home/aslepnev/git/ej/grish_uni.csv')
u$ticker = as.character(t(as.data.table(strsplit(u$ticker, ' ')))[, 1])






h = fread('/home/aslepnev/git/ej/grish_asia2.csv')
h = h[2000:nrow(h),]
for(i in 1:nrow(h)){
    if(i%%100==0) print(i)
    x = strsplit(h[i, dt], '/')[[1]]
    if(as.numeric(x[3]) < 1000) x[3] = as.character(2000+as.numeric(x[3]))
    h$dt[i] = paste(x[c(3,1,2)], collapse='-')
}
for(col in colnames(h)[-1]) {
    x= as.matrix(h[, col, with=FALSE])
    x[x=='#N/A N/A'] = NA
    
    h[, (col):=as.numeric(x)]
}
h1 = xts(h[, -1, with=FALSE], order.by=as.Date(h[,dt]))
u = fread('/home/aslepnev/git/ej/grish_uni.csv')[ticker%in%colnames(h1),]
D = list(u=u, h=h1[,u$ticker])  # 'u' and 'h' match
save(D, file='/home/aslepnev/webhub/grish_asia.RData')




#library(xlsx)
library(data.table)
library(foreach)
library(quantmod)
#u = read.xlsx('/home/aslepnev/git/ej/grish_uni_2011.xlsx', '2018')
u = data.table(as.matrix(u))
u$ticker = as.character(t(as.data.table(strsplit(u$ticker, ' ')))[, 1])
save(u, file="/home/aslepnev/webhub/grish_uni_2018.RData")
u = get(load("/home/aslepnev/webhub/grish_uni_2018.RData"))
p = get(load("/home/aslepnev/webhub/zacks_data.RData"))
sum(u$ticker%in%colnames(p$h))
sum(!u$ticker%in%colnames(p$h))
i=1; grishe = foreach(x=u$ticker,.errorhandling='pass')%do%{ Sys.sleep(1.1); print(i); i=i+1; get(getSymbols(x))[,paste0(x,'.Adjusted')] }

h1 = foreach(x=grishe[5<foreach(x=grishe,.combine=c)%do%length(x)],.combine=cbind)%do%x








#p = get(load("/home/aslepnev/webhub/zacks_data.RData"))
u = foreach(y = 2009:2018, .combine=rbind)%do%{
    u = fread(paste0('/home/aslepnev/webhub/grish_uni_2011_', y, '.csv'))
    u$dt = as.Date(paste(y,12,31,sep='-'))
    u$ticker = as.character(t(as.data.table(strsplit(gsub('/', '', u$ticker), ' ')))[, 1])
    u
}
utickers = u[, unique(ticker)]
u$sec_id = (1:length(utickers))[match(u$ticker, utickers)]
i=1; grishe = foreach(x=utickers,.errorhandling='pass')%do%{ Sys.sleep(1.1); print(i); i=i+1; get(getSymbols(x))[,paste0(x,'.Adjusted')] }
save(grishe, file='/home/aslepnev/webhub/grish_uni_2011_p.RData')
mtickers = utickers[sapply(grishe, FUN=length)<5]
muni = u[ticker%in%mtickers, ][order(mcap, decreasing=TRUE), ][, .(ticker, name, country_code)]
muni[, .N, by='country_code']

utickers2 == muni[, unique(ticker)]
i=1; grishe2 = foreach(x=utickers2,.errorhandling='pass')%do%{ Sys.sleep(1.1); print(i); i=i+1; get(getSymbols(x))[,paste0(x,'.Adjusted')] }
save(muni, file='/home/aslepnev/webhub/muni.RData')

muni = muni[,head(.SD, 1), by=c('ticker','country_code')]
#muni[country_code=='CH' & nchar(ticker)==3, ticker:=paste0('0', ticker, '.HK')]
#muni[country_code=='CH' & nchar(ticker)==4, ticker:=paste0(ticker, '.HK')]
#muni[country_code=='CH' & nchar(ticker)>4, ticker:=paste0(ticker, '.SS')]
#muni[country_code=='JN', ticker:=paste0(ticker, '.T')]
#muni[country_code=='SZ', ticker:=paste0(ticker, '.VX')]
#muni[country_code=='BZ', ticker:=paste0(ticker, '.SA')]
#muni[country_code=='FI', ticker:=paste0(ticker, '.HE')]
#muni[country_code=='IT', ticker:=paste0(ticker, '.MI')]
#muni[country_code=='GE', ticker:=paste0(ticker, '.DE')]
#muni[country_code=='NE', ticker:=paste0(ticker, '.AS')]
#muni[country_code=='HK' & nchar(ticker)==3, ticker:=paste0('0', ticker, '.HK')]
#muni[country_code=='HK' & nchar(ticker)==4, ticker:=paste0(ticker, '.HK')]
#muni[country_code=='HK' & nchar(ticker)==1, ticker:=paste0('000', ticker, '.HK')]
#muni[country_code=='HK' & nchar(ticker)==2, ticker:=paste0('00', ticker, '.HK')]
#muni[country_code=='GB', ticker:=paste0(ticker, '.L')]
#muni[country_code=='SK' & nchar(ticker)==6, ticker:=paste0(ticker, '.KS')]
#muni[country_code=='SK' & nchar(ticker)==5, ticker:=paste0('0', ticker, '.KS')]
#muni[country_code=='SK' & nchar(ticker)==3, ticker:=paste0('000', ticker, '.KS')]
#muni[country_code=='SK' & nchar(ticker)==4, ticker:=paste0('00', ticker, '.KS')]
#muni[country_code=='SW', ticker:=paste0(ticker, '.ST')]
#muni[country_code=='SP', ticker:=paste0(ticker, '.MC')]
#muni[country_code=='TA', ticker:=paste0(ticker, '.TW')]
#muni[grep('.HK.SS', ticker), ticker:=gsub('.HK.SS', '.HK', ticker)]

fwrite(muni[,head(.SD, 1), by=c('ticker','country_code')], file='/home/aslepnev/webhub/muni.csv')




currencies = as.matrix(c('CH', 'HKD',
                         'HK', 'HKD',
                         '', '',
                         '', '',
                         '', '',
                         '', '',
                         '', '',
                         '', '',
                         ))




for(i in 1:100000){
    print(i)
    sectors1 = sample(sectors, n_sectors)
    idx = c()
    for(j in sectors1) idx=c(idx, sample(which(u1$SECTOR==j), n_stocks))
    h1 = h[, idx]
    h1 = exp(tail(h1[rowSums(!is.na(h1)) == ncol(h1),], 500))-1
    if(nrow(h1) == 500){
        h1 = exp(cumsum(log(1 + rowSums(h1)/ncol(h1))))
        x[[length(x)+1]] = list(sectors=sectors1, tickers=idx, sigma=sd(diff(log(h1)))*sqrt(250), cr=cor(1:length(h1),h1))
    }
}

sigmas=c(); crs=c()
for(i in 1:length(x)) {
    sigmas[i]=x[[i]]$sigma
    crs[i]=x[[i]]$cr
}
y = data.table(sigma=sigmas, cr=crs)
#plot(y[cr>0.98,])
idx = x[[which(y$sigma==y[cr>0.98, min(sigma)])]]$tickers
h1 = h[, idx]
h1 = exp(tail(h1[rowSums(!is.na(h1)) == ncol(h1),], 500))-1
h1 = exp(cumsum(log(1 + rowSums(h1)/ncol(h1))))
plot(h1)


u1 = u[sector=='Information Technology' & dt>='2012-01-01' & country_code=='US', ][order(mcap, decreasing=TRUE), ][, head(.SD, 30), by='dt']
u1[name=='SAMSUNG ELECTRONICS CO LTD', ticker:='005930.KS']
which(utickers=='005930')
grishe[[76]] = get(getSymbols('005930.KS'))[,paste0('005930.KS','.Adjusted')]
p1 = foreach(x=grishe[sapply(grishe, FUN=length)>5],.combine=cbind)%do%x
colnames(p1) = foreach(x=grishe[sapply(grishe, FUN=length)>5],.combine=c)%do%colnames(x)[1]
colnames(p1) = gsub('[.]Adjusted', '', colnames(p1))
u1 = u1[ticker%in%colnames(p1), ]
p1 = p1[, unique(u1$ticker)]
D = list(u=u1, h=p1)  # 'u' and 'h' match
save(D, file='/home/aslepnev/git/ej/it_top10_uni.RData')




source('/home/aslepnev/git/ej/strindexlib.R')
h = fread('/home/aslepnev/webhub/uber_hist1.csv')
ticker_cols = seq(1,ncol(h)-1,by=2)
price_cols = seq(2,ncol(h),by=2)
ts = foreach(i=1:length(price_cols))%do%{
    if(i%%50==0) print(i)
    x = h[, c(ticker_cols[i], price_cols[i]), with=FALSE]
    colnames(x) = c('date', 'price')
    x = x[!is.na(price) & !is.na(date), ][price!='#N/A N/A' & price!='' & date!='', ]
    x = xts(as.numeric(x$price), order.by=as.Date(as.character(x$date), '%m/%d/%Y'))
    colnames(x) = paste0('col_', i)
    x
}

tss = foreach(i=0:(length(ts)%/%200), .combine='merge.xts')%dopar%{
    foreach(j=(i*200+1):min(i*200+200, length(ts)), .combine='merge.xts')%do%{ if(j%%50==0) { print(j) }; ts[[j]] }
}
col_idx = as.numeric(foreach(x=colnames(tss), .combine=c)%do%strsplit(x,'_')[[1]][2])
colnames(tss) = colnames(h)[ticker_cols[col_idx]]
save(tss, file='/home/aslepnev/webhub/uber_hist.RData')  # Original currencies!!

u = foreach(y = 2009:2018, .combine=rbind)%do%{
    u = fread(paste0('/home/aslepnev/webhub/grish_uni_2011_', y, '.csv'))
    u$dt = as.Date(paste(y,10,31,sep='-'))
    u
}
undated_u = u[, tail(.SD, 1), by='ticker'][, .(ticker, name, mcap, sector, industry, country_code, country_name)]
save(undated_u, file='/home/aslepnev/webhub/grish_2011_undated.RData')
save(u, file='/home/aslepnev/webhub/grish_2011_dated.RData')

uu = undated_u
d = pre_screen(list(h=tss), undated_u, smart=TRUE)
save(d, file='/home/aslepnev/webhub/grish_2011_undated_uni.RData')

fxx = tss[, grep(' Curncy', colnames(tss)), with=FALSE]
save(fxx, file='/home/aslepnev/webhub/fxx.RData')






-- static csv-s

source('/home/aslepnev/git/ej/strindexlib.R')
f1 = fread('/home/aslepnev/webhub/funds_static.csv')
f1$sticker = as.character(t(as.data.table(strsplit(f1$TICKER, ' ')))[, 1])
colnames(f1) = gsub(' ', '_', tolower(colnames(f1)))
a = get(load('/home/aslepnev/webhub/sacha_etf_yhoo.RData'))
f2 = a$u[f1, on=.(ticker=sticker)][!is.na(name), .(ticker=i.ticker, name, mcap, assets, fund_group, fund_subgroup, currency, style, type, industry, ind_group, ind_focus, geo_focus, geo_focus2, mcap_focus, mcap_focus2)]

e1 = fread('/home/aslepnev/webhub/equity_data.csv')
colnames(e1) = c('ticker', 'country_code', 'sector', 'gics_industry', 'gics_industry_group', 'currency', 'region', 'country_name', 'name', 'industry_group', 'industry_subgroup')

tss = get(load('/home/aslepnev/webhub/uber_hist.RData'))
fxx = get(load('/home/aslepnev/webhub/fxx.RData'))
D = list(p=tss, h=h_to_log(tss), fxx=fxx, libors=libors, equity=e1, etf=f1)


save(d, file='/home/aslepnev/webhub/uber_uni_p.RData')

save(d, file='/home/aslepnev/webhub/uber_uni_h.RData')

save(d, file='/home/aslepnev/webhub/uber_uni_currency.RData')

save(d, file='/home/aslepnev/webhub/uber_uni_libors.RData')

save(e1, file='/home/aslepnev/webhub/uber_uni_equity.RData')

save(f2, file='/home/aslepnev/webhub/uber_uni_etf.RData')



-- bbg progruz

em1 = foreach(f = list.files('/home/aslepnev/webhub/mcaps', full.names=TRUE), .combine=rbind)%dopar%{  # f = list.files('/home/aslepnev/webhub/mcaps', full.names=TRUE)[1]
    x = fread(f)
    colnames(x) = c('ticker', 'name', 'pe', 'div', 'peg', 'de', 'pb', 'currency', 'mcap')
    x[, dt:=as.Date(strsplit(tail(strsplit(f, '/')[[1]], 1), '\\.')[[1]][1], format='%Y%m%d')]
    x
}
save(em1, file='/home/aslepnev/webhub/uber_uni_equity_metrics.RData')



