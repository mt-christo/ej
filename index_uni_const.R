setwd('/home/aslepnev/git/ej')

if(FALSE){
source('strindexlib.R')
library(quantmod)


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


p = get(load('/home/anton/git/ej/zacks_yhoo.RData'))

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



a=get(load('/home/aslepnev/webhub/sacha_etf_yhoo.RData'))







u = fread('/home/aslepnev/git/ej/grish_uni.csv')
u$ticker = as.character(t(as.data.table(strsplit(u$ticker, ' ')))[, 1])



a





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

D = get(load('/home/aslepnev/webhub/zacks_data.RData'))
p = D$p
u = D$u

refresh_s = function() { gs_auth(token = '/home/aslepnev/git/ej/gdoc_doc.R'); return(gs_key('1y9KUgukyEvfjAVaDYCHPf1rkFn83q8LWS_0tybz2pIM')) }
DICTIONARY = as.data.table(gs_read(refresh_s(), 'wo-universe', range='A1:F1000', col_names=TRUE))
DICTIONARY$ticker = foreach(x=strsplit(DICTIONARY$ticker,' '), .combine=c)%do%x[1]
DICTIONARY$ticker = gsub('/', '', DICTIONARY$ticker)
D = list(u=DICTIONARY[ticker%in%colnames(p),], p=p[,DICTIONARY[ticker%in%colnames(p), ticker]])  # 'u' and 'h' match


p = foreach(x=DICTIONARY$ticker,.errorhandling='pass')%do%{ Sys.sleep(1.1); print(i); i=i+1; get(getSymbols(x))[,paste0(x,'.Adjusted')] }
p1 = foreach(x=p[5<foreach(x=p,.combine=c)%do%length(x)],.combine=cbind)%do%x
colnames(p1) = gsub('[.]Adjusted', '', colnames(p1))
u = DICTIONARY
D = list(u=u[ticker%in%colnames(p1),], p=p1[,u[ticker%in%colnames(p1), ticker]])  # 'u' and 'h' match
## ohh that Grish file is something


}
