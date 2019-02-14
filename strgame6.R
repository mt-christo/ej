library(data.table)
library(foreach)
#u = read.xlsx('/home/aslepnev/git/ej/grish_uni_2011.xlsx', '2018')
#u = data.table(as.matrix(u))
#u$ticker = as.character(t(as.data.table(strsplit(u$ticker, ' ')))[, 1])
#save(u, file="/home/aslepnev/webhub/grish_uni_2018.RData")
u = get(load("/home/aslepnev/webhub/grish_uni_2018.RData"))
p = get(load("/home/aslepnev/webhub/zacks_data.RData"))
#sum(u$ticker%in%colnames(p$h))
#sum(!u$ticker%in%colnames(p$h))

u = u[ticker%in%colnames(p$h), ]
h = p$h[, u$ticker]

source('/home/aslepnev/git/ej/strindexlib.R')
h1 = h_to_log(tail(h, 252))
D = list(u=u, h=h, cov60=cov(tail(h1, 60)), cov125=cov(tail(h1, 125)), cov252=cov(tail(h1, 252)) )  # 'u' and 'h' match
save(D, file='/home/aslepnev/webhub/grish_iter0.RData')



de = get(load('/home/aslepnev/webhub/sacha_etf_yhoo.RData'))
u = de$u; h = de$h
h1 = h_to_log(tail(h, 500))
D = list(u=u, h=h, cov60=cov(tail(h1, 60)), cov125=cov(tail(h1, 125)), cov250=cov(tail(h1, 250)) )
save(D, file='/home/aslepnev/webhub/sacha_etf_yhoo.RData')

