setwd('/home/aslepnev/git/ej')
u = get(load('uni.RData'))
DATA = get(load('uniprc.RData'))
UNI <<- data.table(u, row.names=TRUE)

# sec_ids=c(100,200,300); sec_ws=c(0.5,0.3,0.2); start_dt='2015-01-01'; end_dt='9999-12-31'
b_rt = function(sec_ids, sec_ws, start_dt, end_dt){
    r = na.locf(DATA[index(DATA)>=start_dt & index(DATA)<=end_dt, sec_ids])
    r = diff(log(r))[-1,]
    b = as.xts(rowSums(t(sec_ws*t(r))))
    sd(apply.monthly(b, FUN=function(x){ exp(sum(x)) }))
}
