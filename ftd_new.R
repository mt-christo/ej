# source('rapache_eval1.R')
library(credule)
library(bindata)
library(foreach)

source('ftd_func.R')

y_tod = c(0.04,0.07)
y_sigmas = c(0.005,0.01)
y_as = c(0.15,0.1)
rfr_in = 0.01
spread_years_in = c(5,5)
basket_years_in = 5
rec_rate_in = 0.4

time_step = 0.5
n_steps = basket_years_in/time_step

# Building LOG IR and PD trees
itrees = list()
ir_steps = y_sigmas * sqrt(3*time_step)
for(i in 1:length(y_tod)){
    itree = list()
    itree[[1]] = y_tod[i]
    for(j in 2:n_steps)
        if(j*ir_steps[i] < 0.00184/(y_as[i]*time_step))
            itree[[j]] = itree[[j-1]][1] + seq(ir_steps[i],len=2*j-1,by=-ir_steps[i]) else
            itree[[j]] = itree[[j-1]]

    itrees[[i]] = itree
}

ptrees = foreach(i1=1:length(itrees))%do%{ foreach(i2=1:length(itrees[[i1]]))%do%{ 
    foreach(y=itrees[[i1]][[i2]],.combine=c)%do%imp_pd(y - rfr_in, rfr_in, spread_years_in[i], rec_rate_in)  } }



library(mnormt); library(xts); library(foreach); library(foreach)
cm = function(n, x) { diag(n) + x*(1-diag(n)) }
N = 2; s_d = 100
r=rmnorm(1000000,rep(0,N),s_d*cm(N,0.01)*s_d)
a=rollapply(1:nrow(r),width=1000,by=1000,FUN=function(i){ cor(r[i,])[1,2] })
sd(a)
plot(a)

k=1:N
s1 = foreach(j = seq(0.1,10,len=50),.combine=c)%dopar%{
  r=rmnorm(1000000,rep(0,N),j*k*cm(N,0.1))
  a=rollapply(1:nrow(r),width=500,by=1000,FUN=function(i){ cor(r[i,])[1,2] })
  sd(a)
}

s2 = foreach(j = seq(0.01,0.9,len=50),.combine=c)%dopar%{
  r=rmnorm(1000000,rep(0,N),cm(N,j))
  a=rollapply(1:nrow(r),width=500,by=1000,FUN=function(i){ cor(r[i,])[1,2] })
  sd(a)
}

plot(s1,ylim=c(0,0.07))
lines(s2)



x = get(load('wo_elements.RData'))
cor(x[,c('MU.US.Equity','YNDX.US.Equity','UCG.IM.Equity')])
foreach(j=c('MU.US.Equity','YNDX.US.Equity','UCG.IM.Equity'),.combine=c)%do%
    sd(diff(log(x[index(x)>'2017-01-01',j]))[-1])

sd(diff(log(x[index(x)>'2017-01-01',20]))[-1])

colnames(x) = gsub(' Equity','',gsub('\\.',' ',colnames(x)))
save(x,file='wo_elements.RData')

plot(foreach(i=1:300,.combine=c)%do%bond.price(as.Date('2017-01-01')+i, '2018-01-01', 0.01, 1, 0.03, '30/360'))



N = 360
cormat = function(x,n) { diag(n) + x*(1-diag(n)) }

#sigm=c(0.3,0.3);rfr=0.01;corr0=0.01;r0=c(0.06,0.1);k=0.1;pds=c(0.05,0.06);liq_sp=1
pd_mc = function(sigm,rfr,corr0,r0,k,pds){   #liq_sp){
    n = length(sigm)
    sf = foreach(i=1:n,.combine=c)%do%uniroot(function(x){ round(pd_mc_Single(sigm[i],rfr,r0[i],k,x),3) - pds[i] }, c(0.0001,0.4))$root
    cm0 = cormat(corr0,n)
    x = foreach(j1=1:25,.combine=c)%dopar%{ foreach(j=1:500,.combine=c)%do%{
        mr0 = r0

        dB = rmnorm(N,rep(0,n),cm0)
        dP = rmnorm(N,rep(0,n),cm0)
        for(i in 1:n)
            dP[,i] = pnorm(dP[,i])
        dP0 = rmnorm(N,rep(0,n),cm0) > 0
            
        r = matrix(0,N,n); r[1,] = r0; tmp_sigm = sigm
        for(i in 2:N) {
            tmp_sigm = ifelse(dP[i,] < sf, tmp_sigm*2, tmp_sigm)
            
            d_tmp = r[i-1,] + k*(mr0 - r[i-1,]) + tmp_sigm*sqrt(r[i-1,])*dB[i,]
            r[i,] = ifelse(d_tmp > rfr, d_tmp, rfr)

            tmp_sigm = ifelse(dP0[i,], sigm, tmp_sigm)
        }

        b = array(FALSE,N) # matrix(0,N,n);
        for(i in 1:n){
            b_tmp = bond.price(as.Date('2017-01-01'), '2018-01-01', 0.01, 1, r[,i], '30/360')
            b_tmp = b_tmp + (100-b_tmp)*(1:N)/N
            b = b | (b_tmp < 40)
        }

        if (max(b)) 1 else 0
    }}
  mean(x)
}

pd_mc(c(0.3,0.3),0.6,c(0.06,0.1),0.05,c(0.01,0.01))

uniroot(function(x){ pd_mc(x) - r0 }, c(0.2,0.4))


f=function(x) { round(pd_mc_Single(0.35,0.06,0.05,x,6),3) - 0.05 }
uniroot(f,c(0.01,0.6))$root

pd_mc_Single(sigm=0.1, rfr=0.01, r0=0.1, k=0.1, switch_factor=0.05)

pd_mc_Single = function(sigm,rfr,r0,k,switch_factor){   #liq_sp){
    x = foreach(j1=1:25,.combine=c)%dopar%{ foreach(j=1:1000,.combine=c)%do%{        
        #N = 360; sigm=0.01; rfr=0.01; corr0=0.6; r0=0.1; k=0.1; liq_sp=1; switch_factor = 0.01;
        mr0 = r0        
#        dB = rnorm(N,0,1)*(3^floor(log(runif(N))/log(switch_factor)))
        dB = rnorm(N,0,1)
        dP = pnorm(rnorm(N,0,1))
            
        r = array(0,N); r[1] = r0; 
        for(i in 2:N){
            if(dP[i] < switch_factor) mr0 = mr0*(1 + 10*sigm) #else if (dP[i] > 1 - switch_factor) mr0 = mr0/(1 + sigm)
            r[i] = max(rfr, r[i-1] + k*(mr0 - r[i-1]) + sigm*sqrt(r[i-1])*dB[i])
        }; #plot(r)

        b_tmp = bond.price(as.Date('2017-01-01'), '2018-01-01', 0.01, 1, r, '30/360')
        b = b_tmp + (100-b_tmp)*(1:N)/N < 40

        if (max(b)) 1 else 0
    }}
  mean(x)
}



plot(b+(100-b)*(1:N)/N)
plot(b)

    plot(b[,2])


osc_wght_arr = function(x, x0){
    res = t((t(x)-x0)/x0)
    res/(rowSums(abs(res))+0.0000001)
}

cm0 = cormat(0.9999,2)

N = 360
#sigm=0.05;corr0=0.6;r0=c(0.06,0.1);k=0.9;liq_sp=1
pd_mc = function(sigm,corr0,r0,k,liq_sp){
  cm0 = cormat(corr0,2)
  x = foreach(j1=1:30,.combine=c)%dopar%{ foreach(j=1:30,.combine=c)%do%{
  #x = foreach(j1=1:1,.combine=c)%dopar%{ foreach(j=1:1,.combine=c)%do%{
    dB = rmnorm(N,c(0,0),cm0)
    r = matrix(0,N,2); r[1,] = r0
    for(i in 2:N) {
        d_tmp = r[i-1,] + k*(mr0 - r[i-1,]) + sigm*sqrt(r[i-1,])*dB[i,]
        r[i,] = ifelse(d_tmp > 0, d_tmp, 0)
    }
cor(r)            
    b = matrix(0,N,ncol(r));
    for(i in 1:ncol(r)){
        b_tmp = bond.price(as.Date('2017-01-01'), '2018-01-01', 0.03, 1, r[,i], '30/360')
        b[,i] = b_tmp + (100-b_tmp)*(1:N)/N
    }

    w = r/rowSums(r) # osc_wght_arr(r,mr0)
    #plot(cumsum(rowSums(w[1:(N-1),]*(b[2:N,]-b[1:(N-1),])/b[1:(N-1),])))
    sum(rowSums(w[1:(N-1),]*(b[2:N,]-b[1:(N-1),])/b[1:(N-1),]))
    
  }}
  mean(x)
}

plot(foreach(i=seq(0.01,0.9,len=10),.combine=c)%do%pd_mc(0.01,0,c(0.06,0.1),i,1))

plot(b[,1])





# d = x_e; i=3
save_statics = function(conn,d,static_kind){
    fids = foreach(i=1:ncol(d),.combine=c)%do%as.numeric(dbGetQuery(conn, paste("Q_AddSecurityField '",d[1,i],"'",sep='')))
    secids = foreach(i=2:nrow(d),.combine=c)%do%as.numeric(dbGetQuery(conn, paste("Q_AddSecurity '",d[i,1],"'",sep='')))
    for(i in 1:length(secids)){
        q = "" 
        for(j in 1:length(fids))
            q = paste(q,"\nexec Q_AddSecurityStaticData ",secids[i],", ",fids[j],", '",gsub("'","",d[i+1,j]),"', ",static_kind,sep='')
        dbGetQuery(conn, q)
        print(i)
    }
}

library(xlsx)
library(RJDBC)
library(foreach)
drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver','sqljdbc4-2.0.jar')
conn = dbConnect(drv, 'jdbc:sqlserver://10.245.67.30;user=RUser;password=Qwerty12;database=QUIK_REPO')

x = read.csv('/var/share/hist.csv',sep=';',header=FALSE,stringsAsFactors=FALSE)
x_e = read.csv('/var/share/Equities.csv',sep=';',header=FALSE,stringsAsFactors=FALSE)
x_f = read.csv('/var/share/Funds.csv',sep=';',header=FALSE,stringsAsFactors=FALSE)
x_c = read.csv('/var/share/Currencies.csv',sep=';',header=FALSE,stringsAsFactors=FALSE)

dbSendQuery(conn, 'truncate table Q_Security_StaticData; select 1')
save_statics(conn,x_e,1)
save_statics(conn,x_f,2)
save_statics(conn,x_c,3)

secids = foreach(i=seq(1,ncol(x)-1,by=2),.combine=c)%do%as.numeric(dbGetQuery(conn, paste("Q_AddSecurity '",x[1,i],"'",sep='')))
col_indices = seq(1,ncol(x)-1,by=2)
for(i in 1:length(secids)){
    tryCatch({
      y = x[-(1:2),col_indices[i]+0:1]
      y = y[!is.na(y[,2]) & !is.na(y[,1]),]
      y[,1]=as.Date(y[,1],format='%d.%m.%Y')
      if(nrow(y) > 1) dbWriteTable(conn,'q_quotes_eod',cbind(a1=secids[i],a2=3,y),overwrite=FALSE,append=TRUE)
      print(secids[i])
    }, error=function(cond) {})
}

for(i in c(14,21,181,428,444,4214,4218)){
    tryCatch({
      y = x[-1,col_indices[i]+0:1]
      y = y[!is.na(y[,2]),]
      y[,1]=as.Date(y[,1],format='%d.%m.%Y')
      if(nrow(y) > 1) dbWriteTable(conn,'q_quotes_eod',cbind(a1=secids[i],a2=3,y),overwrite=FALSE,append=TRUE)
      print(secids[i])
    }, error=function(cond) {})
}



library(RJDBC)
drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver','sqljdbc4-2.0.jar')
conn = dbConnect(drv, 'jdbc:sqlserver://10.245.67.30\\quikodbc;user=QuikLimitsUser;password=Qwerty12;database=QUIK_REPO')
dbGetQuery(conn, 'select count(*) from q_quotes_eod')
secs = dbGetQuery(conn, 'select SecName,SecID from Q_Securities')
csv_names = gsub(' ','',gsub('\\.',' ',names(x)))
db_names = gsub('-','',gsub('\\*','',gsub(' ','',paste(ifelse(sapply(secs$SecName,function(y)!is.na(as.numeric(substr(y,1,1)))),'X',''),secs$SecName,sep=''))))
secids = secs$SecID[match(csv_names,db_names)]
secs$SecName[2633]

y = secs$SecName[2631]
names(x)[seq(1,ncol(x)-1,by=2)][4]


#x = readLines('eq_hist.csv')
#x[1] = substr(x[1],1,nchar(x[1])-1)
#length(strsplit(paste(x[1],';',sep=''),';')[[1]])
#length(strsplit(x[2],';')[[1]])
#writeLines(x,con=file('eq_hist.csv'))

x = read.table('eq_hist.csv',header=TRUE,sep=';',dec=',',row.names=NULL)
names(x)[1:5267] = names(x)[2:5268]
names(x)[2*(1:2634)] = paste(names(x)[2*(1:2634)-1],'Value',sep='')
names(x)[1:7]

for(i in seq(1,ncol(x)-1,by=2)){
    y = x[,i+0:1]
    y = y[!is.na(y[,2]),]
    y[,1]=as.Date(y[,1],format='%d.%m.%Y')    
    dbWriteTable(conn,'q_quotes_eod',cbind(secids[i],3,y),overwrite=FALSE,append=TRUE)
    print(csv_names[i])
}


#conn = create_conn()
#x = get_eod_quotes(c('AAPL US Equity','GOOGL US Equity'))
#x
x = dbGetQuery(create_conn(), 'select AVAT_Depth_Days, AVAT, AVAT_Datetime from Q_AVAT_Hist where SecID = 18 order by AVAT_Datetime')

x = dbGetQuery(create_conn(), 'select AVAT_Depth_Days, AVAT, case AVAT_Depth_Days when 0 then dateadd(n,-15,AVAT_Datetime) else AVAT_Datetime end as AVAT_Datetime
from Q_AVAT_Hist where SecID = 18 order by AVAT_Datetime')

x1 = x[x$AVAT_Depth_Days==0,]; x1 = as.xts(x1$AVAT,order.by=as.POSIXct(x1$AVAT_Datetime))
x2 = x[x$AVAT_Depth_Days==-1,]; x2 = as.xts(x2$AVAT,order.by=as.POSIXct(x2$AVAT_Datetime))
plot(x1,ylim=range(c(x1,x2)))
lines(x2,ylim=range(c(x1,x2)),lwd=3)
 


x = dbGetQuery(create_conn(), 'select AVAT_Depth_Days, AVAT, AVAT_Datetime from Q_AVAT_Hist where SecID = 18 order by AVAT_Datetime')
x1 = x[x$AVAT_Depth_Days==20,]; x1 = as.xts(x1$AVAT,order.by=as.POSIXct(x1$AVAT_Datetime))
plot(x1)

s = c('JNJ US Equity',
'SYK US Equity',
'DGX US Equity',
'MDT US Equity',
'BAX US Equity',
'TMO US Equity',
'HSIC US Equity',
'BDX US Equity',
'UNH US Equity',
'LLY US Equity',
'AMGN US Equity',
'GILD US Equity',
'AGN US Equity',
'CELG US Equity',
'BIIB US Equity')

r0 = get_eod_quotes(s,create_conn())
N = 30
r1 = exp(diff(log(tail(r0,N*252))))[-1,]-1
dc = diff(index(r1))
# r_in = r1; wgh = array(1/ncol(r0),ncol(r0)); vcl = 0.11
opt_weights = function(r_in, wgh, vcl){
    rb = foreach(i=1:length(wgh),.combine='+')%do%{ wgh[i] * r_in[,i] }
    expo = vcl / (rollapply(rb,20,sd)*sqrt(252))
    expo[expo>1.5] = 1.5
    rb = tail(rb,length(expo))
    1 + rb*expo - expo*rate...*tail(dc,length(expo))/360 - 0.02*tail(dc,length(expo)) 
    prod(rb+1)^(252/length(rb))
}


x = data.frame(a=1:4,b=3:6)
