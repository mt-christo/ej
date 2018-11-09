library(data.table)
library(xts)
library(corpcor)
library(doMC)
#library(plot3D)
library(rgl)
#library(evd)
source('/home/aslepnev/git/ej/stropt.R')
registerDoMC(cores=7)
DIVS = array(0, 5)
COUPON = 6.2
RFR = 0.01
TTM = 3
x=0; y=0
BARRIERS1 = c(100,90,70)
BARRIERS2 = c(100,100,100)
x1 = foreach(i=1:500)%dopar%{
    print(i)
    SIGMAS = 0.2*abs(runif(5))
    COR_MAT = make.positive.definite(abs(cor(foreach(i=1:5,.combine=cbind)%do%runif(4)))^3)
    list(cm=COR_MAT, cv=cor2cov(COR_MAT,SIGMAS), r=wo_calculate(TTM, BARRIERS1, SIGMAS, COR_MAT, RFR, DIVS, COUPON))
}
x2 = foreach(i=1:500)%dopar%{
    print(i)
    SIGMAS = 0.2*abs(runif(5))
    COR_MAT = make.positive.definite(abs(cor(foreach(i=1:5,.combine=cbind)%do%runif(4)))^3)
    list(cm=COR_MAT, cv=cor2cov(COR_MAT,SIGMAS), r=wo_calculate(TTM, BARRIERS2, SIGMAS, COR_MAT, RFR, DIVS, COUPON))
}

y1 = foreach(i=x1,.combine=rbind)%do%{ data.frame(m=mean(i$cm),s=sum(diag(i$cv)), r=i$r) }
y2 = foreach(i=x2,.combine=rbind)%do%{ data.frame(m=mean(i$cm),s=sum(diag(i$cv)), r=i$r) }

plot(y1$s,y1$r)
plot(y2$m,y2$r)




scatter3D(y$m,y$s,y$r)
#a1=y$r; a2=y$m; a3=y$s
#f = lm(a1 ~ a2+a3)
#summary(f)

plot(y$m,y$r)
cor(y$m*89-y$s*17,y$r)
plot(y$m*89-y$s*17,y$r)
plot(y$m*35-y$s*13,y$r)

cor(y1$s,y1$r)
cor(y2$m,y2$r)

sector_map = as.data.frame(t(matrix(c('Technology','tech',
'Communications','comm',
'Financial','fin',
'Consumer, Non-cyclical','cons',
'Energy','ener',
'Consumer, Cyclical','cons',
'Industrial','ind',
'Basic Materials','mat',
'Utilities','util',
'Diversified','div'), nrow=2)))
colnames(sector_map) = c('name', 'code')




q1 = get(load('uniprc1.RData'))    
q2 = get(load('uniprc2.RData'))
q = diff(log(merge.xts(q1,q2)))
save(q, file='uniprc.RData')
u = data.table(get(load('uni.RData')))

N = 20; K = 5
IDX = 2001:(2000+N)
cls = colnames(q)[IDX]
stocks = u[IDX,2]
cmb = t(combn(cls,K))

res = array(0,nrow(cmb))
for(i in 1:nrow(cmb)){
    print(i)
    r = q[,cmb[i,]]
    r = r[rowSums(is.na(r))==0,]
    r = cumsum(rowSums(exp(r)-1)/K)
    res[i] = cor(r,1:length(r))
}

plot(r)
