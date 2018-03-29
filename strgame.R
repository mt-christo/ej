library(corpcor)
library(doMC)
library(plot3D)
library(rgl)
library(evd)
registerDoMC(cores=3)
DIVS = array(0, 5)
COUPON = 6.2
RFR = 0.01
TTM = 3
BARRIERS = c(100,90,70)
x = foreach(i=1:100)%dopar%{
    print(i)
    SIGMAS = 0.2*abs(runif(5))
    COR_MAT = make.positive.definite(abs(cor(foreach(i=1:5,.combine=cbind)%do%runif(4)))^3)
    list(cor2cov(COR_MAT,SIGMAS), wo_calculate(TTM, BARRIERS, SIGMAS, COR_MAT, RFR, DIVS, COUPON))
#    list(COR_MAT, wo_calculate(TTM, BARRIERS, SIGMAS, COR_MAT, RFR, DIVS, COUPON))
}

y = foreach(i=x,.combine=rbind)%do%{ m=i[[1]]; data.frame(m=sum(diag(m)),r=i[[2]]) }
y = foreach(i=x,.combine=rbind)%do%{ m=i[[1]]; data.frame(m=sum(diag(m)),r=i[[2]]) }
y = foreach(i=x,.combine=rbind)%do%{ m=i[[1]]; data.frame(m=mean(cov2cor(m)),r=i[[2]]) }
y = foreach(i=x,.combine=rbind)%do%{ m=i[[1]]; data.frame(m=mean(m),s=sum(diag(m)), r=i[[2]]) }
plot3d(y$m,y$s,y$r)
a1=y$r; a2=y$m; a3=y$s
f = lm(a1 ~ a2+a3)
summary(f)
plot(y$s,y$r)
cor(y$m*89-y$s*17,y$r)
plot(y$m*89-y$s*17,y$r)
plot(y$m*35-y$s*13,y$r)
cor(y$s,y$r)
cor(y$m,y$r)


    
