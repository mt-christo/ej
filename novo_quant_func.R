library(mnormt)
library(data.table)
library(foreach)
library(xts)
#library(mvtnorm)
library(corpcor)

FIX_SP_MULT <<- 1.0
FIX_SP_ADD_BC <<- 4.0
FIX_SP_ADD_WO <<- 3.5

# ttm=TTM; barriers=BARRIERS; sigmas=SIGMAS[cmb[,i]]; cor_mat=COR_MAT_ALL[cmb[,i], cmb[,i]]; rfr=RFR; dys=DIVS[cmb[,i]]; coupon=COUPON
wo_calculate_an = function(ttm, barriers, sigmas, cor_mat, rfr, dys, coupon){
    # i=1; q_in=-(rfr - dys - 0.5*sigmas^2)*i + log(barriers[i]*0.01); cor_mat_in=cor_mat; sigmas_in=sigmas*sqrt(i)    
    integro = function(q_in, cor_mat_in, sigmas_in){
        as.numeric(pmvnorm(q_in/sigmas_in, rep(100.0,length(q_in)), corr=make.positive.definite(cor_mat_in), maxpts=5000, abseps=0.0001))
    }

    ps = foreach(i=1:ttm,.combine=c)%do%integro(-(rfr - dys - 0.5*sigmas^2)*i + log(barriers[i]*0.01), cor_mat, sigmas*sqrt(i))
    return( coupon*sum(exp(-rfr*(1:ttm)) * (c(0, cumprod((1-ps)[-ttm])*(1:(ttm-1))*ps[-1]) + ps)) )
}
