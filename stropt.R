library(mnormt)
library(data.table)
library(foreach)
library(xts)
library(mvtnorm)
library(corpcor)

FIX_SP_MULT <<- 1.0
FIX_SP_ADD_BC <<- 4.0
FIX_SP_ADD_WO <<- 3.5

cor2cov = function(corMat, varVec) {
  # test the input
  if (!is.matrix(corMat)) stop("'corMat must be a matrix")
  n = nrow(corMat)
  if (ncol(corMat) != n) stop("'corMat' must be square")
  if (mode(corMat) != "numeric") stop("'corMat must be numeric")
  if (mode(varVec) != "numeric") stop("'varVec must be numeric")
  if (!is.null(dim(varVec))) {
    if (length(dim(varVec)) != 2) stop("'varVec' should be a vector")
    if (any(dim(varVec)==1)) stop("'varVec' cannot be a matrix")
    varVec = as.numeric(varVec) # convert row or col matrix to a vector
  }
  if (!all(diag(corMat) == 1)) stop("correlation matrices have 1 on the diagonal")
  if (any(corMat < -1 | corMat > +1)) 
    stop("correlations must be between -1 and 1")
  if (any(varVec <= 0)) stop("variances must be non-negative")
  if (length(varVec) != n) stop("length of 'varMat' does not match 'corMat' size")

  # Compute the covariance
  sdMat = diag(sqrt(varVec))
  rtn = sdMat %*% corMat %*% t(sdMat)
  if (det(rtn)<=0) warning("covariance matrix is not positive definite")
  return(rtn)
}

corMine = function(ts) {
    foreach(i=1:ncol(ts),.combine=cbind)%do%{ 
        foreach(j=1:ncol(ts),.combine=c)%do%{
            idx = !(is.na(ts[,i]) | is.na(ts[,j]))
            cor(diff(ts[idx,i])[-1],diff(ts[idx,j])[-1])
        } }
}


get_rfr = function(ttm){
    if(ttm==1) 0.01 else if(ttm==2) 0.012 else if(ttm==3) 0.015 else if(ttm==4) 0.018 else if(ttm==5) 0.021 else if(ttm==6) 0.025 else 0.028
}

CorrectCM = function(CM){
    n <- dim(var(CM))[1L]
    E <- eigen(CM)
    CM1 <- E$vectors %*% tcrossprod(diag(pmax(E$values, 0), n), E$vectors)
    Balance <- diag(1/sqrt(diag(CM1)))
    CM2 <- Balance %*% CM1 %*% Balance  
    return(CM2)
}

wo_calculate_an = function(ttm, barriers, sigmas, cor_mat, rfr, dys, coupon){
    integro = function(q_in, cor_mat_in, sigmas_in){
        as.numeric(pmvnorm(q_in/sigmas_in, rep(100.0,length(q_in)), corr=make.positive.definite(cor_mat_in), maxpts=5000, abseps=0.0001))
    }
    
    ps = foreach(i=1:ttm,.combine=c)%do%integro(-(rfr - dys - 0.5*sigmas^2)*i + log(barriers[i]*0.01), cor_mat, sigmas*sqrt(i))
    coupon*sum(exp(-rfr*(1:ttm)) * (c(0, cumprod((1-ps)[-ttm])*(1:(ttm-1))*ps[-1]) + ps))
}


wo_calculate = function(ttm, barriers, sigmas, cor_mat, rfr, dys, coupon){
        mc_paths = 300000
    
        d = foreach(i=1:ttm)%do%{ rmnorm(mc_paths,array(0,length(sigmas)),make.positive.definite(cor_mat)) }
        drfs = rfr - dys - 0.5*sigmas^2
        if(ttm > 1) for(i in 2:ttm) d[[i]] = d[[i]] + d[[i-1]]
        if(length(sigmas) > 1)
            wo = foreach(j=1:length(d))%do%{ x = j*drfs[1]+sigmas[1]*d[[j]][,1]; for(i in 2:ncol(d[[j]])) x = ifelse(x >  j*drfs[i]+sigmas[i]*d[[j]][,i],  j*drfs[i]+sigmas[i]*d[[j]][,i], x); x }
#            wo = foreach(y=d)%do%{ x = drfs[1]+sigmas[1]*y[,1]; for(i in 2:ncol(y)) x = ifelse(x >  drfs[i]+sigmas[i]*y[,i],  drfs[i]+sigmas[i]*y[,i], x); x }
        else
            wo = foreach(y=d)%do%{ x = drfs[1]+sigmas[1]*y; x }
        #wo = foreach(y=d)%dopar%{ x = sigmas[1]*y[,1]; for(i in 2:ncol(y)) x = ifelse(x >  sigmas[i]*y[,i],  sigmas[i]*y[,i], x); x }
    	
	#r = array(0,mc_paths)
	res = array(0,0)
	for(i in ttm:1){
		#r = r + rnorm(length(r),rfr-0.5*sigma^2,sigma)
		#r = r + rnorm(length(r),0,sigma)
                r = wo[[i]]
                idx = exp(r) > barriers[i]*0.01
                res = c(res, array(i*coupon*exp(-i*rfr),sum(idx)))
##                res = c(res, array(coupon*exp(-i*rfr),sum(idx)))
            
                for(j in 1:length(wo))
                    wo[[j]] = wo[[j]][!idx]
	}

        res = c(res, array(0,mc_paths - length(res)))
    
	mean(res)
##sum(res)/mc_paths
}

wo_calculator_web = function(params_file, quotes_file){
    params = fread('~/git/ej/wo_params.csv')
    q = fread('~/git/ej/wo_quotes.csv')
    STOCKS = unique(q$ticker)

    BARRIERS = as.numeric(strsplit(params[param=='strikes',value],'-')[[1]])
    TTM = length(BARRIERS)
    COUPON = as.numeric(params[param=='coupon',value])
    STOCKS = unique(q$ticker)
    
    prices = log(1 + foreach(t=unique(q$ticker),.combine='merge.xts')%do%xts(q[ticker==t,val],order.by=as.Date(q[ticker==t,dt])))[-1]
    names(prices) = STOCKS
    COR_MAT = foreach(wnd=c(50,100,200,300,400,500),.combine=pmax)%do%corMine(tail(prices,wnd))
    my_cor = function(n, x) { diag(n) + (1-diag(n))*x }
    COR_MAT_AVG = my_cor(length(STOCKS),mean(COR_MAT))
    COR_MAT = (COR_MAT+COR_MAT_AVG)/2

    DIVS_PROJ = 0 # 0.01*as.numeric(get_secs_num_field(STOCKS,'PROJ 12M DIV',conn))
    DIVS_FACT = 0 # 0.01*as.numeric(get_secs_num_field(STOCKS,'FACT 12M DIV',conn))
    DIVS = ifelse(!is.na(DIVS_PROJ),DIVS_PROJ,ifelse(!is.na(DIVS_FACT),DIVS_FACT,0))
    #DIVS = array(0,length(STOCKS))

    SIGMAS = foreach(i=1:ncol(prices),.combine=c)%do%(sqrt(250)*sd(tail(diff(prices[!is.na(prices[,i]),i]),250)))
    RFR = get_rfr(TTM)

    return(round(wo_calculate(TTM, BARRIERS, SIGMAS, COR_MAT, RFR, DIVS, COUPON),1)*FIX_SP_MULT + FIX_SP_ADD_WO)
}


