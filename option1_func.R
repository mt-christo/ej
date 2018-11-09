# source("rapache_eval1.R")
# source("option1_func.R")

FIX_SP_MULT <<- 1.0
FIX_SP_ADD_BC <<- 4.0
FIX_SP_ADD_WO <<- 3.5






# Black-Scholes Option Value
# Call value is returned in values[1], put in values[2]
blackscholes <- function(S, X, rf, dy, T, sigma) {
    values <- c(2)
 
    d1 <- (log(S/X)+(rf-dy+sigma^2/2)*T)/(sigma*sqrt(T))
    d2 <- d1 - sigma * sqrt(T)
 
    values[1] <- S*exp(-dy*T)*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)
    values[2] <- X*exp(-rf*T)*pnorm(-d2) - S*exp(-dy*T)*pnorm(-d1)
 
    values
}

# ts = pr_conv
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







phx_calculate = function(mc_paths, cor_mat, sigmas, dys, ttm_in, rfr, barrier, bar_type, coupon_in, cpn_carrier, cpn_type, cpn_bar_type, freq, pf_type){
    coupon = coupon_in/freq
    ttm = ttm_in*freq
    
    d = foreach(i=1:ttm)%dopar%{ rmnorm(mc_paths,array(0,length(sigmas)),cor_mat) }
    drfs = (rfr - dys - 0.5*sigmas^2)/freq    
    if(ttm > 1) for(i in 2:ttm) d[[i]] = d[[i]] + d[[i-1]]
    
    if(length(sigmas) > 1){
        wo = foreach(y=d)%dopar%{ x = drfs[1]+sigmas[1]*y[,1]; for(i in 2:ncol(y)) x = ifelse(x >  drfs[i]+sigmas[i]*y[,i],  drfs[i]+sigmas[i]*y[,i], x); x }
        pf = foreach(y=d)%dopar%{ x = drfs[1]+sigmas[1]*y[,1]; for(i in 2:ncol(y)) x = x + drfs[i]+sigmas[i]*y[,i]; x/ncol(y) }
    }
    else{
        wo = foreach(y=d)%dopar%{ x = drfs[1]+sigmas[1]*y; x }
    }
    

    
	#r = array(0,mc_paths)
	res = array(0,0)
	for(i in ttm:1){
		#r = r + rnorm(length(r),rfr-0.5*sigma^2,sigma)
		#r = r + rnorm(length(r),0,sigma)
                r = wo[[i]]
                idx = exp(r) > barriers[i]*0.01
                res = c(res, array(i*coupon*exp(-i*rfr),sum(idx)))
            
                for(j in 1:length(wo))
                    wo[[j]] = wo[[j]][!idx]
	}

        res = c(res, array(0,mc_paths - length(res)))
    
	mean(res)
}









plot_basket_web = function(){
    writeLines(gsub('NEWLINE','\n',GET$params), file('/home/slepnev/R/tmp_params_plot_web.txt'))
    p = read.csv(file='/home/slepnev/R/tmp_params_plot_web.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

    STOCKS = strsplit(p$V2[p$V1=='Tickers'],'@@')[[1]]
    prices = log(get_eod_quotes(STOCKS, create_conn()))
    w = array(1/length(STOCKS),length(STOCKS))
    
    x = apply.monthly(exp(cumsum(basket_returns(tail(prices,250*5),w))),mean)
    x = data.frame(date=index(x),value=as.numeric(x[,1]),stringsAsFactors=FALSE)
    cat(paste(x$date,collapse=';'))
    cat('@@')
    cat(paste(gsub('\\.',',',as.character(x$value)),collapse=';'))
}


plot_basket_web_daily_GET = function(){
    plot_basket_web_daily(GET$params)
}


plot_basket_web_daily_GET_Original = function(){
    plot_basket_web_daily(GET$params, 'Original')
}


plot_basket_web_daily_POST = function(){
    plot_basket_web_daily(POST$params)
}


plot_basket_web_daily = function(params, conv_type = 'CONVERT'){
    writeLines(gsub('NEWLINE','\n',params), file('/home/slepnev/R/tmp_params_plot_web_daily.txt'))
    p = read.csv(file='/home/slepnev/R/tmp_params_plot_web_daily.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

    STOCKS = strsplit(p$V2[p$V1=='Tickers'],'@@')[[1]]
#    prices = log(get_eod_quotes(STOCKS, create_conn()))
    prices = get_eod_quotes(STOCKS, create_conn(), 3, conv_type)
    w = array(1/length(STOCKS),length(STOCKS))
    
    x = tail(prices,250*5)
#    x = data.frame(date=index(x),value=as.numeric(x[,1]),stringsAsFactors=FALSE)
    if(dim(x)[2]==1){
        cat(paste(index(x),collapse=';'))
        cat('@@')
        cat(paste(gsub('\\.',',',as.character(na.locf(x))),collapse=';'))
    } else{
        x = as.data.frame(x)[order(index(x),decreasing=TRUE),]
        cat(paste(rownames(x),collapse=';'))
        for(i in 1:dim(x)[2]){
            cat('@@')
            cat(paste(gsub('\\.',',',as.character(na.locf(x[,i]))),collapse=';'))
        }
    }
}


plot_wo_web = function(){
    writeLines(gsub('NEWLINE','\n',GET$params), file('/home/slepnev/R/tmp_params_plot_wo_web.txt'))
    p = read.csv(file='/home/slepnev/R/tmp_params_plot_wo_web.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

    STOCKS = strsplit(p$V2[p$V1=='Tickers'],'@@')[[1]]
    prices = diff(na.locf(log(get_eod_quotes(STOCKS, create_conn()))))[-1,]
    
    #x = apply.monthly(exp(cumsum(basket_returns(tail(prices,250*5),w))),mean)
    x = apply.monthly(exp(cumsum(tail(prices,250*1))),mean)
    x[1,] = 1
    cat(paste(index(x),collapse=';'))
    for(i in 1:dim(x)[2]){
        cat('@@')
        cat(paste(gsub('\\.',',',as.character(x[,i])),collapse=';'))
    }
}


basket_returns = function(ts,wght){
    ts2 = ts
    for(i in 1:ncol(ts2))
        ts2 = ts2[!is.na(ts2[,i]),]    
    for(i in 1:ncol(ts2))
        ts2[,i] = ts2[,i]*wght[i]
    res = xts(c(0,rowSums(diff(ts2)[-1,])), order.by=index(ts2))
    res
}


basket_all_returns = function(ts){
    ts2 = ts
    for(i in 1:ncol(ts2))
        ts2 = ts2[!is.na(ts2[,i]),]
    
    ts2 = diff(ts2)[-1,]

    maxx = array(0,ncol(ts2))
    minn = array(0,ncol(ts2))
    for(i in 1:ncol(ts2)){
        ts2[,i] = cumsum(ts2[,i])
        maxx[i] = max(ts2[,i])
        minn[i] = min(ts2[,i])
    }

    ts2
    #cl = rainbow(ncol(ts2))
    #plot(ts2[,1],ylim=c(min(minn),max(maxx)),t='l')
    #for(i in 2:ncol(ts2))
    #    lines(ts2[,i],col=coll[i],t='l',lwd=2)
}









basketcall_calculator_web = function(){
    writeLines(gsub('NEWLINE','\n',POST$params), file('/home/slepnev/R/tmp_params_bc_web.txt'))
    p = read.csv(file='/home/slepnev/R/tmp_params_bc_web.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

    TTM = round(as.numeric(gsub('\\,','\\.',p[p[,1]=='Term',2])))
    STOCKS = strsplit(p$V2[p$V1=='Tickers'],'@@')[[1]]
    RFR = get_rfr(TTM)
    
    cat(gsub('\\.',',',as.character(round(basketcall_calculate_mc(TTM,STOCKS,RFR)*100*FIX_SP_MULT + FIX_SP_ADD_BC,1)))) # !!!
}


basketcall_calculator = function(){
    writeLines(gsub('NEWLINE','\n',POST$params), file('/home/slepnev/R/tmp_params_bc.txt'))
    p = read.csv(file='/home/slepnev/R/tmp_params_bc.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

    TTM = as.numeric(p[p[,1]=='TTM',2])
    STOCKS = p$V2[p$V1=='STOCK']
    RFR = as.numeric(p[p[,1]=='RFR',2])
    CURRENCY = as.character(p[p[,1]=='Currency',2])

    cat(basketcall_calculate(TTM,STOCKS,RFR,CURRENCY))
}


# ttm=TTM; stocks=STOCKS; rfr=RFR; quote_curr = 'USD'              quote_curr = CURRENCY;         
basketcall_calculate = function(ttm, stocks, rfr, quote_curr = 'USD'){
    conn = create_conn()
    pr = tail(log(get_eod_quotes_CURR(stocks, conn, quote_curr)),500)
    pr = pr[,foreach(i=1:ncol(pr),.combine=c)%do%(min(pr[,i], na.rm=TRUE)!=0 || max(pr[,i], na.rm=TRUE)!=0)]
    pr = pr[,-grep('convquote',names(pr))]

    COR_MAT = foreach(wnd=c(50,100,200,300,400,500),.combine=pmax)%do%corMine(tail(pr,wnd))
    SD = array(NA,ncol(pr))
    SD[grep('quote',names(pr))] = 0.01*as.numeric(get_secs_num_field(stocks,'12M ATM VOLATILITY',conn))
    SD[is.na(SD)] = foreach(i=which(is.na(SD)),.combine=c)%do%(sd(diff(pr[,i]),na.rm=TRUE)*sqrt(360))

    DIVS_PROJ = 0.01*as.numeric(get_secs_num_field(stocks,'PROJ 12M DIV',conn))
    DIVS_FACT = 0.01*as.numeric(get_secs_num_field(stocks,'FACT 12M DIV',conn))
    DIVS = 0.5*ifelse(!is.na(DIVS_PROJ),DIVS_PROJ,ifelse(!is.na(DIVS_FACT),DIVS_FACT,0))
    #DIVS = array(0,length(stocks))
    SD = if(length(SD)>1) (1+0.05*(ttm-1))*sqrt(sum(diag(SD) * COR_MAT * diag(SD)))/length(stocks) else SD
    
    BARRIERS = array(100,length(stocks))

    blackscholes(1,1,rfr,mean(DIVS),ttm,SD)[1]
}


# ttm=TTM; stocks=STOCKS; rfr=RFR; quote_curr = 'USD'              quote_curr = CURRENCY;         
basketcall_calculate_mc = function(ttm, stocks, rfr, quote_curr = 'USD'){
    conn = create_conn()
    pr = tail(log(get_eod_quotes_CURR(stocks, conn, quote_curr)),500)
    #  pr = pr[,foreach(i=1:ncol(pr),.combine=c)%do%(min(pr[,i], na.rm=TRUE)!=0 || max(pr[,i], na.rm=TRUE)!=0)]

    pr_conv = pr[,grep('convquote',names(pr))]
    COR_MAT = foreach(wnd=c(50,100,200,300,400,500),.combine=pmax)%do%corMine(tail(pr_conv,wnd))
    #  my_cm=function(n,x){ diag(n)+x*(1-diag(n)) };    COR_MAT = my_cm(ncol(COR_MAT),0.2)
    pr = pr[,-grep('convquote',names(pr))]
    
    #  SD = array(NA,ncol(pr))
    #  SD[grep('quote',names(pr))] = 0.01*as.numeric(get_secs_num_field(stocks,'12M ATM VOLATILITY',conn))
    #  SD[is.na(SD)] = foreach(i=which(is.na(SD)),.combine=c)%do%(sd(diff(pr[,i]),na.rm=TRUE)*sqrt(360))

    SD_IMPL = 0.01*as.numeric(get_secs_num_field(stocks,'12M ATM VOLATILITY',conn))
    SD = (1+0.05*(ttm-1))*foreach(i=1:length(stocks),.combine=c)%do%{
        scor_mat = apply(corMine(pr[,1:3+3*(i-1)]),1:2,function(x){ifelse(is.na(x),0,x)})
        diag(scor_mat) = 1.0
        scor_mat[2,] = -scor_mat[2,]
        scor_mat[,2] = -scor_mat[,2]
        sr = diff(pr[,1:3+3*(i-1)])
        sd0 = ifelse(!is.na(SD_IMPL[i]), SD_IMPL[i], sqrt(250)*sd(sr$quote[!is.na(sr$quote)]))
        sd1 = sqrt(250)*sd(sr$rate[!is.na(sr$rate)])
        sd2 = sqrt(250)*sd(sr$qrate[!is.na(sr$qrate)])
        sqrt(sum(diag(c(sd0,sd1,sd2))*scor_mat*diag(c(sd0,sd1,sd2))))
    }

    DIVS_PROJ = 0.01*as.numeric(get_secs_num_field(stocks,'PROJ 12M DIV',conn))
    DIVS_FACT = 0.01*as.numeric(get_secs_num_field(stocks,'FACT 12M DIV',conn))
    DIVS = ifelse(!is.na(DIVS_PROJ),DIVS_PROJ,ifelse(!is.na(DIVS_FACT),DIVS_FACT,0))
    if(length(DIVS) < length(stocks))
        DIVS = array(0, length(stocks))
    #DIVS = array(0,length(stocks))
    
    mc_paths = 250000
    d = rmnorm(mc_paths,array(0,length(SD)),COR_MAT)
    if(ttm > 1)
        for(i in 2:ttm)
            d = d + rmnorm(mc_paths,array(0,length(SD)),COR_MAT)
    
    drfs = rfr - DIVS - 0.5*SD^2
    r = exp(-ttm*rfr)*if(length(stocks)==1) { exp(drfs*ttm + SD*d) - 1 } else foreach(i = 1:ncol(d),.combine='+')%do%{ exp(drfs[i]*ttm + SD[i]*d[,i]) - 1 } / length(stocks)
    mean(ifelse(r<0,0,r))
}









# my_cor = function(n, x) { diag(n) + (1-diag(n))*x }
wo_calculator_web = function(){
    writeLines(gsub('NEWLINE','\n',POST$params), file('/home/slepnev/R/tmp_params_wo_web.txt'))
    p = read.csv(file='/home/slepnev/R/tmp_params_wo_web.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

    conn = create_conn()

    TTM = round(as.numeric(gsub('\\,','\\.',p[p[,1]=='Term',2])))
    COUPON = as.numeric(gsub(',','.',p[p[,1]=='Coupon',2]))

    STOCKS = strsplit(p$V2[p$V1=='Tickers'],'@@')[[1]]
    prices = log(get_eod_quotes(STOCKS, create_conn()))
    #COR_MAT = pmax(corMine(tail(prices,500)),corMine(tail(prices,250)),corMine(tail(prices,125)))
    COR_MAT = foreach(wnd=c(50,100,200,300,400,500),.combine=pmax)%do%corMine(tail(prices,wnd))
    my_cor = function(n, x) { diag(n) + (1-diag(n))*x }
    COR_MAT_AVG = my_cor(length(STOCKS),mean(COR_MAT))
    COR_MAT = (COR_MAT+COR_MAT_AVG)/2

    DIVS_PROJ = 0.01*as.numeric(get_secs_num_field(STOCKS,'PROJ 12M DIV',conn))
    DIVS_FACT = 0.01*as.numeric(get_secs_num_field(STOCKS,'FACT 12M DIV',conn))
    DIVS = ifelse(!is.na(DIVS_PROJ),DIVS_PROJ,ifelse(!is.na(DIVS_FACT),DIVS_FACT,0))
    #DIVS = array(0,length(STOCKS))

    SIGMAS = foreach(i=1:ncol(prices),.combine=c)%do%(sqrt(250)*sd(tail(diff(prices[!is.na(prices[,i]),i]),250)))
    RFR = get_rfr(TTM)
    BARRIERS = as.numeric(strsplit(p$V2[p$V1=='Barriers'],'@@')[[1]])

    cat(gsub('\\.',',',as.character(round(wo_calculate(TTM, BARRIERS, SIGMAS, COR_MAT, RFR, DIVS, COUPON),1)*FIX_SP_MULT + FIX_SP_ADD_WO))) # !!!

    #  wo_calculate(TTM, BARRIERS, SIGMAS, COR_MAT, RFR, DIVS, COUPON)
}


wo_calculator = function(){
    writeLines(gsub('NEWLINE','\n',POST$params), file('/home/slepnev/R/tmp_params_wo.txt'))
    p = read.csv(file='/home/slepnev/R/tmp_params_wo.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

    conn = create_conn()

    STOCKS = as.character(p$V2[p$V1=='STOCK'])
    SIGMAS = as.numeric(p$V2[p$V1=='SIGMA'])

    RFR = as.numeric(p[p[,1]=='RFR',2])
    COUPON = as.numeric(p[p[,1]=='Coupon',2])
    TTM = as.numeric(p[p[,1]=='TTM',2])
    BARRIERS = as.numeric(strsplit(as.character(p[p[,1]=='Barriers',2]),'-')[[1]])
    COR_MAT = matrix(as.numeric(p$V2[p$V1=='MATRIXvs']),length(SIGMAS),length(SIGMAS))

    DIVS_PROJ = 0.01*as.numeric(get_secs_num_field(STOCKS,'PROJ 12M DIV',conn))
    DIVS_FACT = 0.01*as.numeric(get_secs_num_field(STOCKS,'FACT 12M DIV',conn))
    DIVS = ifelse(!is.na(DIVS_PROJ),DIVS_PROJ,ifelse(!is.na(DIVS_FACT),DIVS_FACT,0))


    cat(wo_calculate(TTM, BARRIERS, SIGMAS, COR_MAT, RFR, DIVS, COUPON))
}


# ttm = TTM; barriers = BARRIERS; sigmas = SIGMAS; cor_mat = COR_MAT; rfr = RFR; coupon = COUPON; dys = DIVS
# ttm = 5; barriers = c(100,100,100,100,100); sigmas = c(0.05,0.07,0.04,0.03,0.05); corr = 0.3; rfr = 0.015; coupon = 0.04
wo_calculate = function(ttm, barriers, sigmas, cor_mat, rfr, dys, coupon){
        mc_paths = 300000
    
        d = foreach(i=1:ttm)%dopar%{ rmnorm(mc_paths,array(0,length(sigmas)),cor_mat) }
        drfs = rfr - dys - 0.5*sigmas^2
        if(ttm > 1) for(i in 2:ttm) d[[i]] = d[[i]] + d[[i-1]]
        if(length(sigmas) > 1)
            wo = foreach(y=d)%dopar%{ x = drfs[1]+sigmas[1]*y[,1]; for(i in 2:ncol(y)) x = ifelse(x >  drfs[i]+sigmas[i]*y[,i],  drfs[i]+sigmas[i]*y[,i], x); x }
        else
            wo = foreach(y=d)%dopar%{ x = drfs[1]+sigmas[1]*y; x }
        #wo = foreach(y=d)%dopar%{ x = sigmas[1]*y[,1]; for(i in 2:ncol(y)) x = ifelse(x >  sigmas[i]*y[,i],  sigmas[i]*y[,i], x); x }
    	
	#r = array(0,mc_paths)
	res = array(0,0)
	for(i in ttm:1){
		#r = r + rnorm(length(r),rfr-0.5*sigma^2,sigma)
		#r = r + rnorm(length(r),0,sigma)
                r = wo[[i]]
                idx = exp(r) > barriers[i]*0.01
                res = c(res, array(i*coupon*exp(-i*rfr),sum(idx)))
            
                for(j in 1:length(wo))
                    wo[[j]] = wo[[j]][!idx]
	}

        res = c(res, array(0,mc_paths - length(res)))
    
	mean(res)
}


wo_emp_matrices = function(){
        x = get(load('/home/slepnev/R/wo_elements.RData'))
	writeLines(gsub('NEWLINE','\n',POST$params), file('/home/slepnev/R/tmp_params_wo.txt'))
	p = read.csv(file='/home/slepnev/R/tmp_params_wo.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)
	writeLines(gsub('NEWLINE','\n',POST$params2), file('/home/slepnev/R/tmp_params_wo2.txt'))
        p2 = read.csv(file='/home/slepnev/R/tmp_params_wo2.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

	STOCKS = as.character(p2$V2[p2$V1=='STOCK'])
        CURRENCY = as.character(p[p[,1]=='Currency',2])

        conn = create_conn()
        prices = log(get_eod_quotes(STOCKS, conn, CURRENCY))    #x[,STOCKS])
        #prices = log(x[,STOCKS])
        corMine = function(ts) { foreach(i=1:ncol(ts),.combine=cbind)%do%{ foreach(j=1:ncol(ts),.combine=c)%do%{
            idx = !(is.na(ts[,i]) | is.na(ts[,j]))
            cor(diff(ts[idx,i])[-1],diff(ts[idx,j])[-1])
        } } }
        
	res = corMine(tail(prices,63))
	rownames(res) = colnames(prices)
	colnames(res) = colnames(prices)
	for(i in c(125,250,500)){
		tmp_res = corMine(tail(prices,i))
		rownames(tmp_res) = colnames(prices)
		colnames(tmp_res) = colnames(prices)
		res = rbind(res,tmp_res)
	}


        SD_HIST = foreach(i=1:ncol(prices),.combine=c)%do%(sqrt(250)*sd(tail(diff(prices[!is.na(prices[,i]),i]),250)))
        SD = 0.01*as.numeric(get_secs_num_field(STOCKS,'12M ATM VOLATILITY',conn))
        SD  = ifelse(is.na(SD),SD_HIST,SD)

	cat(paste(t(res),collapse=';'))
	cat('+')
	#cat(paste(foreach(i=1:ncol(prices),.combine=c)%do%(sqrt(250)*sd(tail(diff(prices[!is.na(prices[,i]),i]),250))),collapse=';'))
	cat(paste(SD,collapse=';'))
}










# ttm = TTM; barriers = BARRIERS; sigmas = SIGMAS; cor_mat = COR_MAT; rfr = RFR; coupon = COUPON; dys = DIVS
# ttm = 5; levels = c(1,1.2,1.3,1.5); factors = c(2,1,0.5,0); fix_rets = c(0,0,0,0.1); sigma = 0.3; rfr = 0.02; div = 0.0
# ttm = 1; levels = c(1,1.1); factors = c(1,10); fix_rets = c(0.0,0.0); sigma = 0.2; rfr = 0.01; div = 0.0
veyer_calculate = function(ttm, levels, factors, fix_rets, sigma, rfr, div){
    mc_paths = 300000
   
    r = array(1,mc_paths)
    r_lvl = array(1,mc_paths)
    drf = rfr-div-0.5*sigma^2

    for(i in 1:ttm){
        r = r*exp(rnorm(mc_paths,drf,sigma))
        for(i in 1:length(levels))
            r_lvl[r > levels[i] & r_lvl < i] = i
    }

    r = factors[r_lvl]*(r-1) + fix_rets[r_lvl]
    exp(-rfr*ttm) * mean(ifelse(r>0,r,0))
}


veyer_calculate_alt = function(ttm, levels, factors, fix_rets, sigma, rfr, div){
    mc_paths = 300000
    core_count = 32
   
    drf = rfr-div-0.5*sigma^2

    res = exp(-rfr*ttm) * mean(foreach(i=1:core_count,.combine=c)%dopar%{
        ress = 0
        lvls = array(0,ttm)
        max_lvl = 0
        for(j in 1:floor(mc_paths/core_count)){
            r0 = exp(cumsum(rnorm(ttm,drf,sigma)))
            lvls = which(levels<max(r0))
            ress = ress + if(length(lvls) > 0 && r0[ttm] > 1) { max_lvl = max(lvls); (r0[ttm] - 1)*factors[max_lvl] + fix_rets[max_lvl] } else 0
        }

        ress/j
    })


}









# ttm = 3; levels = c(1,2); sigma = 0.2; rfr = 0.01; div = 0.0
titan_calculate = function(ttm, levels, sigma, rfr, div){
    mc_paths = 300000
   
    r = array(1,mc_paths)
    drf = rfr-div-0.5*sigma^2
    r = exp(rnorm(mc_paths,drf*ttm,sigma*sqrt(ttm)))-1
    r = r * ifelse(r<levels[1],1,ifelse(r>levels[2],levels[2],r))
    exp(-rfr*ttm) * mean(ifelse(r>0,r,0))
}










bowo_calculate = function(ttm, barriers, sigmas, cor_mat, rfr, coupon, barrier_ceiling){
	#writeLines(gsub('NEWLINE','\n',readLines('tmp_params.txt')), file(tmp_file_1))
        #p = read.csv(file=tmp_file_1,header=FALSE,sep=',',stringsAsFactors=FALSE)
    
        #cm = function(n, x) { diag(n) + x*(1-diag(n)) }
    
	#writeLines(gsub('NEWLINE','\n',POST$params), file('/home/slepnev/R/tmp_params.txt'))
	#p = read.csv(file='/home/slepnev/R/tmp_params.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

        mc_paths = 200000
    
        #d = foreach(i=1:ttm)%dopar%rmnorm(mc_paths,array(0,length(sigmas)),cm(length(sigmas),corr))
        d = foreach(i=1:ttm)%dopar%{ rmnorm(mc_paths,array(0,length(sigmas)),cor_mat) }
        drfs = rfr - 0.5*sigmas^2
        #d = foreach(i=1:ttm)%dopar%{ rmnorm(mc_paths,rfr-0.5*sigmas^2,cor_mat) }
        for(i in 2:ttm) d[[i]] = d[[i]] + d[[i-1]]
        wo = foreach(y=d)%dopar%{ x = drfs[1]+sigmas[1]*y[,1]; for(i in 2:ncol(y)) x = ifelse(x >  drfs[i]+sigmas[i]*y[,i],  drfs[i]+sigmas[i]*y[,i], x); x }
        #wo = foreach(y=d)%dopar%{ x = sigmas[1]*y[,1]; for(i in 2:ncol(y)) x = ifelse(x >  sigmas[i]*y[,i],  sigmas[i]*y[,i], x); x }
    	
	#r = array(0,mc_paths)
	res = array(0,0)
	for(i in ttm:1){
		#r = r + rnorm(length(r),rfr-0.5*sigma^2,sigma)
		#r = r + rnorm(length(r),0,sigma)
                r = wo[[i]]
                idx = exp(r) > barriers[i]*0.01 & exp(r) < (barriers[i] + barrier_ceiling)*0.01
                res = c(res, array(i*coupon*exp(-i*rfr),sum(idx)))
            
                for(j in 1:length(wo))
                    wo[[j]] = wo[[j]][!idx]
	}

        res = c(res, array(0,mc_paths - length(res)))
    
	#mean(res)

	cat(mean(res))
	
}











# ttm = 5; base_spot = 1; sigma = 0.2; rfr = 0.015
restrike_calculator = function(){

    writeLines(gsub('NEWLINE','\n',POST$params), file('/home/slepnev/R/tmp_restrike_params.txt'))
    p = read.csv(file='/home/slepnev/R/tmp_restrike_params.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

    ttm = as.numeric(p[p[,2] == 'TTM, years:',3])
    base_spot = as.numeric(p[p[,2] == 'Spot:',3])
    sigma = as.numeric(p[p[,2] == 'Sigma:',3])
    rfr = as.numeric(p[p[,2] == 'RFR:',3])
    dividend = as.numeric(p[p[,2] == 'Dividend yield:',3])
    mc_paths = as.numeric(p[p[,2] == 'Monte-Carlo paths:',3])
	
    drf = rfr - dividend - 0.5*(sigma^2)
    r = array(0,mc_paths) # log [spot/base spot]
    strikes = array(base_spot,mc_paths) # strikes
    res = array(0,mc_paths)

    for(i in 1:ttm){
        r = r + rnorm(length(r),drf,sigma)
        spots = base_spot*exp(r)
        res = res + exp(-i*rfr) * ifelse(spots > strikes, spots/strikes - 1, 0)
        strikes = spots
    }

    cat(mean(res))
}









#      ttm = 5; strike = 0.8; sigma = 0.2; rfr = 0.015; is_daily = TRUE; dividend = 0.01; mc_paths = 10000; is_call = TRUE;           strike = -1;           is_daily = FALSE
#
lb_calculator = function(){

    writeLines(gsub('NEWLINE','\n',POST$params), file('/home/slepnev/R/tmp_lb_params.txt'))
    p = read.csv(file='/home/slepnev/R/tmp_lb_params.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

    ttm = as.numeric(p[p[,2] == 'TTM, years:',3])
    spot =  as.numeric(p[p[,2] == 'Spot:',3])
    strike = if (as.character(p[p[,2] == 'Strike:',3]) == 'FLOATING') -1 else as.numeric(p[p[,2] == 'Strike:',3])
    sigma = as.numeric(p[p[,2] == 'Sigma:',3])
    is_daily = if (as.character(p[p[,2] == 'Time step:',3]) == 'DAILY') TRUE else FALSE
    pmt_sched = as.character(p[p[,2] == 'Payment schedule:',3])
    is_call = if (as.character(p[p[,2] == 'Option type:',3]) == 'CALL') TRUE else FALSE
    rfr = as.numeric(p[p[,2] == 'RFR:',3])
    dividend = as.numeric(p[p[,2] == 'Dividend yield:',3])
    mc_paths = as.numeric(p[p[,2] == 'Monte-Carlo paths:',3])
	
    d_epsilon = 0.001
    v_epsilon0 = 0.001
    r_epsilon0 = 0.0001

    if (is_daily){
        n = ttm * 252    
        drf = (rfr - dividend - 0.5*sigma^2)/252
        dsigm = sigma/sqrt(252)
        r_epsilon = r_epsilon0/252
        v_epsilon = v_epsilon0/sqrt(252)
        pay_wnd = if(pmt_sched == 'ANNUAL') foreach(i=1:ttm)%do%(1:(i*252)) else list(1:n)
    } else{
        n = ttm    
        drf = rfr - dividend - 0.5*sigma^2
        dsigm = sigma
        pay_wnd = if(pmt_sched == 'ANNUAL') foreach(i=1:n)%do%(1:i) else list(1:n)
    }

    core_count = 32
    
    res = if(strike > 0) foreach(i=1:core_count,.combine='+')%dopar%{
        ress = array(0,4)
        if (is_call) for(j in 1:floor(mc_paths/core_count)){
                r0 = rnorm(n,0,1)
                for(k in 1:4){
                    rfr2 = rfr
                    r = if(k==1) spot*exp(cumsum(drf + r0*dsigm)) else
                          if(k==2) (spot + d_epsilon)*exp(cumsum(drf + r0*dsigm)) else
                          if(k==3) spot*exp(cumsum(drf + r0*(dsigm + v_epsilon))) else
                          if(k==4) { rfr2 = rfr + r_epsilon0; spot*exp(cumsum(drf + r_epsilon + r0*dsigm)); }

                    ress[k] = ress[k] + if(length(pay_wnd)==1) max(max(r) - strike,0)*exp(-ttm*rfr2) else { r = sapply(pay_wnd, FUN=function(x) { max(max(r[x]) - strike,0) }); sum(c(r[1],diff(r))*exp(-(1:length(pay_wnd))*rfr2)) }
                }
            } else for(j in 1:floor(mc_paths/core_count)){
                r0 = rnorm(n,0,1)
                for(k in 1:4){
                    rfr2 = rfr
                    r = if(k==1) spot*exp(cumsum(drf + r0*dsigm)) else
                          if(k==2) (spot + d_epsilon)*exp(cumsum(drf + r0*dsigm)) else
                          if(k==3) spot*exp(cumsum(drf + r0*(dsigm + v_epsilon))) else
                          if(k==4) { rfr2 = rfr + r_epsilon0; spot*exp(cumsum(drf + r_epsilon + r0*dsigm));}

                    ress[k] = ress[k] + if(length(pay_wnd)==1) max(strike - min(r),0)*exp(-ttm*rfr2) else sapply(pay_wnd, FUN=function(x) { max(strike - min(r[x]),0) }); r = sum(c(r[1],diff(r))*exp(-(1:length(pay_wnd))*rfr2))
                }
            }

        ress/floor(mc_paths/core_count)
    }/core_count else foreach(i=1:core_count,.combine='+')%dopar%{
        ress = array(0,4)
        if (is_call) for(j in 1:floor(mc_paths/core_count)){
                r0 = rnorm(n,0,1)
                for(k in 1:4){
                    rfr2 = rfr
                    r = if(k==1) spot*exp(cumsum(drf + r0*dsigm)) else
                          if(k==2) (spot + d_epsilon)*exp(cumsum(drf + r0*dsigm)) else
                          if(k==3) spot*exp(cumsum(drf + r0*(dsigm + v_epsilon))) else
                          if(k==4) { rfr2 = rfr + r_epsilon0; spot*exp(cumsum(drf + r_epsilon + r0*dsigm)); }

                    ress[k] = ress[k] + max(r[n] - min(r),0)*exp(-ttm*rfr2)                 
                }
            } else for(j in 1:floor(mc_paths/core_count)){
                r0 = rnorm(n,0,1)
                for(k in 1:4){
                    rfr2 = rfr
                    r = if(k==1) spot*exp(cumsum(drf + r0*dsigm)) else
                          if(k==2) (spot + d_epsilon)*exp(cumsum(drf + r0*dsigm)) else
                          if(k==3) spot*exp(cumsum(drf + r0*(dsigm + v_epsilon))) else
                          if(k==4) { rfr2 = rfr + r_epsilon0; spot*exp(cumsum(drf + r_epsilon + r0*dsigm));  }

                    ress[k] = ress[k] + max(max(r) - r[n],0)*exp(-ttm*rfr2)                 
                }
            }

        ress/floor(mc_paths/core_count)
    }/core_count

   
    cat(res[1])
    cat(';')
    cat((res[2]-res[1])/d_epsilon)
    cat(';')
    cat((res[3]-res[1])/v_epsilon0)
    cat(';')
    cat((res[4]-res[1])/r_epsilon0)
}









# ttm = 5; base_spot = 1; strike = 1; sigma = 0.2; rfr = 0.015; mc_paths = 50000; coupons = 6; ranges = c(70,130)
ra_calculator = function(){

    writeLines(gsub('NEWLINE','\n',POST$params), file('/home/slepnev/R/tmp_ra_params.txt'))
    p = read.csv(file='/home/slepnev/R/tmp_ra_params.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

    ttm = as.numeric(p[p[,2] == 'TTM, years:',3])
    sigma = as.numeric(strsplit(as.character(p[p[,2]=='Sigma:',3]),'-')[[1]])
    ranges = as.array(as.numeric(strsplit(as.character(p[p[,2]=='Ranges:',3]),'-')[[1]]))
    coupons = as.array(as.numeric(strsplit(as.character(p[p[,2]=='Coupons:',3]),'-')[[1]]))
    rfr = as.numeric(p[p[,2] == 'RFR:',3])
    mc_paths = as.numeric(p[p[,2] == 'Monte-Carlo paths:',3])

    v_epsilon = 0.001
    d_epsilon = 0.001
    r_epsilon = 0.001

    n = ttm * 252    
    drf = (rfr - 0.5*sigma^2)/252

    dsigm = sigma/sqrt(252)
    dcoup = coupons/(100 * 252)  # simple percent, daily
    drng = ranges/100
    dv_eps = v_epsilon/sqrt(252)
    drf_v = (rfr - 0.5*(sigma+v_epsilon)^2)/252
    dr_eps = r_epsilon/252

    res = foreach(i=1:40,.combine='+')%dopar%{
        res = 0
        res_d = 0
        res_v = 0
        res_r = 0
        for(j in 1:floor(mc_paths/40)){
            r0 = rnorm(n,0,1)
            r = exp(cumsum(drf + r0*dsigm))

            r_d = exp(d_epsilon + cumsum(drf + r0*dsigm))
            r_v = exp(cumsum(drf_v + r0*(dsigm + dv_eps)))
            r_r = exp(cumsum(drf + dr_eps + r0*dsigm))
            for(k in 2:length(dcoup)){
                res = res + sum(ifelse(r > drng[k-1] & r < drng[k], dcoup[k-1], 0))

                res_d = res_d + sum(ifelse(r_d > drng[k-1] & r_d < drng[k], dcoup[k-1], 0))
                res_v = res_v + sum(ifelse(r_v > drng[k-1] & r_v < drng[k], dcoup[k-1], 0))
                res_r = res_r + sum(ifelse(r_r > drng[k-1] & r_r < drng[k], dcoup[k-1], 0))
            }
        }
        
        c(res, res_d, res_v, res_r)/floor(mc_paths/40)
    }/40

    res[1:3] = res[1:3] * exp(-ttm*rfr)
    res[4] = res[4] * exp(-ttm*(rfr + r_epsilon))
    
    cat(res[1])
    cat(';')
    cat((res[2]-res[1])/d_epsilon)
    cat(';')
    cat((res[3]-res[1])/v_epsilon)
    cat(';')
    cat((res[4]-res[1])/r_epsilon)
}


sp_process_share = function(){
    
}
