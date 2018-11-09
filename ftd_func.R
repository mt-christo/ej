# source('rapache_eval1.R')

## CDS spread implied from PD value
## pd = yearly default intensity, rfr - risk-free rate
## , n_years =  years to maturity, rr - recovery rate
imp_spread = function(pd0, rfr_in, n_years, rr = 0.4){
	res = 0
	p_d = pd0
	p_s = 1 - p_d

	n_years_rounded = floor(n_years)
	for(i in 1:n_years_rounded){
		res = res + p_d*(1 - rr)*exp(-rfr_in*(i - 0.5))
		p_d = pd0*p_s
		p_s = p_s*(1 - pd0)		
	}

	if(n_years > n_years_rounded)
		res = res + (n_years - n_years_rounded)*p_d*(1 - rr)*exp(-rfr_in*(i - 0.5))

	return(res)
}
#imp_spread(0.025207,0.0115,4)

# pd0=1-mean(ps); n_years=basket_years_in; rr=rec_rate_in
imp_spread2 = function(pd0, rfr_in, n_years, rr = 0.4){
	n_years_rounded = floor(n_years)
        As = array(0,n_years_rounded)
        Bs = array(0,n_years_rounded)
        Cs = array(0,n_years_rounded)      
	p_reach = 1
	p_surv = 1 - pd0
        
	for(i in 1:n_years_rounded){
		As[i] = p_reach*p_surv*exp(-rfr_in*i)
		Bs[i] = p_reach*0.5*(1 - p_surv)*exp(-rfr_in*(i - 0.5))
		Cs[i] = p_reach*(1 - p_surv)*(1 - rr)*exp(-rfr_in*(i - 0.5))
		p_reach = p_reach*p_surv
	}

	if(n_years > n_years_rounded){
                i = length(As)+1
		As[i] = (n_years - n_years_rounded)*p_reach*p_surv*exp(-rfr_in*i)
		Bs[i] = (n_years - n_years_rounded)*p_reach*0.5*(1 - p_surv)*exp(-rfr_in*(i - 0.5))
		Cs[i] = (n_years - n_years_rounded)*p_reach*(1 - p_surv)*(1 - rr)*exp(-rfr_in*(i - 0.5))
        }

	return(sum(Cs)/sum(As+Bs))
}

## Yearly default intensity (PD) from CDS value
## s = market cds quote, rfr - risk-free rate
## , n_years =  years to maturity, rr - recovery rate
#s = 0.05694877; rfr = 0.0115; n_years = 4; rr = 0.4
#s = yields[1]*0.01; rfr = 0.015; n_years = 3; rr = 0.4
imp_pd = function(s, rfr_in, n_years, rr = 0.4){
	imp_spread0 = function(x) { imp_spread(x,rfr_in,n_years,rr) - s }

	return(uniroot(imp_spread0,c(0,1.0))$root)
}
#imp_pd(0.057,RFR,3)

imp_pd2 = function(s, rfr_in, n_years, rr = 0.4, rec_ratio_in = 10000){
	imp_spread0 = function(x) { imp_spread2(x,rfr_in,n_years,rr) - s }

        x = uniroot(imp_spread0,c(0,1.0))$root
        x = x + (1 - x)*rec_ratio_in
	return(x)
}
#imp_pd(0.057,RFR,3)

## Probability of AT LEAST 1 default, given time series of basket default prob vectors
ftd_prob_mtx = function(pds_ts){
	return(pds_ts[,1] 
	+ foreach(i=2:ncol(pds_ts),.combine='+')%do%
	{ pds_ts[,i]*foreach(j=1:(i-1),.combine='*')%do%(1 - pds_ts[,j]) })
}

## Probability of AT LEAST 1 default, given time series of basket default prob vectors
ftd_prob = function(pds){
	return(pds[1] + foreach(i=2:length(pds),.combine='+')%do%
		{ pds[i]*foreach(j=1:(i-1),.combine='*')%do%(1 - pds[j]) })
}

#y_pd = y
#for(i in 1:nrow(y))
#	for(j in 1:ncol(y))
#		y_pd[i,j] = imp_pd(y[i,j]*0.01, RFR, N_YEARS_IMPLY_SPREAD, REC_RATE)
#y_tod = yields_tmp
#rfr_in=rfr_tmp
# y_tod=yields_tmp; rfr_in=rfr_tmp; spread_years_in=N_YEARS_IMPLY_SPREAD; basket_years_in=N_YEARS_BASKET; rec_rate_in=REC_RATE
# y_tod=yields_in[[i]]; rfr_in=if(i==2) { RFR + epsilon_rfr } else RFR; spread_years_in=N_YEARS_IMPLY_SPREAD; basket_years_in=N_YEARS_BASKET; rec_rate_in=REC_RATE; corr_mat_in=cor_mat
FTD = function(y_tod, rfr_in, spread_years_in, basket_years_in, rec_rate_in){
	d = D_GLOBAL 
	
	# subtracting RFR from Yields, to form CDS spread
	e_pds = as.numeric(foreach(i=1:length(y_tod),.combine=c)%do%imp_pd(y_tod[i] - rfr_in, rfr_in, spread_years_in[i], rec_rate_in))
	q_pds = qnorm(e_pds)
	
	## 1 means survival of i'th bond, 0 means default
	survs = list()
	survs[[1]] = foreach(i=1:ncol(d[[1]]),.combine=cbind)%do%ifelse(d[[1]][,i] < q_pds[i], 0, 1)
	for(j in 2:length(d))
		survs = c(survs, list(survs[[j-1]]*foreach(i=1:ncol(d[[j]]),.combine=cbind)%do%ifelse(d[[j]][,i] < q_pds[i], 0, 1)))
	
	## Same for the basket
	surv = foreach(x=survs)%do%{ foreach(i=1:ncol(x),.combine='*')%do%x[,i] }

	As_on = list(); Bs_on = list(); Cs_on = list();
	for(i in 1:basket_years_in){
		As_on[[i]] = surv[[i]]*exp(-rfr_in*i)

		non_surv = if(i==1) (1 - surv[[i]]) else ifelse(surv[[i]]==surv[[i-1]], 0, 1 - surv[[i]])
		Bs_on[[i]] = 0.5*non_surv*exp(-rfr_in*(i - 0.5))
		Cs_on[[i]] = non_surv*(1 - rec_rate_in)*exp(-rfr_in*(i - 0.5))
print(mean(Cs_on[[i]]))
	}

	A = foreach(x=As_on,.combine='+')%do%x
	B = foreach(x=Bs_on,.combine='+')%do%x
	C = foreach(x=Cs_on,.combine='+')%do%x
	c(spread = mean(C/(A+B)), value = mean(C));
}

CorrectCM = function(CM)
  {
  n <- dim(var(CM))[1L]
  E <- eigen(CM)
  CM1 <- E$vectors %*% tcrossprod(diag(pmax(E$values, 0), n), E$vectors)
  Balance <- diag(1/sqrt(diag(CM1)))
  CM2 <- Balance %*% CM1 %*% Balance  
  return(CM2)
}

if(FALSE){

   i = 1
    y_tod=bond_yields; rfr_in=if(i==2) { RFR + epsilon_rfr } else RFR; spread_years_in=N_YEARS_IMPLY_SPREAD; basket_years_in=N_YEARS_BASKET; rec_rates_in=RECOVERIES; corr_mat_in=cor_mat
    rec_ratios_in = rec_ratios
    #cm=function(x){ diag(4)+x*(1-diag(4)) }; corr_mat_in = cm(0.99999);
    FTD_a(y_tod, rfr_in, spread_years_in, basket_years_in, rec_rate_in, corr_mat_in, rec_ratios_in);

    i = 2
    y_tod1=bond_yields; y_tod2=bond_yields; rfr_in=if(i==2) { RFR + epsilon_rfr } else RFR; spread_years_in=N_YEARS_IMPLY_SPREAD; basket_years_in=N_YEARS_BASKET; rec_rates_in=RECOVERIES; corr_mat_in=cor_mat
    rec_ratios_in = rec_ratios
    #cm=function(x){ diag(4)+x*(1-diag(4)) }; corr_mat_in = cm(0.99999);
    dFTD_a(y_tod1, y_tod2, rfr_in, spread_years_in, basket_years_in, rec_rate_in, corr_mat_in, rec_ratios_in);
}
FTD_a = function(y_tod, rfr_in, spread_years_in, basket_years_in, rec_rates_in, corr_mat_in, rec_ratios_in){
	# subtracting RFR from Yields, to form CDS spread
	e_pds = as.numeric(foreach(i=1:length(y_tod),.combine=c)%do%imp_pd2(y_tod[i] - rfr_in, rfr_in, spread_years_in[i], rec_rates_in[i], rec_ratios_in[i]))
	q_pds = qnorm(e_pds)
#        ps = as.numeric(pmvnorm(q_pds,rep(100.0,length(q_pds)),corr=as.matrix(nearPD(corr_mat_in,corr=TRUE)$mat),maxpts=5000,abseps=0.0001))
        ps = as.numeric(pmvnorm(q_pds,rep(100.0,length(q_pds)),corr=CorrectCM(corr_mat_in),maxpts=5000,abseps=0.0001))
    
        imp_spread2(1-mean(ps), rfr_in, basket_years_in, sum(rec_rates_in*e_pds)/sum(e_pds))
}

# y_tod1 = y_tod; y_tod2 = y_tod1+c(0.01,-0.01,0.01,-0.01)
dFTD_a = function(y_tod1, y_tod2, rfr_in, spread_years_in, basket_years_in, rec_rates_in, corr_mat_in, rec_ratios_in){
    x = dFTD_two(y_tod1, y_tod2, rfr_in, spread_years_in, basket_years_in, rec_rates_in, corr_mat_in, rec_ratios_in)
    x[2]-x[1]
}

# y_tod1 = bond_yields; y_tod2 = bond_yields + epsilon_rfr; rfr_in = RFR; spread_years_in = N_YEARS_IMPLY_SPREAD; basket_years_in = N_YEARS_BASKET; rec_rates_in = RECOVERIES; corr_mat_in = cor_mat; rec_ratios_in = rec_ratios; rfr2_in = RFR + epsilon_rfr
dFTD_two = function(y_tod1, y_tod2, rfr_in, spread_years_in, basket_years_in, rec_rates_in, corr_mat_in, rec_ratios_in, rfr2_in = 0){
        # subtracting RFR from Yields, to form CDS spread
        e_pds1 = as.numeric(foreach(i=1:length(y_tod1),.combine=c)%do%imp_pd2(y_tod1[i] - rfr_in, rfr_in, spread_years_in[i], rec_rates_in[i], rec_ratios_in[i]))
        q_pds1 = qnorm(e_pds1)
        e_pds2 = as.numeric(foreach(i=1:length(y_tod2),.combine=c)%do%imp_pd2(y_tod2[i] - if(rfr2_in==0) rfr_in else rfr2_in, if(rfr2_in==0) rfr_in else rfr2_in, spread_years_in[i], rec_rates_in[i], rec_ratios_in[i]))
        q_pds2 = qnorm(e_pds2)

    d = 0
    qs1 = q_pds1
    S = 1:length(y_tod1)
    for(i in 1:length(y_tod1)){
#        d = d - sign(q_pds2[i] - q_pds1[i])*as.numeric(pmvnorm(ifelse(S==i,min(qs1[i],q_pds2[i]),qs1),ifelse(S==i,max(qs1[i],q_pds2[i]),Inf),corr=CorrectCM(corr_mat_in),maxpts=100000,abseps=0.000001))
        d = d - sign(q_pds2[i] - q_pds1[i])*as.numeric(pmvnorm(ifelse(S==i,min(qs1[i],q_pds2[i]),qs1),ifelse(S==i,max(qs1[i],q_pds2[i]),Inf),corr=cov2cor(corr_mat_in),maxpts=100000,abseps=0.000001))
        qs1[i] = q_pds2[i]
    }

    ps = as.numeric(pmvnorm(q_pds1,rep(100.0,length(q_pds1)),corr=corr_mat_in,maxpts=1000,abseps=0.0001))
    spread1 = imp_spread2(1-mean(ps), rfr_in, basket_years_in, sum(rec_rates_in*e_pds1)/sum(e_pds1))
    spread2 = imp_spread2(1-mean(ps+d), rfr_in, basket_years_in, sum(rec_rates_in*e_pds2)/sum(e_pds2))
    
    c(spread1, spread2)
}


ftd_calculator = function(){
        tryCatch({

	#tmp_file_1 = tempfile()

	#writeLines(gsub('NEWLINE','\n',POST$params), file(tmp_file_1))
	writeLines(gsub('NEWLINE','\n',POST$params), file('/home/slepnev/R/tmp_params_ftd.txt'))
	#p = read.csv(file=tmp_file_1,header=FALSE,sep=',',stringsAsFactors=FALSE)
	p = read.csv(file='/home/slepnev/R/tmp_params_ftd.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

	CLEAN_PRICES = as.numeric(p$V2[p$V1=='CLEAN_PRICE'])
        nbonds = length(CLEAN_PRICES)
	MATURITIES = as.Date(gsub('#','',p$V2[p$V1=='MATURITY']),format='%d.%m.%Y')
	COUPONS = 0.01*as.numeric(p$V2[p$V1=='COUPON'])
	COUPON_FREQS = as.numeric(p$V2[p$V1=='FREQUENCY'])
	RECOVERIES = as.numeric(p$V2[p$V1=='RECOVERY'])
	CONVENTION = '30/360'

	RFR = as.numeric(p[p[,1]=='RFR:',2])
	FUNDR = as.numeric(p[p[,1]=='Funding rate:',2])
	SETTLE_DATE = as.Date(gsub('#','',p[p[,1]=='Trade date:',2]))
	FTD_MATURITY = as.Date(gsub('#','',p[p[,1]=='Maturity:',2]),format='%d.%m.%Y') #as.Date(gsub('#','',p[p[,1]=='Maturity:',2]))
	N_YEARS_BASKET = as.numeric(FTD_MATURITY - SETTLE_DATE)/365
	HEDGE_TYPE = as.character(p$V2[p$V1=='Hedge type:'])
	N_YEARS_IMPLY_SPREAD = as.numeric(MATURITIES - SETTLE_DATE)/365  # array-shaped and non-integer now

	yield_from_c = function(x, settle_date, note_coupon = -1){
            if(note_coupon == -1) foreach(j=1:length(x),.combine=c)%do%bond.yield(settle_date, MATURITIES[j], COUPONS[j], COUPON_FREQS[j], x[j], CONVENTION) else
                foreach(j=1:length(x),.combine=c)%do%bond.yield(settle_date, FTD_MATURITY, note_coupon, 1, x[j], CONVENTION)
                                                                                                                                                        

	}

	c_from_yield = function(x, settle_date, note_coupon = -1){
            if(note_coupon == -1) foreach(j=1:length(x),.combine=c)%do%bond.price(settle_date, MATURITIES[j], COUPONS[j], COUPON_FREQS[j], x[j], CONVENTION) else
                foreach(j=1:length(x),.combine=c)%do%bond.price(settle_date, FTD_MATURITY, note_coupon, 1, x[j], CONVENTION)
	}

	cor_mat = cor.smooth(matrix(as.numeric(p$V2[p$V1=='MATRIXvs']),length(MATURITIES),length(MATURITIES)))
	
        if(!isSymmetric(cor_mat)) stop('ERROR!')

	pr = eigen(cor_mat)$vectors
	spr = solve(pr)

	epsilon_rfr = 0.01 # 10 basis points
	bond_yields = yield_from_c(CLEAN_PRICES, SETTLE_DATE)
        rec_ratios = exp(20*(RECOVERIES - CLEAN_PRICES*0.01))

        ftd0 = FTD_a(bond_yields, RFR, N_YEARS_IMPLY_SPREAD, N_YEARS_BASKET, RECOVERIES, cor_mat, rec_ratios)
        
        dv01_pair = dFTD_two(bond_yields - epsilon_rfr*0.5, bond_yields + epsilon_rfr*0.5, RFR - epsilon_rfr*0.5, N_YEARS_IMPLY_SPREAD, N_YEARS_BASKET, RECOVERIES, cor_mat, rec_ratios, RFR + epsilon_rfr*0.5)
        dv01 = diff(c_from_yield(RFR + dv01_pair,SETTLE_DATE,ftd0))/epsilon_rfr

        w = if(FALSE){
                cprices_in = c(list(CLEAN_PRICES),list(CLEAN_PRICES),foreach(j=1:nrow(spr))%do%{ CLEAN_PRICES + epsilon*spr[j,] }) # second element will be DV01-shifted
                yields_in = foreach(p = cprices_in)%do%yield_from_c(p,SETTLE_DATE)
                ftd_devs = foreach(i=2:length(yields_in),.combine=rbind)%dopar%dFTD_two(yields_in[[1]], yields_in[[i]] + if(i==2) RFR else 0, if(i==2) { RFR + epsilon_rfr } else RFR, N_YEARS_IMPLY_SPREAD, N_YEARS_BASKET, RECOVERIES, cor_mat, rec_ratios)
                ftds = foreach(i=1:nrow(ftd_devs),.combine=c)%do%{ x = c_from_yield(ftd_devs[i,],SETTLE_DATE); x[2] - x[1] }
                ftd_devs = foreach(i=1:length(ftds),.combine=c)%do%(ftds[i]/(if(i==1) 100*epsilon_rfr else epsilon))
                foreach(i=1:nrow(pr),.combine=cbind)%do%sum(ftd_devs[-1]*pr[i,1:(ncol(pr))])
            } else if (HEDGE_TYPE == 'YIELD') {
                epsilon = 0.00001 # 0.1 bp
                y_pairs = foreach(i=1:nbonds)%do%{ dFTD_two(bond_yields - ifelse(i==1:nbonds,epsilon*0.5,0), bond_yields + ifelse(i==1:nbonds,epsilon*0.5,0), RFR, N_YEARS_IMPLY_SPREAD, N_YEARS_BASKET, RECOVERIES, cor_mat, rec_ratios) }
                w_tmp = foreach(y=y_pairs,.combine=c)%do%{ diff(y)/epsilon }
                w_tmp*(ftd0-FUNDR+RFR)/sum(w_tmp*(bond_yields - FUNDR))
            } else if (HEDGE_TYPE == 'PRICE'){
 if(FALSE){
             cprices_in = c(list(CLEAN_PRICES),list(CLEAN_PRICES),foreach(j=1:length(CLEAN_PRICES))%do%{ CLEAN_PRICES + ifelse(j==1:length(CLEAN_PRICES),epsilon,0) }) # second element will be DV01-shifted
             yields_in = foreach(p = cprices_in)%do%yield_from_c(p,SETTLE_DATE)
             
             ftd_devs = foreach(i=2:length(yields_in),.combine=rbind)%dopar%dFTD_two(yields_in[[1]], yields_in[[i]] + if(i==2) RFR else 0, if(i==2) { RFR + epsilon_rfr } else RFR, N_YEARS_IMPLY_SPREAD, N_YEARS_BASKET, RECOVERIES, cor_mat, rec_ratios)
             ftds = foreach(i=1:nrow(ftd_devs),.combine=c)%do%{ x = c_from_yield(ftd_devs[i,],SETTLE_DATE); 100*(x[2] - x[1])/x[2] }
             ftd_devs = foreach(i=1:length(ftds),.combine=c)%do%(ftds[i]/(if(i==1) 100*epsilon_rfr else epsilon))
             ftd_devs[2:length(ftd_devs)]

             y1 = yields_in[[1]][[1]]
             y2 = y1 + 0.0000001
             y3 = y1 - 0.0000001
             j=1; d1 = bond.duration(SETTLE_DATE, MATURITIES[j], COUPONS[j], COUPON_FREQS[j], y2, CONVENTION)
             c1 = bond.price(SETTLE_DATE, MATURITIES[j], COUPONS[j], COUPON_FREQS[j], y1, CONVENTION)
             c2 = bond.price(SETTLE_DATE, MATURITIES[j], COUPONS[j], COUPON_FREQS[j], y2, CONVENTION)
             c3 = bond.price(SETTLE_DATE, MATURITIES[j], COUPONS[j], COUPON_FREQS[j], y3, CONVENTION)
             (c2-c1)/(y1-y2)
             (c3-c2)/(y2-y3)
             d1*c1/(1+y1)
 }
                epsilon = 0.001 # 0.1 bp
                dv01_pair = dFTD_two(bond_yields - epsilon_rfr*0.5, bond_yields + epsilon_rfr*0.5, RFR - epsilon_rfr*0.5, N_YEARS_IMPLY_SPREAD, N_YEARS_BASKET, RECOVERIES, cor_mat, rec_ratios, RFR + epsilon_rfr*0.5)
                y_pairs = foreach(i=1:nbonds)%do%{
                    dFTD_two(yield_from_c(CLEAN_PRICES - ifelse(i==1:nbonds,epsilon*0.5,0),SETTLE_DATE), yield_from_c(CLEAN_PRICES + ifelse(i==1:nbonds,epsilon*0.5,0),SETTLE_DATE), RFR, N_YEARS_IMPLY_SPREAD, N_YEARS_BASKET, RECOVERIES, cor_mat, rec_ratios) }
                y_pairs = foreach(y=y_pairs)%do%c_from_yield(RFR + y,SETTLE_DATE,ftd0)
                foreach(y=y_pairs,.combine=c)%do%{ diff(y)/epsilon }
        }


        #w = if(HEDGE_TYPE == 'WEIGHTS') w/sum(w) else w

        #w = w*sum(w*(bond_yields - RFR))/sum(w*(bond_yields - FUNDR))
    
	res = c(RFR + ftd0, dv01, w)
	
        cat(paste(res,collapse=';'))
        }, error = function(e) { cat('ERROR!;') })  

}


ftd_backtest = function(){
	p = read.csv(file='/home/slepnev/R/tmp_params_ftd.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

	CLEAN_PRICES = as.numeric(p$V2[p$V1=='CLEAN_PRICE'])
	MATURITIES = as.Date(gsub('#','',p$V2[p$V1=='MATURITY']))
	COUPONS = 0.01*as.numeric(p$V2[p$V1=='COUPON'])
	COUPON_FREQS = as.numeric(p$V2[p$V1=='FREQUENCY'])
	RECOVERIES = as.numeric(p$V2[p$V1=='RECOVERY'])
	CONVENTION = '30/360'

	RFR = as.numeric(p[p[,1]=='RFR:',2])
	N_YEARS_BASKET = as.numeric(p[p[,1]=='FTD years:',2])
	SETTLE_DATE = as.Date(gsub('#','',p[p[,1]=='Trade date:',2]))
	HEDGE_TYPE = as.character(p$V2[p$V1=='Hedge type:'])
	N_YEARS_IMPLY_SPREAD = as.numeric(MATURITIES - SETTLE_DATE)/365  # array-shaped and non-integer now

	yield_from_c = function(x, settle_date){
		foreach(j=1:length(x),.combine=c)%do%bond.yield(settle_date, MATURITIES[j], COUPONS[j], COUPON_FREQS[j], x[j], CONVENTION)
	}

	c_from_yield = function(x, settle_date){
		foreach(j=1:length(x),.combine=c)%do%bond.price(settle_date, MATURITIES[j], COUPONS[j], COUPON_FREQS[j], x[j], CONVENTION)
	}

	epsilon = 0.1 # 10 basis points
	#epsilon = 0.002 # 10 basis points
	epsilon_rfr = 0.001 # 10 basis points
	bonds_yields = yield_from_c(CLEAN_PRICES, SETTLE_DATE)



        
        r012 = c(0.04, 0.05)    #c(100,100)
        N = 0.9*365; sigmas = r012*0.1    #3/sqrt(250)
        cormat = function(x,n) { diag(n) + x*(1-diag(n)) }
        c12 =- 0.5
        cor_mat = cormat(c12,2)
        r12 = rmnorm(N,c(0,0),cor_mat)
        w1234 = foreach(k0 = c(1,2), .combine=cbind)%do%{            
            y1 = r012[1] + sigmas[1]*k0*r12[,1]
            y2 = r012[2] + sigmas[2]*k0*r12[,2]
            
#            foreach(k=1:(N-1), .combine=rbind)%do%{  w1 = r012[1]/y1[k];  w2 = r012[2]/y2[k];  c(y1[k], y2[k], w1/(w1+w2), w2/(w1+w2), (y1[k+1]-y1[k])*w1/(w1+w2) + (y2[k+1]-y2[k])*w2/(w1+w2))  }
#        };
#        plot(cumsum(w1234[,10]));
#        lines(cumsum(w1234[,5]))

            pr = eigen(cor_mat)$vectors
            spr = solve(pr)
            w12 = foreach(k=1:N,.combine=rbind)%dopar%{
                bond_yields = c(y1[k],y2[k])
                yields_in = c(list(bond_yields),list(bond_yields + epsilon_rfr),foreach(j=1:(nrow(spr)))%do%{ bond_yields + epsilon*spr[j,] }) # second element will be DV01-shifted
            
                rec_ratios = rep(0.00000001,2)

                w = if(TRUE){
             CLEAN_PRICES = c_from_yield(bond_yields, SETTLE_DATE)
             cprices_in = c(list(CLEAN_PRICES),list(CLEAN_PRICES),foreach(j=1:nrow(spr))%do%{ CLEAN_PRICES + epsilon*spr[j,] }) # second element will be DV01-shifted
             #yields_in = c(list(bond_yields),list(bond_yields + epsilon_rfr),foreach(j=1:(nrow(spr)))%do%{ bond_yields + epsilon*spr[j,] }) # second element will be DV01-shifted
             yields_in = foreach(p = cprices_in)%do%yield_from_c(p,SETTLE_DATE)
             
             ftd_devs = foreach(i=2:length(yields_in),.combine=rbind)%do%dFTD_two(yields_in[[1]], yields_in[[i]] + if(i==2) RFR else 0, if(i==2) { RFR + epsilon_rfr } else RFR, N_YEARS_IMPLY_SPREAD, N_YEARS_BASKET, RECOVERIES, cor_mat, rec_ratios)
             ftds = foreach(i=1:nrow(ftd_devs),.combine=c)%do%{ x = c_from_yield(ftd_devs[i,],SETTLE_DATE); x[2] - x[1] }
             ftd_devs = foreach(i=1:length(ftds),.combine=c)%do%(ftds[i]/(if(i==1) 100*epsilon_rfr else epsilon))
             # Each row of PR determines coefficients of corresponding correlated yield factor in the uncorrelated factors
             foreach(i=1:nrow(pr),.combine=cbind)%do%sum(ftd_devs[-1]*pr[i,1:(ncol(pr))])
                } else {
                    yields_in = c(list(bond_yields),list(bond_yields + epsilon_rfr),foreach(j=1:length(bond_yields))%do%{ bond_yields + ifelse(j==1:length(bond_yields),1,0)*epsilon }) # second element will be DV01-shifted
                    ftd0 = FTD_a(yields_in[[1]], RFR, N_YEARS_IMPLY_SPREAD - k/250, N_YEARS_BASKET - k/250, RECOVERIES, cor_mat, rec_ratios)
                    ftds = foreach(i=2:length(yields_in),.combine=c)%do%dFTD_a(yields_in[[1]], yields_in[[i]], if(i==2) { RFR + epsilon_rfr } else RFR, N_YEARS_IMPLY_SPREAD - k/250, N_YEARS_BASKET - k/250, RECOVERIES, cor_mat, rec_ratios)	
                    ftd_devs = foreach(i=1:length(ftds),.combine=c)%do%(ftds[i]/(if(i==1) epsilon_rfr else epsilon))
                    ftd_devs[2:3]
                }
            
                c(c_from_yield(bond_yields, SETTLE_DATE+k), w)
            }

            cbind(y1,y2,w12)
        }

        btr = matrix(0,nrow(w1234)-1,2)
        for(i in 2:nrow(btr)){
            btr[i,1] = sum((w1234[i,3:4]-w1234[i-1,3:4])*w1234[i-1,5:6])
            btr[i,2] = sum((w1234[i,9:10]-w1234[i-1,9:10])*w1234[i-1,11:12])
        }
        plot(cumsum(-btr[,2]))
        lines(cumsum(-btr[,1]))

        btr = matrix(0,nrow(w1234)-1,2)
        for(i in 2:nrow(btr)){
            btr[i,1] = (w1234[i,1]-w1234[i-1,1])*w1234[i-1,3]
            btr[i,2] = (w1234[i,2]-w1234[i-1,2])*w1234[i-1,3]
        }
        plot(cumsum(btr[,2]))
        lines(cumsum(btr[,1]))

        plot(w1234[,3])
        cor(w1234[,c(3,5)])
        w1234[1:10,]

        r = rmnorm(1000,c(0,0),cormat(-0.85,2)); r[,1]=r[,1]/sd(r[,1]); r[,2]=r[,2]/sd(r[,2]); r[,1]=r[,1]-mean(r[,1])+20; r[,2]=r[,2]-mean(r[,2])+150; cor(r); cov(r); 
        r = w1234[,c(1,3)]; r[,1]=r[,1]/sd(r[,1]); r[,2]=r[,2]/sd(r[,2]); r[,1]=r[,1]; r[,2]=r[,2]; cor(r); cov(r); 

        r = rmnorm(1000,c(10,10),cormat(-0.85,2)); r[1:10,]
        cov(r); cor(r);  plot(cumsum(foreach(i=2:nrow(r),.combine=c)%do%{ (r[i,1]-r[i-1,1])*r[i-1,2] }))
        
}


ftd_emp_matrices = function(){
	#cl <- makeForkCluster(32)
	#registerDoParallel(cl)

	tmp_file_1 = tempfile()

	#writeLines(gsub('NEWLINE','\n',POST$params), file(tmp_file_1))
	writeLines(gsub('NEWLINE','\n',POST$params), file('/home/slepnev/R/tmp_params_ftd.txt'))
	#p = read.csv(file=tmp_file_1,header=FALSE,sep=',',stringsAsFactors=FALSE)
	p = read.csv(file='/home/slepnev/R/tmp_params_ftd.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

	#writeLines(gsub('NEWLINE','\n',POST$params2), file(tmp_file_1))
	#p2 = read.csv(file=tmp_file_1,header=FALSE,sep=',',stringsAsFactors=FALSE)

	writeLines(gsub('NEWLINE','\n',POST$params2), file('/home/slepnev/R/tmp_params_ftd2.txt'))
	p2 = read.csv(file='/home/slepnev/R/tmp_params_ftd2.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

#stop()
	
    CLEAN_PRICES = as.numeric(p$V2[p$V1=='CLEAN_PRICE'])
	MATURITIES = as.Date(gsub('#','',p$V2[p$V1=='MATURITY']),format='%d.%m.%Y')
	COUPONS = 0.01*as.numeric(p$V2[p$V1=='COUPON'])
	COUPON_FREQS = as.numeric(p$V2[p$V1=='FREQUENCY'])
	CONVENTION = '30/360'

	RFR = as.numeric(p[p[,1]=='RFR:',2])
	REC_RATE = as.numeric(p[p[,1]=='Recovery rate:',2])
	N_YEARS_BASKET = as.numeric(p[p[,1]=='FTD years:',2])
	SETTLE_DATE = as.Date(gsub('#','',p[p[,1]=='Trade date:',2]))
	N_YEARS_IMPLY_SPREAD = as.numeric(MATURITIES - SETTLE_DATE)/365  # array-shaped and non-integer now

	
	yield_from_c = function(x, settle_date){
		foreach(j=1:length(x),.combine=c)%do%bond.yield(settle_date, MATURITIES[j], COUPONS[j], COUPON_FREQS[j], x[j], CONVENTION)
	}

	p = p2
	issue_names_param = p[p$V1=='ISSUE_NAME',2]
	p = p[p$V1!='ISSUE_NAME',]
	#price_idx = which(p[2,]=='PX_MID')
	#price_idx = which(p[2,]=='YLD_CNV_FROM_HIGH' | p[2,]=='YLD_CNV_FROM_LOW')
	price_idx = which(p[2,]=='YLD_CNV_BID' | p[2,]=='YLD_CNV_ASK')
	#price_idx = which(p[2,]=='YLD_CNV_LAST'); price_idx = c(price_idx,price_idx)
	
	name_idxs = which(p$V1=='Date')-1
	issue_names_hist = p$V1[name_idxs]
	
	start_idxs = which(p$V1=='Date')+1
	end_idxs = c(which(p$V1=='Date')[-1]-2,dim(p)[1])
	
	prices = xts(p[start_idxs[1]:end_idxs[1],price_idx],order.by=as.Date(gsub('#','',p[start_idxs[1]:end_idxs[1],1])))
	storage.mode(prices) = 'numeric'
	prices = prices[!is.na(prices[,1]) & !is.na(prices[,2])]
	prices = 0.5*(prices[,1] + prices[,2])
	
	for(i in 2:length(name_idxs)){
		tmp = xts(p[start_idxs[i]:end_idxs[i],price_idx],order.by=as.Date(gsub('#','',p[start_idxs[i]:end_idxs[i],1])))
	
		storage.mode(tmp) = 'numeric'
		tmp = tmp[!is.na(tmp[,1]) & !is.na(tmp[,2])]
		tmp = 0.5*(tmp[,1] + tmp[,2])
	
		tmp = tmp[index(tmp)%in%index(prices),]
		prices = prices[index(prices)%in%index(tmp),]
		prices = merge.xts(prices,tmp)
	}
	
	#ttms = foreach(m=MATURITIES,.combine=cbind)%do%(m-index(prices))
	#p1=prices/ttms

        prices = prices[index(prices)<SETTLE_DATE,]
    
	res = cor(tail(prices,63))
	rownames(res) = issue_names_hist
	colnames(res) = issue_names_hist
	res = res[issue_names_param,issue_names_param]
	for(i in c(125,250,500)){
		tmp_res = cor(tail(prices,i))
		rownames(tmp_res) = issue_names_hist
		colnames(tmp_res) = issue_names_hist
		tmp_res = tmp_res[issue_names_param,issue_names_param]
		res = rbind(res,tmp_res)
	}

	#stopCluster(cl)

	#write.table(res, sep=';', row.names = FALSE, file="C:\\Users\\slepnev-sokolinsky\\Documents\\src\\r_ftd_hist_matrices.csv", col.names=FALSE)
	cat(paste(t(res),collapse=';'))
	cat('+')

	res = yield_from_c(CLEAN_PRICES, SETTLE_DATE)
	#write.table(res, sep=';', row.names = FALSE, file="C:\\Users\\slepnev-sokolinsky\\Documents\\src\\r_ftd_hist_result.csv", col.names=FALSE)
	cat(paste(res,collapse=';'))
}
