source("ftd_func.R")

print('CALCULATOR MESSAGE:  READING PARAMETERS')

p = read.csv(file="r_ftd_params.csv",header=FALSE,sep=',',stringsAsFactors=FALSE)

CLEAN_PRICES = as.numeric(p$V2[p$V1=='CLEAN_PRICE'])
MATURITIES = as.Date(gsub('#','',p$V2[p$V1=='MATURITY']), format='%d.%m.%Y')
COUPONS = 0.01*as.numeric(p$V2[p$V1=='COUPON'])
COUPON_FREQS = as.numeric(p$V2[p$V1=='FREQUENCY'])
CONVENTION = '30/360'

RFR = as.numeric(p[p[,1]=='RFR',2])
REC_RATE = as.numeric(p[p[,1]=='RR',2])
N_YEARS_BASKET = as.numeric(p[p[,1]=='FTDYears',2])
SETTLE_DATE = Sys.Date()
N_YEARS_IMPLY_SPREAD = as.numeric(MATURITIES - SETTLE_DATE)/365  # array-shaped and non-integer now

yield_from_c = function(x, settle_date){
	foreach(j=1:length(x),.combine=c)%do%bond.yield(settle_date, MATURITIES[j], COUPONS[j], COUPON_FREQS[j], x[j], CONVENTION)
}

cor_mat = matrix(as.numeric(p$V2[p$V1=='MATRIXvs']),length(MATURITIES),length(MATURITIES))

print('CALCULATOR MESSAGE:  GENERATING MONTE-CARLO PATHS')

D_GLOBAL <<- foreach(i=1:N_YEARS_BASKET)%do%rmnorm(1000000,array(0,length(MATURITIES)),cor_mat)

pr = eigen(cor_mat)$vectors
spr = solve(pr)
epsilon = 0.1 # 10 basis points
epsilon_rfr = 0.001 # 10 basis points

print('CALCULATOR MESSAGE:  RUNNING PARALLELIZED MONTE-CARLO CALCULATIONS')

yields_in = c(list(CLEAN_PRICES),list(CLEAN_PRICES),foreach(j=1:nrow(spr))%do%{ CLEAN_PRICES + epsilon*spr[j,] }) # second element will be DV01-shifted
ftds = foreach(i=1:length(yields_in),.combine=c, .packages=c('xts', 'foreach', 'jrvFinance'))%dopar%{
	yields_tmp = yield_from_c(yields_in[[i]], SETTLE_DATE) + if(i==2) epsilon_rfr else 0
	rfr_tmp = RFR + if(i==2) epsilon_rfr else 0
	FTD(yields_tmp, rfr_tmp)
}

ftd_devs = foreach(i=2:length(ftds),.combine=c)%do%((ftds[[i]]-ftds[[1]])/(if(i==2) epsilon_rfr else epsilon))

# Each row of PR determines coefficients of corresponding correlated yield factor in the uncorrelated factors
w_new = foreach(i=1:nrow(pr),.combine=cbind)%do%sum(ftd_devs[-1]*pr[i,])

print('CALCULATOR MESSAGE:  GENERATING OUTPUT')

write.table(c(RFR + ftds[1], ftd_devs[1], w_new), file="r_ftd_result.csv", row.names=FALSE, col.names=FALSE)



