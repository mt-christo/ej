library(xts)
library(foreach)
library(data.table)
library(RCurl)
library(lubridate)


#max_date = as.Date('1900-01-01')
#for(d in c('~/FUT/RB','~/FUT/CL'))
#    if(length(c(grep('BARCHART',d),grep('files',d)))==0)
#        for(f in list.files(d,recursive=TRUE,full.names=TRUE))
#            max_date = max(max_date,max(as.Date(unlist(fread(f)[,1]),format='%m/%d/%Y')))

#hist = 1
#for(d in as.Date(as.Date('2015-01-01'):as.Date('2015-02-01'))){
#    d0 = strsplit(format(as.Date(d),format='%d/%m/%Y'),'/')[[1]]
#    x = getURL(paste0("http://acs.barchart.com/mri/data/mry",d0[2],d0[1],substr(d0[3],4,4),".asc"),userpwd="Alex702:140405")
#    if(length(grep('404 Not Found',x)) == 0){
#        hist = if(length(x_old) > 1) rbind(hist,fread(x)) else fread(x)
#    }
#}

x0 = fread(getURL(paste0("http://acs.barchart.com/mri/data/mry",d0[2],d0[1],substr(d0[3],4,4),".asc"),userpwd="Alex702:140405"))
a=x0$V1
ticker_map = data.frame(my=c('ZC','ZS','ZL','ZW','KE','LE','HE','GF','ZO','ZR','ZM'),
                        pablo=c('@C','@S','@BO','@W','@KW','@LE','@HE','@GF','@O','@RR','@SM'))
b=foreach(x=c('ZC','ZS','ZL','ZW','KE','LE','HE','GF','ZO','ZR','ZM'), .combine=c)%do%{
#b=foreach(x=c('@C','@S','@BO','@W','@KW','@LE','@HE','@GF','@O','@RR','@SM'), .combine=c)%do%{
    y = a[grep(x,substr(a,1,nchar(x)))]
    y = y[nchar(x)+2 == foreach(i=y,.combine=c)%do%nchar(i)]
#    y = y[!grepl('0',y) & !grepl('1',y) & !grepl('2',y) & !grepl('3',y)]
#    y = foreach(i=y,.combine=c)%do%paste0(substr(i,1,nchar(i)-1),1,substr(i,nchar(i),nchar(i)))
    y = foreach(i=y,.combine=c)%do%substr(i,1,nchar(i)-1)
    gsub(x,ticker_map$pablo[ticker_map$my==x],y)
}
#b = sort(unique(b))
write.csv(c(paste0(b,'15'),paste0(b,'16'),paste0(b,'17'),paste0(b,'18'),paste0(b,'19')),file='missing_tickers.csv')


if(FALSE){
    MONTH_CODES = data.frame(code = c('F','G','H','J','K','M','N','Q','U','V','X','Z'), n = 1:12, stringsAsFactors=FALSE)
    monthint = function(x) { MONTH_CODES$n[match(x, MONTH_CODES$code)] }

r = list()
for(n in as.vector(unlist(fread('~/FUT/Lots.txt', fill=TRUE)[,1]))){
    r[[n]] = list()
    for(m in MONTH_CODES$code){
        x_old = 1
        for(f in list.files(paste0('~/FUT/',n),recursive=TRUE,full.names=TRUE,pattern=paste0('*',m,'.txt'))){
            if(nchar(strsplit(f,'/')[[1]][6]) - nchar(n) - 4 == 3)
            {
                print(f)
                x = fread(f)
                x[,1] = as.Date(unlist(fread(f)[,1]),format='%m/%d/%Y')
                x = as.xts(x)
                i = which(diff(index(x))>500)
                if(length(i) > 0)
                    x = x[1:i[1]]

                r[[n]][[m]][[paste0(substr(as.character(year(max(index(x)))),1,2), substr(strsplit(f,'/')[[1]][6],nchar(n)+1,nchar(n)+2))]] = x
            }
        }
    }
}

save(r, file="CME-20170120.RData")
r = get(load(file="CME-20170120.RData"))

CALENDARS = list()
for(x in names(r)) CALENDARS[[x]] = monthint(names(r[[x]])[foreach(z=r[[x]],.combine=c)%do%(length(z) > 10)])
}

# name='FC'; months=c('Q','U','V'); weights=c(1,-2,1); oi_min=100
# name='FC'; months=c(3,4,5); weights=c(1,-2,1); oi_min=10; dt_start='2008-01-01'; dt_end='2012-01-01'
mmspread = function(name, months, weights, oi_min, dt_start='2011-01-01', dt_end='2018-01-01'){
    month_letters = if(months[1]%in%MONTH_CODES$code) months else MONTH_CODES$code[months]
    month_ns = MONTH_CODES$n[match(month_letters, MONTH_CODES$code)]
    m1 = head(month_ns,1); mn = tail(month_ns,1)
    months_exclude = if(m1 < mn) m1:mn else c(1:mn,m1:12)
    month_semi = if(m1 == 1) 12 else (m1 - 1)
    res = foreach(i = 1:length(month_letters),.combine='+')%do%{
        x = r[[name]][[month_letters[i]]]
        names(x) = c('Open','High','Low','Close','Volume','OpenInt')
        weights[i] * x[x$OpenInt >= oi_min]$Close
    } * length(month_letters) / foreach(i = 1:length(month_letters),.combine='+')%do%{
        x = r[[name]][[month_letters[i]]]
        names(x) = c('Open','High','Low','Close','Volume','OpenInt')
        x[x$OpenInt >= oi_min]$Close
    }
    100*res[index(res)>dt_start & index(res)<dt_end & !month(index(res))%in%months_exclude & !(month(index(res))==month_semi & day(index(res))>20)]
#    100*res[index(res)>dt_start & index(res)<dt_end]
}

# name='C'; months=c(3,5,7); weights=c(1,-2,1); min_oi=5; qts=c(0.3,0.7); dts=c('2009-01-01','2012-01-01')
qbasic = function(name, months, weights, min_oi, qts, dts){
#    t = rollapply(mmspread(name, months, weights, min_oi, '1990-01-01', '2018-01-01'), FUN=mean, 2)
    t = mmspread(name, months, weights, min_oi, '1990-01-01', '2018-01-01')
    twnd = t[index(t)>dts[1] & index(t)<dts[2]]
    qmin = as.numeric(quantile(twnd,qts[1]))
    qmax = as.numeric(quantile(twnd,qts[2]))
    plot.zoo(t[index(t)>dts[2]])
    abline(h=qmin)
    abline(h=qmax)
}

# name='LC'; weights=c(1,-2,1); min_oi=5; qts=c(0.25,0.75); an_dts=c('2000-01-01','2012-01-01'); max_date='2018-01-01'
# name='S'; weights=c(1,-2,1); min_oi=5; qts=c(0.25,0.75); an_dts=c('2000-01-01','2012-01-01'); max_date='2018-01-01'
# name='S'; weights=c(1,-2,1); min_oi=5; qts=0; an_dts=c('2000-01-01','2012-01-01'); max_date='2018-01-01'
# name='FC'; weights=c(1,-2,1); min_oi=5; qts=0; an_dts=c('2000-01-01','2012-01-01'); max_date='2018-01-01'
qspikes = function(name, weights, min_oi, qts, an_dts, max_date){
    cal = CALENDARS[[name]]
    res = foreach(i = 1:length(cal),.combine=rbind)%do%{
        months = c(cal,cal)[i:(i+length(weights)-1)]
        t = mmspread(name, months, weights, min_oi, an_dts[1], max_date); #plot(t)
        t1 = t[index(t) > an_dts[1] & index(t) < an_dts[2]]
        t2 = t[index(t) > an_dts[2]]

        print(i)
        if(length(t1)>100 && length(t2)>100){
            q1min = if(length(qts)>1) as.numeric(quantile(t1, qts[1])) else mean(t1)
            q1max = if(length(qts)>1) as.numeric(quantile(t1, qts[2])) else mean(t1)
            t2_years = unique(year(index(t2)))
            
            above_cnt = foreach(y=t2_years,.combine=c)%do%sum(t2[year(index(t2))==y] < q1min)
            below_cnt = foreach(y=t2_years,.combine=c)%do%sum(t2[year(index(t2))==y] > q1max)
            c(name,months, if(above_cnt==0 && below_cnt==0) 100 else min(min(above_cnt),min(below_cnt)))
        }
    }

    colnames(res) = c('name',paste0('month',1:length(weights)),'flag')
    as.data.frame(res, stringsAsFactors=FALSE)
}

s = foreach(name = c('FC','LC','LH','C','S'),.combine=rbind)%do%qspikes(name, c(1,-2,1), 50, c(0.25,0.75), c('2009-01-01','2012-01-01'), '2018-01-01')
s[s$flag>0,]

s = foreach(name = c('FC','LC','LH','C','S'),.combine=rbind)%do%qspikes(name, c(1,-1), 5, c(0.25,0.75), c('2009-01-01','2012-01-01'), '2018-01-01')
s[s$flag>0,]

s = foreach(name = c('FC','LC','LH','C','S'),.combine=rbind)%do%qspikes(name, c(1,-1), 5, 0, c('2009-01-01','2012-01-01'), '2018-01-01')
s[s$flag>0,]

qbasic('C', c(3,5,7), c(1,-2,1), 5, c(0.3,0.7), c('2009-01-01','2012-01-01'))


plot(t2); abline(h=q1min); abline(h=q1max)


#t = mmspread('FC', c('Q','U','V'), c(1,-2,1), 100, '1990-01-01', '2018-01-01')

plot(mmspread('FC', c('Q','U','V'), c(1,-2,1), 100, '2001-01-01', '2018-01-01'))
plot(mmspread('LH', c(2,4,5), c(1,-2,1), 5, '2012-01-01', '2018-01-01'))
# c('F','G','H','J','K','M','N','Q','U','V','X','Z')
qbasic('FC', c(8,9,10), c(1,-2,1), 100, '2012-01-01', '2015-01-01')
qbasic('FC', c('Q','U','V'), c(1,-2,1), 100, '2001-01-01', '2015-01-01')
qbasic('LC', c('J','M','Q'), c(1,-2,1), 2000, '2000-01-01', '2012-01-01')


qbasic('FC', c('H','J','K'), c(1,-2,1), 100, c(0.3,0.7), c('2001-01-01', '2010-01-01'))
qbasic('LC', c('G','J','M'), c(1,-2,1), 100, c(0.3,0.7), c('2001-01-01', '2010-01-01'))
qbasic('FC', CALENDARS[['FC']][0+1:3], c(1,-2,1), 100, c(0.3,0.7), c('2001-01-01', '2008-01-01'))
qbasic('FC', CALENDARS[['FC']][1+1:3], c(1,-2,1), 100, c(0.3,0.7), c('2001-01-01', '2008-01-01'))
qbasic('FC', CALENDARS[['FC']][2+1:3], c(1,-2,1), 100, c(0.3,0.7), c('2001-01-01', '2008-01-01'))
qbasic('FC', CALENDARS[['FC']][3+1:3], c(1,-2,1), 100, c(0.3,0.7), c('2001-01-01', '2008-01-01'))
qbasic('FC', CALENDARS[['FC']][4+1:3], c(1,-2,1), 100, c(0.3,0.7), c('2001-01-01', '2008-01-01'))
qbasic('FC', CALENDARS[['FC']][5+1:3], c(1,-2,1), 100, c(0.3,0.7), c('2001-01-01', '2008-01-01'))

qbasic('LC', CALENDARS[['LC']][1+1:3], c(1,-2,1), 7000, c(0.1,0.9), c('1995-01-01', '1998-01-01'))

t = mmspread('FC', CALENDARS[['FC']][1+1:3], c(1,-2,1), 10, '1999-01-01', '2018-01-01')
d = as.Date(foreach(y = unique(year(index(t))),.combine='rbind')%do%{
    x = t[year(index(t))==y]
    index(x)[which.min(x)]
})
hist(month(d))

qbasic('FC', CALENDARS[['LH']][1+1:3], c(1,-2,1), 10, '2008-01-01', '2012-01-01')

qbasic('LH', CALENDARS[['LH']][1+1:3], c(1,-2,1), 10, '2008-01-01', '2012-01-01')




t1 = foreach(y = unique(year(index(t))),.combine='merge.xts')%do%{
    x = t[year(index(t))==y]
    year(index(x)) = year(index(x)) - y
    x
}
t1 = na.locf(t1)
t1 = t1[0==rowSums(is.na(t1))]

rngspread = function(name, months, weights){
    s = mmspread(name, months, weights, 500, '2009-01-01', '2018-01-01')
    res = foreach(y = unique(year(index(s))),.combine='rbind')%do%range(s[year(index(s))==y])
    min(res[,2]) - max(res[,1])
}

plot(mmspread('FC', c('Q','U','V','X','F'),c(1,-4,6,-4,1), 1000, '2009-01-01', '2018-01-01'))

rngspread('FC', c('Q','U'),c(1,-1))
rngspread('FC', c('U','V'),c(1,-1))
rngspread('FC', c('Q','U'),c(1,-1))
rngspread('FC', c('Q','U'),c(1,-1))
rngspread('FC', c('U','V','X'))
rngspread('FC', c('Q','U','V'))
rngspread('FC', c('Q','U','V'))
rngspread('FC', c('Q','U','V'))





MONTH_CODES$code[CALENDARS[['LC']][1+1:3]]
n = c('LC/','','')
r = list()
for(n in as.vector(unlist(fread('~/FUT/Lots.txt', fill=TRUE)[,1]))){
    r[[n]] = list()
    for(m in month_codes$code){
        x_old = 1
        for(f in list.files(paste0('~/FUT/',n),recursive=TRUE,full.names=TRUE,pattern=paste0('*',m,'.txt'))){
            if(nchar(strsplit(f,'/')[[1]][6]) - nchar(n) - 4 == 3)
            {
                print(f)
                x = fread(f)
                x[,1] = as.Date(unlist(fread(f)[,1]),format='%m/%d/%Y')
                x = as.xts(x)
                i = which(diff(index(x))>500)
                if(length(i) > 0)
                    x = x[1:i[1]]
                
                if(length(x_old) > 1){
                    x1 = x_old[index(x_old)%in%index(x)]
                    x2 = x[index(x)%in%index(x_old)]
                    x12 = if(nrow(x1)==0) x1 else rbind(x1[x1[,6] >= x2[,6]], x2[x2[,6] > x1[,6]])
                    x_old = rbind(x_old[!index(x_old)%in%index(x)], x12, x[!index(x)%in%index(x_old)])
                } else {
                    x_old = x
                }
            }
        }

        r[[n]][[m]] = x_old
    }
}


