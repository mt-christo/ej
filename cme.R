library(xts)
library(foreach)
library(data.table)
library(RCurl)

max_date = as.Date('1900-01-01')
for(d in c('~/FUT/RB','~/FUT/CL'))
    if(length(c(grep('BARCHART',d),grep('files',d)))==0)
        for(f in list.files(d,recursive=TRUE,full.names=TRUE))
            max_date = max(max_date,max(as.Date(unlist(fread(f)[,1]),format='%m/%d/%Y')))

foreach(n = as.vector(unlist(fread('~/FUT/Lots.txt', fill=TRUE)[,1])), .combine=rbind)%do%{
    x_old = NA
    for(f in list.files(paste0('~/FUT/',n),recursive=TRUE,full.names=TRUE)){
        x = fread(f)
        x$Date = as.Date(unlist(fread(f)[,1]),format='%m/%d/%Y')
        x = as.xts(x)

        if(!is.na(x_old)){
            x1 = x_old[index(x_old)%in%index(x)]
            x2 = x[index(x)%in%index(x_old)]
            x12 = rbind(x1[x1$OpenInt >= x2$OpenInt], x2[x2$OpenInt > x1$OpenInt])
            x_old = rbind(x_old[!index(x_old)%in%index(x)], x12, x[!index(x)%in%index(x_old)])
        } else {
            x_old = x
        }
            
        
    }
    x_old$Name = n
}
