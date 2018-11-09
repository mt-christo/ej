library(telegram)
library( CryptoTools )

source('/home/aslepnev/git/ej/tele.R')
source('/home/aslepnev/git/ej/spikestrat.R')

symbols = names( fromJSON( paste0( "https://min-api.cryptocompare.com/data/all/coinlist" ) )$Data )
currency = 'BTC'
CryptoTools_settings(cryptocompare = list(limit = 2000, storage_from = '2018-02-10'))
#for( symbol in paste( symbols, currency, sep = '/' ) )
for( symbol in symbols )
    if(length(list.files(paste0('~/CryptoCompare/CCCAGG/',symbol,'/BTC/'))) > 0)
        try( CryptoTools:::store_cryptocompare_data( gsub( '\\*', '_', paste0(symbol,'/BTC'),  ) ))

n = names_signal()
data = fromJSON( paste0( "https://min-api.cryptocompare.com/data/all/coinlist" ) )$Data
ids = as.data.frame(foreach(x=data,.combine=rbind)%do%c(symbol=x$Name,id=x$Id), stringAsFactors=FALSE)
for(x in n){
    sstats = fromJSON( paste0('https://www.cryptocompare.com/api/data/socialstats/?id=',ids$id[ids$symbol==x]) )$Data
    plot_symbol(x)
    bot$sendMessage(sstats$Reddit$link) 
    bot$sendMessage(sstats$Facebook$link) 
}
bot$sendMessage('Finished spike tests.')





