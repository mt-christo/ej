library( CryptoTools )
symbols = names( fromJSON( paste0( "https://min-api.cryptocompare.com/data/all/coinlist" ) )$Data )
currency = 'BTC'
CryptoTools_settings(cryptocompare = list(limit = 2000, storage_from = '2018-02-10'))
#for( symbol in paste( symbols, currency, sep = '/' ) )
for( symbol in symbols )
    try( CryptoTools:::store_cryptocompare_data( gsub( '\\*', '_', symbol ) ))
