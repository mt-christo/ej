# FACTOR MODEL

METRIC_NORM_OVER_UNI = c('pe', 'div')
METRIC_NORM_OVER_SECTOR = c('logmcap', 'sharpe')

LOADING_NORM_OVER_UNI = c('value', 'size')
LOADING_NORM_OVER_SECTOR = c('momentum', 'volatility')

BASIC_MODEL = list(value = list(c('pe', 'div'), c(1, 1)),
                   volatility = list(c('sigma20', 'sigma40'), c(1, 1)),
                   size = list('logmcap', 1),
                   momentum = list(c('pm12m1m', 'sharpe'), c(1, 1)))


# -- FACTOR MODEL

