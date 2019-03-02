new = c('xts','igraph','corpcor','parallel','dplyr','lubridate','data.table','foreach','quantmod','mnormt','RCurl','googlesheets','doMC','mailR','tseries','nloptr','mvtnorm')
install.packages(new)

install.packages(new, Ncpus = 6)
