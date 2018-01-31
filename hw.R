library(QuantTools)

Rcpp::sourceCpp('hw.cpp')

ticks = data.table(time = c('2017-01-01','2017-01-01','2017-01-01'), price = c(1,2,1), volume=c(1,1,1), id = 1:3)
