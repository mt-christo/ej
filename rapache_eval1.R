library(doMC) 
#library(doParallel) 
library(foreach)
library(xts)
library(chron)
library(jrvFinance) 
library(mnormt)
library(fExoticOptions); 
library(mvtnorm)
library(RJDBC)
library(psych)
#library(Matrix)

registerDoMC(35)

#registerDoParallel(cores=45)

#	cl <- makeForkCluster(60)
#	registerDoParallel(cl)

source('/home/slepnev/R/db_func.R')
source("/home/slepnev/R/ftd_func.R")
source("/home/slepnev/R/convert_func.R")
source("/home/slepnev/R/option1_func.R")
source("/home/slepnev/R/file_func.R")
source("/home/slepnev/R/rtsopttest_func.R")





