## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
fig.path='graphics/stat'
)

## ----setup,include=FALSE,echo=FALSE--------------------------------------
options(keep.source = TRUE, width = 75)

## ----read, eval = FALSE--------------------------------------------------
#  library(bigmemory)
#  library(biganalytics)
#  x <- read.big.matrix("airline.csv", type="integer", header=TRUE,
#                        backingfile="airline.bin",
#                        descriptorfile="airline.desc",
#                        extraCols="Age")
#  summary(x)
#  
#  #                          min        max        mean        NA's
#  #Year                     1987       2008     1998.62           0
#  #Month                       1         12        6.55           0
#  #DayofMonth                  1         31       15.72           0
#  #DayOfWeek                   1          7        3.94           0
#  #ArrDelay                -1437       2598        7.05     2587529
#  #DepDelay                -1410       2601        8.17     2302136
#  #... (other variables omitted here) ...

## ----birth, eval = FALSE-------------------------------------------------
#  birthmonth <- function(y) {
#     minYear <- min(y[,'Year'], na.rm=TRUE)
#     these <- which(y[,'Year']==minYear)
#     minMonth <- min(y[these,'Month'], na.rm=TRUE)
#     return(12*minYear + minMonth - 1)
#  }

## ----run_birth, eval = FALSE---------------------------------------------
#  allplanes <- unique(x[,'TailNum'])
#  planeStart <- rep(0, length(allplanes))
#  for (i in allplanes) {
#    planeStart[i] <- birthmonth( x[mwhich(x, 'TailNum', i, 'eq'),
#                                c('Year', 'Month'), drop=FALSE] )
#  }

## ----bigsplit, eval = FALSE----------------------------------------------
#  library(bigtabulate)
#  planeindices <- bigsplit(x, 'TailNum')

## ----split, eval = FALSE-------------------------------------------------
#  planeindices <- split(1:nrow(x), x[,'TailNum'])

## ----sapply, eval = FALSE------------------------------------------------
#  planeStart <- sapply(planeindices,
#                    function(i) birthmonth(x[i, c('Year','Month'),
#                                             drop=FALSE]))

## ----parallel, eval = FALSE----------------------------------------------
#  library(doMC)
#  registerDoMC(cores=2)
#  planeStart <- foreach(i=planeindices, .combine=c) %dopar% {
#    return(birthmonth(x[i, c('Year','Month'), drop=FALSE]))
#  }

## ----ages, eval = FALSE--------------------------------------------------
#  x[,'Age'] <- x[,'Year']*as.integer(12) +
#            x[,'Month'] - as.integer(planeStart[x[,'TailNum']])

## ----biglm, eval = FALSE-------------------------------------------------
#  blm <- biglm.big.matrix(ArrDelay ~ Age + Year, data=x)
#  summary(blm)

## ----biglm_output, eval = FALSE------------------------------------------
#  #Large data regression model: biglm(formula = formula, data = data, ...)
#  #Sample size = 84216580
#  #               Coef    (95%     CI)     SE p
#  #(Intercept) 91.6149 87.6509 95.5789 1.9820 0
#  #Age          0.0144  0.0142  0.0146 0.0001 0
#  #Year        -0.0424 -0.0444 -0.0404 0.0010 0

