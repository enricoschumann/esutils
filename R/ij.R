## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-
## Time-stamp: <2015-06-17 16:42:35 CEST (es)>

## ij_i <- function(i, j, nrow) {
##     if (is.matrix(i))
##         i[ ,1L] + (j[ ,2L] - 1) * nrow
##     else        
##         i + (j - 1) *nrow
## }

## i_ij <- function(index, nrow) {
##     x <- index %% nrow
##     i <- ifelse(x == 0, nrow, x)
##     j <- ifelse(x == 0, index%/%nrow, index%/%nrow + 1)
##     cbind(i,j)
## }


## z <- array(runif(100),dim=c(5,3))
## i <- 3; j <- 2
## ij_i(3,2, nrow(z))

## # test 1: extract element with i,j and with index
## z[i,j]
## z[ij_i(i,j,dim(z)[1L])]

## # test 2: get i,j back from index
## index <- getIndex(i,j,dim(z)[1L])
## getIJ(index,dim(z)[1L])

## # vectorised
## index <- 1:7
## getIJ(index,dim(z)[1L])

## i <- rep(1,3); j <- 1:3
## z[cbind(i,j)]
## z[getIndex(i,j,dim(z)[1L])]
