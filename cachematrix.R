## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

 makeCacheMatrix <- function (x=matrix()){
  m <-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
  }

## get the value of the matrix
  get <- function() x 

## set the inverse of the matrix
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m

 ## get the inverse of the matrix
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
  }

## cacheSolve returns a matrix that is the inverse of 'x'
 cacheSolve <- function( x,...){
 m<-x$getmatrix()

## check if the matrix is cached or not, if the matrix is existing, the cached value is retrieved with a message, or else value is recalculated
 if(!is.null(m)){
 message("getting cached data")
 return(m)
  }

## value is recalculated
  data <- x$get()
  m<- solve(data,...)
  x$setmatrix(m)
  m
  }

##example on the above code
##matrix assigned to l
##  l = cbind(c(4,1), c(3, 1))     
##  m = makeCacheMatrix(l)   
##  m$get()
##    [,1] [,2]
## [1,]    4    3
## [2,]    1    1
## cacheSolve(m)   ## Inverse of matrix l calculated
## [,1] [,2]
##[1,]    1   -3
##[2,]   -1    4
## cacheSolve(m)                 ##gets the cahed value, no extra calculation done
## getting cached data
##    [,1] [,2]
## [1,]    1   -3
## [2,]   -1    4