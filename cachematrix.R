## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
##--------------------------------------------------------------------------
## Creates a special "matrix" object that can cache its inverse
## steps:
## 1) c=rbind(c(1,0,4), c(1,3,4), c(4,1,0))
## 2) a<-makeMatrix()
## 3) a$(c)
## 4) cacheSolve(a)
##--------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}
##--------------------------------------------------------------------------
## Creates a special "matrix" object that can cache its inverse
## Takes the data stored in the class "cache" and solves it
##--------------------------------------------------------------------------
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}

##--------------------------------------------------------------------------
## Creates a special "vector" object that can cache its means
## Used as an example 
##--------------------------------------------------------------------------

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
cachemean <- function(x, ...) {
    m <- x$get()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data,...)
    x$setmean(m)
    m
}


 
    