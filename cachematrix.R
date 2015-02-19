## The first function, makeCacheMatrix creates a matrix,
## sets its value, gets its value, 
## sets value of inverse, gets value of the inverse matrix 
## second function, cacheSolve calculates the inverse of the matrix 
## created with the makeCacheMatrix function but first checks to see
## if the mean has already been calculated. 
## If it has it gets it from cache rather than recalculating it.


## This function creates a special "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) 
                m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
