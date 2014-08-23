## R-Programming Assignment 2
## Functions to allow for special matrices, capable of caching their inverse

## Returns a cacheable Matrix which can store its inverse when calculated
## by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    
    list(set=set, get=get, 
         setinv=setinv, 
         getinv=getinv)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message('from cache')
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setinv(inv)
    inv
}
