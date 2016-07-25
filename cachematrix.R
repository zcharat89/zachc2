
## The function makeCacheMatrix builds a cached matrix object to be 'inversed' by its sibling function, CacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    Z = NULL
    set = function(y) {
        x <<- y
        Z <<- NULL
}
    get = function() x
    setInverse = function(solve) Z <<- solve
    getInverse = function() Z
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The function cacheSolve computes the inverse of the cached matrix produced from makeCacheMatrix function which
## utilizes the 'solve' function 

cacheSolve <- function(x, ...) {
    Z = x$getInverse()
    Q <- x$get()
    Z = solve(Q, ...)
    x$setInverse(Z)
    Z
  
}

