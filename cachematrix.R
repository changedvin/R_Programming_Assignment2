## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInvrs <- function(solveMat) inv <<- solveMat
    getInvrs <- function() inv
    list(set = set, get = get, setInvrs = setInvrs, getInvrs = getInvrs)
    
}



cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
