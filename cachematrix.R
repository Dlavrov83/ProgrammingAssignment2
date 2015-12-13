## The following two functions are used to cache the inverse of a matrix
## in order to save computational time if possible.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() mat
    setmean <- function(inverse) inv <<- inverse
    getmean <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


# The following function returns the inverse of the matrix after checking
# whethere it has already been computed. if it has, it gets the result and skips the
# computation. If not, it computes the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
