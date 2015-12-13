## The following two functions are used to cache the inverse of a matrix
## in order to save computational time if possible.

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() mat
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


# The following function returns the inverse of the matrix after checking
# whethere it has already been computed. if it has, it gets the result and skips the
# computation. If not, it computes the inverse and sets the value in the cache.

cacheSolve <- function(mat, ...) {
    inv <- mat$getInv()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- mat$get()
    inv <- solve(data)
    mat$setInv(inv)
    inv
}
