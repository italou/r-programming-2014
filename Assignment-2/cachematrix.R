## The following functions ease the process to calculate matrix inversion
## by caching already solved matrix.
## It's assumed that the given matrix is inversible.


## Creates the "special" matrix. Add the necessary functions for caching.
makeCacheMatrix <- function(x = matrix(as.numeric(sample(16, replace=T)), 4, 4)) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'. If already calculated, then 
## it returns via cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Example (remove comment markers):
## print("creating matrix -> m")
## m <- matrix(as.numeric(sample(16, replace=T)), 4, 4)
## print("creating special matrix, adding functions -> cm")
## cm <- makeCacheMatrix(m)
## print("solving inverse. not cached so solve() is called -> mi")
## mi <- cacheSolve(cm)
## print("solving again. inverse is retrieved from cache -> mi")
## mi <- cacheSolve(cm)
## print("creating special inversed matrix, adding functions -> cmi")
## cmi <- makeCacheMatrix(mi)
## print("solving inversed inverse. not cached so solve() is called -> mii")
## mii <- cacheSolve(cmi)
## print("once more... getting from cache -> mii")
## mii <- cacheSolve(cmi)
## print("m -> original matrix")
## m
## print("mi -> original matrix inversed ")
## mi
## print("mii -> inverse matrix inversed")
## mii
## print("m equals mii?")
## all.equal(m, mii)