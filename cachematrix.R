### Matrix inversion is usually a costly computation; these functions will cache
### the inverse of the matrix rather than computing it repeatedly

### Assumption: the matrix supplied is always invertible

## makeCacheMatrix: creates a special "matrix" object that can cahce its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by 
#              makeCacheMatrix; if inverse has already been calculated 
#              (and matrix has not changed), then cacheSolve will retreive 
#              the inverse of the cache

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}


