# Together, functions calculate the inverse of a matrix, and store it in cache
# if the inverse has already been calculated for the given matrix, then it gets
# the inverse from cache instead of recalculating.


# creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # makes the inverse of the matrix NULL
    i <- NULL
    
    # creates a function, set, where matrix inverse will be cached
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # gets the value of the inverse matrix
    get <- function() x
    
    # uses solve to calculate the inverse matrix
    setInv <- function(solve) i <<- solve
    
    # gets the inverse matrix
    getInv <- function() i
    
    # passes makeCacheMatrix values
    list(set = set, get=get, setInv=setInv, getInv=getInv)
}


# computes or gets the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    i <- x$getInv()
    
    # gets the inverse of the matrix, if matrix was already cached
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # for matrixes that are not cached
    # assigns the value of the matrix inverse to data
    data <- x$get()
    # calculates matrix inverse
    i <- solve(data,...)
    # caches the matrix inverse
    x$setInv(i)
    i
}
