##The first function, makeCacheMatrix creates a special " matrix ", which is really a list ##containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse 
    getinv <-  function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    # if the inverse has already been calculated
    
    if (!is.null(inv)){
        # get it from the cache and skips the computation. 
        
        message("getting cached data")
        return(inv)
    }
    # else, calculates the inverse 
    data <- x$get()
    inv <- inverse(data, ...)
    x$setinverse(inv)
    
    inv
}
