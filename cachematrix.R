##Hemanshu Singh
## There are two functions in this file. The first creates a matrix that can cache
## its inverse, and the second calculates the inverse, retrieving the cached value
## if it is available.


## Returns list with four functions that read and write a matrix,
## as well as read and write the inverse of that matrix.
makeCacheMatrix <- function(mx = matrix()) {
    inverse <- NULL
    set <- function(m) {
        mx <<- m
        inverse <<- NULL
    }
    get <- function() mx
    set_inverse <- function(i) inverse <<- i
    get_inverse <- function() inverse
    
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Gets the inverse of a matrix. If the value is cached, it will get that value.
## If not, it will solve for the inverse and then store the value in the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$set_inverse(inverse)
    inverse
}
