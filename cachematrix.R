## Assignment 2: Caching the inverse of a Matrix
## Matrix inversion is usually a costly computation.
## There maybe some benefit to caching the inverse of a matrix instead of compute it repeatedly.
## This assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache the
## inverse of a matrix

## This function creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function computes the inverse of the matrix that can cache its inverse.
## If the inverse has already been computed, then the cacheSolve function should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        ## Return a matrix that is the inverse of 'x'
        message ("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv        
}
