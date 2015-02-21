# Matrix inversion is a time consuming computation, and caching help store the result if the matrix is not changed so that we
# can use it particulalry if there is a loop computation involved. The  function CacheMatrix() and cacheSolve() offers the 
# following functionalities:

## makeCacheMatrix() return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve() first checks if the inverse has been already computed. If it gets the result it skips the computation otherwise
#it computes the inverse, sets the value in the cache via setinverse function. 
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinverse()
         if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
