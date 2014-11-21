 ## Coursera - R Programming Assignment 2
 ## Martin Jones
 ##
 ## makeCacheMatrix 
 ## This function creates a matrix object that can cache its inverse.
 ## It takes one matrix as its argument. The matrix must be capable of being inversed.
 ##

 makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
 
        set <- function(y) {
             x <<- y
             inv <<- NULL
         }

        get <- function() x

        setinv <- function(i) inv <<- i
        getinv <- function() inv

        list(set = set, get = get,setinv = setinv,getinv = getinv)
  } 


 ## cacheSolve 
 ## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
 ## If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` will retrieve the inverse from the cache.
 
 cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
             print("getting cached data")
        return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
 }
