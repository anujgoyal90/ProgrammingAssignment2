## This program is used to cache the inverse of a matrix. Since calculating the inverse of a matrix is a time-consuming
## process, it can be useful to save the results and pull them up as needed

## The makeCachMatrix creates a matrix object which is capable of holding its inverse
## This creates a list containing a function to
##1. set the value of a vector
##2. get the value of a vector
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y){
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setInverse <- function (inverse) inv <<- inverse
    getInverse <- function() inv
    list(set =set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function first checks to see if the inverse is already calculated.
## If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the matrix and sets it in the cache

cacheSolve <- function(x) { ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
