## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Prepare Cache Matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## inverse function using solve function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data.matrix <- x$get()
        inv <- solve(data.matrix)
        x$setinverse(inv)
        return(inv)
}
