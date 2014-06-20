## Functions will cache the inverse of a matrix to limit potential 
## costly computation.


## makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                
                inv <- NULL                

                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }                
                
                get <- function() x
                
                setinverse <- function(solve) inv <<- solve  # Invert matrix x: solve(x)
                
                getinverse <- function() inv                
                
                # List containing functions
                list(set = set, 
                     get = get,
                     setinverse = setinverse,
                     getinverse = setinverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has been 
## calculated (and the matrix has not changed), the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

                inv <- x$getinverse()
        
                # Check if the inverted matrix inv is not null (not empty)
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)  # Cached inverted matrix is returned
                }
                
                data <- x$get()        

                inv <- solve(data, ...)
                
                x$setinverse(inv)
                
                inv  # Inverted matrix                
}