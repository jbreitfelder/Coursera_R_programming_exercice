## This function creates a special "matrix" that is actually a list with functions 
## to set/get the matrix and set/get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## function to set the matrix in the cache
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## function to get the matrix from the cache
        get <- function() x
        ## function to set the inverse in the cache
        setinverse <- function(inverse) inv <<- inverse
        ## function to get the inverse from the cache
        getinverse <- function() inv
        ## The code returns a list of the for functions listed above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if the inverse of x is in in the cache.
## If it is, and x has not changed, it reads the inverse from the cache an returns that value
## Otherwise, it calculates the inverse using the function 'solve', and then stored it in the cache.
cacheSolve <- function(x, ...){ 
        inv <- x$getinverse()
        ## If inv is not NULL it means the inverse of x was stored in the cache.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## If the inverse was not stored in the cache yet we will compute it here :
        inv <- solve(data, ...)
        ## and store it in the cache for the next time we need it :
        x$setinverse(inv)
        ## The function returns the inverse of the matrix x
        inv
}