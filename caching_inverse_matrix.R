 # Caching Inverse of a Matrix

# Example usage
# m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
# m2 <- makeCacheMatrix(m)
# cacheSolve(m2)
# [,1] [,2]
# [1,]  0.0    1
# [2,]  0.5    0
# cacheSolve(m2)
# inverse is cached
# [,1] [,2]
# [1,]  0.0    1
# [2,]  0.5    0

# Creates a matrix that can cache it's inverse
#
# Args:
#   x: A matrix (Optional)
#
# Returns:
#   A matrix with functions to get/set value & get/set inverse


makeCacheMatrix <- function(x = matrix()) 
## To store the result
{
    z <- NULL
    set <- function(y) ##used to define variables in different environment
	{
        x <<- y
        z <<- NULL
    }
    ## get is assigned an anonymous function which returns x
    get <- function() x
    setinversemat <- function(inversemat) z <<- inversemat
    getinversemat <- function() z
    ##returns a list of functions
    list(set=set,
	  get=get,
	  setinversemat=setinversemat,
	  getinversemat=getinversemat)
}
cacheSolve <- function(x, ...) 
## Return a matrix that is the inverse of 'x'
{
    z <- x$getinversemat()
    if(!is.null(z)) ## Check if there is cache Data available
	{
        message("getting cached data:=")
        return(z)##return result 
    }
    ##if cache data has no result then it caluclates the inverse
    data <- x$get()
    z <- solve(data, ...)
    ## update cache data with this value so that same operation, if done again, can be avoided
    x$setinversemat(z)
    z
}
