makeCacheMatrix <- function(x = matrix()) 
{
    z <- NULL
    set <- function(y) 
	{
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setinversemat <- function(inversemat) z <<- inversemat
    getinversemat <- function() z
    list(set=set,
	get=get,
	setinversemat=setinversemat,
	getinversemat=getinversemat)
}
cacheSolve <- function(x, ...) 
{
    z <- x$getinversemat()
    if(!is.null(z)) 
	{
        message("getting cached data:=")
        return(z)
    }
    data <- x$get()
    z <- solve(data, ...)
    x$setinversemat(z)
    z
}
