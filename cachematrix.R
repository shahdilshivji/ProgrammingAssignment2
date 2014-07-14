#These functions create a matrix and then cache's the inverse of that matrix


#This function creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = numeric()) 
{
        m <- NULL
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) m <<- solve #assigning the inverse of the matrix
        getsolve <- function() m 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#This function computes the inverse of the matrix above, and then caches that inverse.
cacheSolve <- function(x, ...) 
{
        m <- x$getsolve()
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m #returning the inverse of the matrix
}
