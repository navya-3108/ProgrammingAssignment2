## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL  # Initialize the inverse to NULL
    # Function to set the matrix and reset the cached inverse
    set <- function(y) {
    x <<- y
        m_inverse <<- NULL
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse
    setinverse <- function(inverse) m_inverse <<- inverse
    
    # Function to get the inverse
    getinverse <- function() m_inverse
    
    # Return a list of the four functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function, cacheSolve, computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, it retrieves the inverse from the cache.
## Otherwise, it calculates the inverse and stores it in the cache for future use.
cacheSolve <- function(x, ...) {
    # Get the cached inverse
    m_inverse <- x$getinverse()
    
    # Check if the inverse has already been calculated
    if (!is.null(m_inverse)) {
        message("getting cached data")
        return(m_inverse) # Return the cached inverse
    }
    
    # If the inverse is not in the cache, get the matrix
    data <- x$get()
    
    # Compute the inverse using the solve() function
    m_inverse <- solve(data, ...)
    
    # Cache the computed inverse
    x$setinverse(m_inverse)
    
    # Return the inverse
    m_inverse
}
