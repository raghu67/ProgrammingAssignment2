## makeCacheMatrix and cacheSolve provide conveinent methods for efficiently computing the
## inverse of a matrix. If the inverse has already been calculated, it is 
## cached for future use.
## Assumption: It is assumed the input matrix is invertible and no error checking is performed

## makeCacheMatrix function creates a Caching version of the matrix from a regular matrix.
## It returns a list with functions for setting/getting the original matrix 
## and the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        ##Initialize the inverse to NULL
        inverse <- NULL
        ## Update the original matrix
        set <- function(y) {
                x <<- y
        ## Reset the cacheed inverse matrix to force a recompute
                inverse <<- NULL
        }
        ## Return the original matrix
        get <- function() x
        ## Update the Inverse
        setinverse <- function(inv)  inverse <<- inv
        ## Return the cached Inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve function computes the inverse of a special "Caching Matrix" 
## created by the makeCacheMatrix function. How ever, it first checks if 
## a cached version is available and returns the cached version. If not,
## it computes the inverse and stores it for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                ## Return the cached Inverse if available
                message("getting cached data")
                return(inv)
        }
        ## Get the original matrix and compute the inverse
        data <- x$get()
        inv <- solve(data, ...)
        ## Cache the computed inverse matrix for future use
        x$setinverse(inv)
        ## Return the computed/cached inverse
        inv       
}
