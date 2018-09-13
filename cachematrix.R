## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invM <<- NULL                   # Use NULL as default to initialize the inverse of a matrix.
        setMatrix <- function(y) {      # Function to set the matrix.
                x <<- y                 # Pass matrix value y from other environment to x.
                invM <<- NULL           # Clear cache
        }
        getMatrix <- function() x       # Get the value of x using the lexical scoping feature.
        setInv <- function(solve) invM <<- solve        # Calculate and set the inverse of a matrix.
                                                        # solve function is passed from the global environment.
        getInv <- function() invM       # Get the value of the inverse of the matrix from other environment.
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInv()              # Get cached value of inverse of the matrix.
        if(!is.null(invM)){             # Compare to find out if it is not null matrix
                message("getting cached data") 
                return(invM)            # If the inverse is not NULL then return the value.
        }
        data <- x$getMatrix()           # Otherwise get the matrix
        invM <- solve(data, ...)        # and solve to find out inverse
        x$setInv(invM)                  # set inverse to cache it for next iteration.
        invM                            # Output inverse of the matrix
        }

# Test Set
x <<- matrix(1:4, 2, 2)
mcm <- makeCacheMatrix(x)   # pass matrix value to the function
mcm$getMatrix()             # retrieve the value of Matrix
mcm$getInv()                # retrieve the value of invM, which should be NULL
y <- matrix(4:7, 2, 2)      # reset value with a new Matrix
mcm$setMatrix(y)            # reset value with a new Matrix
mcm$getMatrix()             # get the value of new matrix loaded into the function
cacheSolve(mcm)             # notice inverse calculated is inverse of new matrix
mcm$getInv()                # retrieve it directly, now that it has been cached