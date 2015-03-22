## These functions take an invertible matrix and produce the inverse
## of that matrix. If the inverse is already cached, then the function
## just returns the cached value, without solving the inverse again.

## This function take a matrix as its argument and stores it in x 
## It returns a list of sub-functions which can be called from the
## next function to process the matrix data.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { ##sets the values for the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x ##displays current matrix
        setinv <- function(inv) i <<- inv ##sets the values for the inverse
        getinv <- function() i ##displays current inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv) ##list of all sub-functions
}

## This function checks for the presence of cached inverse of the
## matrix, if present it just returns it or else calculates the inverse
## sets it and the prints it.

cacheSolve <- function(x, ...) {
        i <- x$getinv() ##stores inverse
        if(!is.null(i)) { ##if inverse is cached
                message("Getting cached matrix inverse")
                return(i)
        }
        else {
                message("No cached data")
        }
        message("Setting new matrix inverse")
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}



