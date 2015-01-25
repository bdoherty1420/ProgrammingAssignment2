## These functions have the ability to cache the inversion of a matrix in order to
## save time running this computation for large matrices

## This function creates as special "matrix" that contains a function to:
## 1. set the value of the matrix 2. get the value of the matrix
## 3. set the value of the inverted matrix 4. get the value of the inverted
## matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinversion <- function(solve) i <<- solve
        getinversion <- function() i
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}


## This function calculates the inversion of the special "matrix" created
## with the above function. It checks to see if the inversion has been 
## calculated, in which case it gets the cached calculation, otherwise
## it calculates and set the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinversion()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinversion(i)
        i
}
