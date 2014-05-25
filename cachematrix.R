## function 'makeCacheMatrix()' creates a matrix and enables cacheing of computed
## function values using a list to store values

## Function (1) sets values of a matrix (2) gets matrix values (3) sets values of
## inverse of the matrix and (4) gets values for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) Inv <<- solve
        getInv <- function() Inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## function 'cachesolve()' caculates the inverse of the matrix created with the
## 'makeCacheMatrix()' function.  If the inverse has already been calculated, the
## function retrieves the cached value from the list created within the
## 'makeCacheMatrix()' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInv()
        if (!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInv(Inv)
        Inv
        
}
