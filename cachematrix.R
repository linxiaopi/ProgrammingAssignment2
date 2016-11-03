## Creates a function that will create an object to store a matrix and its
## inverse along with functions within the object to set and retrieve them;
## creates another function that computes the inverse and caches it in the
## object, or retrieves the inverse from the cache

## Store matrix and create object to cache inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        
        setI <- function(inv) {
                Inv <<- inv
        }
        
        getI <- function() Inv
        
        list(set = set, get = get, setI = setI, getI = getI)
}


## Retrieve cahced inverse of matrix or compute inverse of matrix and cache it
## in makeCacheMatrix if nothing cached

cacheSolve <- function(x, ...) {
        Inv <- x$getI()
        if(!is.null(Inv)) {
                message("Getting cached data")
                return(Inv)
        }
        
        data <- x$get()
        Inv <- solve(data, ...)
        x$setI(Inv)
        Inv
}
