## Lexical Scoping is one form of RULES that define the process for associating 
## symbols with values.  It states that the values of the free variables are 
## defined by the bindings that were in effect at the time the function
## was created

## makeCacheMatrix is a function closure indicating that it is a function
## together with its environment

## This line indicates that makeCacheMatrix is a function with a single 
## argument. The argument is a matrix.  The function passes other functions: set,
## get, setSolve, and getSolve.

makeCacheMatrix <- function(x = matrix()) { # x is a formal parameter
    m <- NULL  #
    set <- function(y) { # This function returns x
        x <<- y
        m <<- NULL  
    }
    get <- function() x  ## Returns the argument "x"
    setSolve <- function(solve) m <<- solve  #setSolve performs the iteration
    getSolve <- function() m  # Retrieves values from archive
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) { #if m is not null than get the CACHED values
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
