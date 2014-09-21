## Below are two functions: makeCacheMatrix and cacheSolve.
## The goal is to store precomputed matrix inverses because the solve()
## function can be time-intensive for large matrices.
## Check my functions for the assignment here without fuss or explanation.
## Beneath the functions, I walk through the code step-by-step
## If I explain something incorrecty or strangely, please comment!
makeCacheMatrix <- function(x = matrix()) { 
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) i <<- inverse 
     getinv <- function() i
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)     
}

cacheSolve <- function(x, ...) {
     i <- x$getinv()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinv(i)
     i        
}


## makeCacheMatrix(x) returns a list of functions, and stores and returns
##   both a value of a matrix "x" (the only argument)
##   and its inverse "i" (or whatever you store in the variable arbitrarily)
##   that are found in this specific makeCacheMatrix environment for "x"

## cacheSolve(makeCacheMatrix(x)) looks within the environment created by makeCacheMatrix(x) to either:
## 1) return the inverse "i" associated with the list created by makeCacheMatrix(x) (the "i" in the environment)
## OR 2) compute the inverse of matrix "x" and store it as variable "i" within the list environment created by makeCacheMatrix(x)

## More specifically, the "makeCacheMatrix" function returns a list of functions that:
## 1) set the value of a matrix to x
## 2) get the value of the matrix x
## 3) set the value of the inverse of x
## 4) get the value of the inverse of x
## Important: x can be any matrix. For our "cachesolve" function to work, x must be invertible

makeCacheMatrix <- function(x = matrix()) { 
     ## set i variable to null. It will eventually hold the inverse of x, if it has been precomputed
     i <- NULL
     ## define function "set" to define x and set i to null within makeCacheMatrix environment
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     ## define function "get" to return value of x
     get <- function() x
     ## define function "setinv" to set i to the argument variable "inverse" within makeCacheMatrix environment
     setinv <- function(inverse) i <<- inverse 
     ## define function "getinv" to return value of i, the inverse if it has been precomputed
     getinv <- function() i
     ## return a list of functions
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)     
}


## cacheSolve checks the list created by makeCacheMatrix(x) for matrix x and either
## returns the cached inverse if the inverse has already been computed and stored
## OR if no cache is found (i.e. x$getinv()=NULL), cacheSolve computes and returns the inverse,
## storing it within the cache by using the setinv() function.
## Input: a list of functions and value-pairings created by makeCacheMatrix function
cacheSolve <- function(x, ...) {
     ## Set i to the i found within the makeCacheMatrix environment for x
     i <- x$getinv()
     ## If i receives any value (non-null), notify that we are retrieving the cached value
     ## and return i
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     ## Otherwise, set new matrix to the matrix x from makeCacheMatrix(x) environment
     data <- x$get()
     ## Compute inverse of new matrix and set it to "i"
     i <- solve(data, ...)
     ## Use setinv(inverse = i) to set "i" within makeCacheMatrix(x) environment to the inverse of x computed here
     x$setinv(i)
     ## Return "i", a matrix that is the inverse of 'x'
     i        
}
