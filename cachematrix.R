## This pair of functions allows to cache a matrix and its inverse. 

## This function provides a special object designed for caching matrix and its inverse.
## It returns a list composed by 4 elements : a getter and a setter for the matrix.
## A getter and a setter for the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    
    list(set = set, get = get, 
         setsolve = setsolve, getsolve = getsolve)
}

## This function take as argument a special cache object provided by makeCacheMatrix
## It returns the inverse of the matrix. But it compute it only if the inverse has not beean already computed. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
