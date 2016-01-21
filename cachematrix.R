## This function creates a "Special" matrix. It does the following:
## 1. Sets the value of the Matrix.
## 2. Get the value of Matrix
## 3. Set the value of inverse of the matrix
## 4. Get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inver <<- solve
    getinverse <- function() inver
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the matrix created with the makeCacheMatrix function
## It checks if the inverse if cached. If yes, then returns from the cache without calculating. 
## Else it calculates the inverse of the matrix and set this using setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
