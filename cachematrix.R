## This are the functions for programming assignement 2 

## This functions initialize and special object that stores information 
## (the inverse in this case) so it can be called again without calculations

makeCacheMatrix <- function(x = matrix(), ...) {
     m <- NULL
     set <- function(y, ...) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function takes a cacheable matrix and calculates its inverse, 
## and store to further petitions.

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
