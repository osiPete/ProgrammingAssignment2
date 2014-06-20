## The first function, makeCacheMatrix, creates a special "vector"
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
##
## The second function, cacheSolve, calculates the inverse matrix
## created with the first function. However it first checks to
## see if the inverse matrix has already been calculated. If so,
## it gets the inverse matrix from the cache and skips the 
## computation. Otherwise, it calculates the inverse matrix of
## the data and sets the value of the inverse matrix in the cache
## via the setinverse function.

## returns a list of functions to get / set a matrix and it's
## inverse
## args: x, a matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## returns an inverse matrix that is either calculated here
## or retrieved from a cache if it already exists
## args: x, a list of functions created by makeCacheMatrix()
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
