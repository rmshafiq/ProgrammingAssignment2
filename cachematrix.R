## function inputs a square matrix and returns a special "vector" 
## which is a list containing functions to
## 1.set the value of the matrix  2.get the value of the matrix
## 3.set the value of the inverse matrix  4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse)  inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## function computes the inverse matrix of the special "vector" created by makeCacheMatrix
## if inverse has already been calculated, it gets the inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse() ##check wether an inverse has been calculated
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv ## Return a matrix that is the inverse of 'x'
}