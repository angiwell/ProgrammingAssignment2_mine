## Week 3 - Assignment 2
## The makeCacheMatrix function creates a special matrix object that can cache its inverse
## For this assignment, we assume that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix, given in argument, has not changed), 
## then the cachesolve should retrieve the inverse from the cache 
## otherwise, it compute the new matrix inverse

cacheSolve <- function(x, a = matrix(), ...) {
        i <- x$getinverse()
        m <- x$get()
        if(!is.null(i) && identical(m, a)) {
                message("Getting cached data")
                return(i)
        }
        else if(identical(m, a)){
                message("Computing the inverse")
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
        }
        else {
                x$set(a)
                message("New matrix, compute new inverse")
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
        }
        
}

## example of use :
## b <- matrix(c(2, 4, 3, 4), 2, 2)
## a <- matrix(c(2, 4, 3, 1), 2, 2)
## m <- makeCacheMatrix(a)
## cacheSolve(m, a) --> Computes the inverse of matrix a
## cacheSolve(m, a) --> Gets the cached data of a
## cacheSolve(m, b) --> computes the inverse of matrix b
## cacheSolve(m, b) --> Gets the cached data of b
