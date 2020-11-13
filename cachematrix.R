## Programmng Assignment 2
## R Programming Cuurse
## by Dennis Dizon

## The following code contains two related functions: makeCacheMatrix, which creates a special "matrix" object that can cache its inverse; and
## cacheSolve, a function which computes the inverse of the matrix returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## The first function below computes the inverse of a given square matrix using the solve function in R.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The second function below computes the inverse of the matrix returned by the first function. If the inverse has already been calculated (and the matrix has not changed),
## then the function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getinv()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

## A sample input matrix that can be used for the first function can be generated using the following code from the help article for the solve() function.

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h8 <- hilbert(8); h8

## The output of the above function, h8, can then be used as input in the makeCacheMatrix function.
