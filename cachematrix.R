## Author: Sodbo Sharapov
## date: 2016.01.17

## makeCacheMatrix - function to load input matrix to an object that can store
## inverse matrix.
## Input - object contains square matrix with numeric values
## Output - a list of functions and their environment, which stores and operates with
## input matrix and it's inverse. By default inverse matrix is NULL object. 
## It can be computed and cached using cacheSolve function.
## Example of usage: 
## a <- makeCacheMatrix(matrix(rnorm(100),ncol=10))
## a$get()

makeCacheMatrix <- function(x = matrix()) {
        inverse.matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse.matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(Solve) inverse.matrix <<- Solve
        getinverse <- function() inverse.matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve - function which estimate and cache inverse matrix.
## Input is an object produced by makeCacheMatrix function.
## Output - square matrix, which is inverse to matrix, stored in input object.
## After processing this function inverse matrix will be cached in input object.
## Afterwards you don't need to recompute inverse matrix. You can get it by 
## calling x$getinverse() function.
## Example of usage: 
## a <- makeCacheMatrix(matrix(rnorm(100),ncol=10))
## b <- cacheSolve(a)
## a$getinverse()
## Chech whether inversed matrix is really inverse matrix of a :)
## a$get() %*% a$getinverse()


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inverse.matrix <- x$getinverse()
        if(!is.null(inverse.matrix)) {
                message("getting cached data")
                return(inverse.matrix)
        }
        data <- x$get()
        inverse.matrix <- solve(data, ...)
        x$setinverse(inverse.matrix)
        inverse.matrix
}