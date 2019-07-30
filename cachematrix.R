## This function, makeCacheMatrix, creates a special "vector", which is really a list
## of functions to 1. set the value of a given matrix, 2. get the value of the matrix,
## 3. set the value of the inverse of a given matrix (considering it is inversible) and,
## 4. get the value of the inverse of the given matrix.

rm(list = ls())  # Cleans the environment

## makeCacheMatrix works by caching a given matrix and calculating its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    InverseMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        InverseMatrix <<- NULL
    }
    get <- function() x
    set.solved <- function(solve) InverseMatrix <<- solve
    get.solved <- function() InverseMatrix
    list(set = set, get = get,
         set.solved = set.solved,
         get.solved = get.solved)
    
}


## cacheSOlve works by checking whether "InverseMatrix" exists (the value of the inverse of
## the given matrix 'x') and returning it by acessing the cache. If it does not exist,
## cacheSolve will calculate 'x' inverse and return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    InverseMatrix <- x$get.solved()
    if(!is.null(InverseMatrix)) {
        message("getting cached data")
        return(InverseMatrix)
    }
    data <- x$get()
    InverseMatrix <- solve(data, ...)
    x$set.solved(InverseMatrix)
    InverseMatrix
}