## Written by LightningKitty for Coursera - R programming on 25 October 2014
## The functions makeCacheMatrix and cacheSolve reduce repeated computation by 
## creating a "matrix object" that allows the inversion of a matrix to be cached.


## makeCacheMatrix takes a normal matrix and stores it as a "matrix object". 
## This is really a list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                         ## Creates inv to hold the inverse
    set <- function(y) {
        x <<- y                                         ## Creates x and stores the given matrix in it
        inv <<- NULL                                    ## Initially, the inverse has not been calculated
    }
    get <- function() x
    setinverse <- function(solution) inv <<- solution   ## Given the solution to the inverse, stores in inv
    getinverse <- function() inv                        ## Returns the inverse
    list(set = set, get = get,                          ## List to allow indexing access to functions
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve returns the inverse of the matrix in a "matrix object" by:
## 1. checking if the inverse has already been calculated and stored
## 2. if not, calculating the inverse, storing it and returning the inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()                                 ## Check x to see if it has an inverse stored
    if(!is.null(i)) {                                   ## If yes, return the stored inverse
        message("getting cached data")
        return(i)
    }
    data <- x$get()                                     ## Otherwise, get the matrix stored in x
    i <- solve(data, ...)                               ## Calculate the inverse
    x$setinverse(i)                                     ## Cache the inverse in x for the future
    i                                                   ## Return the calculated inverse
}




