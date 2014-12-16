## The purpose of those functions is to cache the computation of
## the inverse of a matrix in order to save time when recalling an inverse
## that has already been calculated previously.

## The makeCacheMatrix() function creates an object that will be access
## later on by the makeSolve() function. This object is a list that
## stores information about the matrix to be inversed.
## Example of use : 
## ma <- matrix(sample.int(100, size = 9, replace = TRUE))
## da <- makeCacheMatrix(ma)
## This "da" object will be feed to the makeSolve() function in order
## to calculate, store, and retrieve the inverse of the "ma" matrix

makeCacheMatrix <- function(x = matrix()) { ## The input should be a matrix
        inverse <- NULL           ## initialize the "inverse" to null
        set <- function(y) {      ## take an input
                x <<- y           ## and save it using superassignement (<<-)
                inverse <<- NULL  ## reset "inverse" to null when
                                  ## a new input is saved.
        }
        get <- function() x       ## return the value of the saved input
        setinverse <- function(solve) inverse <<- solve
                                  ## function called by cacheSolve(),
                                  ## when "inverse" is still at "null".
        
                                  ## It inverses the matrix and
                                  ## save the result in "inverse".
                                  ## That's the cached value.
        
                                  ## If the "inverse" value is not "null",
                                  ## cacheSolve() will skip this step.
        
        getinverse <- function() inverse
                                  ## returns the cached value to cacheSolve()
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
                                  ## This is the list of the four functions
                                  ## in makeCacheMatrix and the way to
                                  ## access them.
}


## The cacheSolve() function will return the inverse of the matrix.
## It'll first look if an inverse value of the matrix has already
## been stored in cache. If yes, and if the matrix hasn't changed,
## cacheSolve() will get the cached value from makeCacheMatrix().
## If no, then the inverse will be computed, printed and stored
## in the cache for later use.
## Example of use :
## cacheSolve(da)
## "da" is the object previously created by makeCacheMatrix() about the
## "ma" matrix.

cacheSolve <- function(x, ...) {  ## The input "x" is the object created
                                  ## by makeCacheMatrix(). 
        inverse <- x$getinverse() ## accesses the object "x" and gets the
                                  ## value of the inverse matrix.
        if(!is.null(inverse)) {   ## If the value stored is not null
                message("getting cached data")  ## print a message
                return(inverse)   ## and return the value stored in cache.
        }
        data <- x$get()           ## If the value stored is null, then get
                                  ## the value of the matrix,
        inverse <- solve(data, ...)  ## compute the inverse,
        x$setinverse(inverse)     ## store it in the cache,
        inverse                   ## and return the result of the computation.
}
