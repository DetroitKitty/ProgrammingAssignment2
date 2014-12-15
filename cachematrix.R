## makeCacheMatrix and cacheSolve compute and locally store the inverse of a matrix
## Efficient use is a single call to makeCacheMatrix
##        mlist <- makeCacheMatrix(m)
## followed by a call to cacheSolve whenever the inverse matrix is needed
##        cacheSolve(mlist) 
## 

## The input to makeCacheMatrix is the matrix to be inverted.

makeCacheMatrix <- function(x = matrix()) {
    minverse <- NULL
    set <- function(y) {
        m <<- y
        minverse <<- NULL
    }
    get <- function() x
    setinverse <- function(minv) minverse <<- minv
    getinverse <- function() minverse
    invisible(list(set=set, get=get, setinverse=setinverse,getinverse=getinverse))
}


## cacheSolve will return cached inverse matrix if it exists, 
##  otherwise computes, stores to cache and returns inverse 
## The input to cacheSolve is the list created by makeCacheMatrix
##

cacheSolve <- function(invlist, ...) {
        ## Return a matrix that is the inverse of 'x'
    minverse <- invlist$getinverse()
    if (!is.null(minverse)) {
        message("Getting cached inverse matrix")
        return(minverse)
    }
    message("computing matrix inverse")
    data <- invlist$get()
    minverse <- solve(data)
    invlist$setinverse(minverse)
    invisible(minverse)
}
