## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL ## New matrix => we don't know the inverse (yet)
    }
    get <- function() x
    set.inv.matrix <- function(inv.matrix) inv <<- inv.matrix
    get.inv.matrix <- function() inv
    list(
        set = set,
        get = get,
        set.inv.matrix = set.inv.matrix,
        get.inv.matrix = get.inv.matrix
    )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$get.inv.matrix()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$set.inv.matrix(inv)
    message("calculated new inverse matrix")

    ## Return a matrix that is the inverse of 'x'
    inv
}
