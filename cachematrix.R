## This function is to create the matrix, obtain the values of the matrix and to obtain the value of the inverse of the matrix created. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    make <- function(y) {
        x <<- y
        i <<- NULL
    }
    receive <- function() x
    makeinverse <- function(inv) i <<- inv
    receiveinverse <- function() i
    list(
        make = make,
        receive = receive,
        makeinverse = makeinverse,
        receiveinverse = receiveinverse
    )
}

## The function calculates the inverse of the matrix and returns the value of that matrix. 

cacheSolve <- function(x, ...) {
    i <- x$receiveinverse()
    if(!is.null(i)) {
        message("Receiving the cached data")
        return(i)
    }
    m <- x$receive()
    i <- solve(m, ...)
    x$makeinverse(i)
    i
}