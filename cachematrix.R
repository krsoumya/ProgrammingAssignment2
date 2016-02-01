# makeCacheMatrix creates a list which contains a function that will create the matrix and obtain the values of that matrix. 
# The function also helps in creating and obtaining the value of the matrix that is created. 

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

# The function calculates the inverse of the matrix and returns the value of that matrix. 
# It first verifies whether or not an inverse has been made and if it has been made, it will return the inverse of the matrix.
# If the inverse does not exist, the function will create the inverse using the makeinverse function and return the inverted matrix. 


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