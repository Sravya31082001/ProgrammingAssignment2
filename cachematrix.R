makeCacheMatrix <- function( m = matrix() ) {
    ## Inverse property is used here
        i <- NULL
        set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    ## To get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    ## Now we will use the below to get the inverse property
    getInverse <- function() {
         i
    }
    ## Using the below code to return the list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    ##Now we have to return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    ##Now we have to get from the object named data
    data <- x$get()
    ## Using matrix multiplication
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}
