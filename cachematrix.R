## makeCacheMatrix:  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize m as NULL
    m <- NULL
    
    # create function set
    set <- function(y) {
        # setting outside variable x equal to passed y using the superassigment operator
        x <<- y
        # set outside variable m equal to NULL using the superassigment operator
        m <<- NULL
    }
    
    # create function get
    get <- function() {
        # return x
        x 
    }
    
    # create function setmatrix
    setmatrix <- function(solve) {
        # computing the inverse of a square matrix and setting the outside variable m
        m <<- solve 
    }
    
    # create function getmatrix
    getmatrix <- function() {
        # return m
        m
    }
    
    list (
        set = set, 
        get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix
        )
}


## cacheSolve:  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    # check for value in m
    if(!is.null(m)) {
        # prints message to the console that m is in cache
        message("getting cached data")
        # stops function execution and returns m
        return(m)
    }
    # call the "get" function from makeCacheMatrix
    data <- x$get()
    # computing the inverse of a square matrix and setting the variable m
    m <- solve(data, ...)
    # call the "setmatrix" function from makeCacheMatrix
    x$setmatrix(m)
    # return m
    m
}
