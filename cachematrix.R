# Part I-----------------------------------------------------------------------
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# Initialising 2 object - 'x' argument & 'i' set to NULL
makeCacheMatrix <- function(x= matrix()){
    i <- NULL
    
    set <- function(y){
        # set assigns input argument to 'x' object in parent environment
        x <<- y
        # set assigns NULL to 'i' object in parent environment
        i <<- NULL
    }
    # Code to get the matrix 'x' from parent environment
    get <- function() {x}
    # Code to set the inverse 'i'of the matrix
    setInverse <- function(inverse) {i <<- inverse}
    # Code to get the inverse 'i' of the matrix
    getInverse <- function() {i}
    # Assigns a name to each element
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

# Part 2 ------------------------------------------------------------------------
##cacheSolve: Computing the inverse of the special "matrix" returned by makeCacheMatrix above.
# starts with single argument 'x' & ...
cacheSolve <- function(x, ...) {
    # function tries to find inverse of matrix
    i <- x$getInverse()
    # if the value is not NULL then get the inverse
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # Sets the inverse of the matrix
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i)
    i
}

# Test-------------------------------------------------------------
## testing the code
new <- matrix(rnorm(25),5,5)
cache <- makeCacheMatrix(new)
cacheSolve(cache)


