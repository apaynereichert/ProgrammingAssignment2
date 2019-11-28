## These two functions allow you to cache a matrix and create its inverse
## The first function contains the 'getters' and 'setters'
## It returns a list containing these four functions so that once an object...
## is created, you can call the getting and setting functions from the list
## The second function creates the inverse of a matrix, but checks first to see...
## if the inverse has already been calculated

## This function allows you to create an object that contains a list of the getter and setter functions
## It takes a matrix as its only argument

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

# This function checks to see if the inverse has already been calculated
# If it hasn't, it calculates the inverse of the matrix 
# Also, I took the ellipsis out of the solve arguments b/c I only want solve to calculate inverses
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmatrix(m)
    m
}