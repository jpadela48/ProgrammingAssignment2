## This second programming assignment is to create two funtions to demo the new
## <<- assignment operator for setting a variable outside of the current environment.
## It is also used to demo how cached objects are computed much faster and to optimize
## the processing power with R.
## The first function is similar to the example makeVector provided in the assignment
## instructions.  

## makeCacheMatrix gets a matrix input values (set then get values) and creates the 
## inverse sets and then creates the inverse using solve() and stores it (assuming it is 
## stored in the cache).

makeCacheMatrix <- function(x = matrix()) {
    matx <- NULL
    set <- function(y) {
        x <<- y
        matx <<- NULL
    }
    get <- function() x
    #Create / Calculate the inverse using solve()
    setInverse <- function() matx <<- solve(x) 
    getInverse <- function() matx
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The following function takes the matrix created in makeCacheMatrix
## above and checks for its existence and prints it if it does exist.  If 
## it doesn't exist, it saves the matrix and prints the inverse using "setInverse".
## needs a squared matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matx <- x$getInverse()
    if (!is.null(matx)) {
        message("Get the cached data")
        return(matx)
    }
    data <- x$get()
    matx <- solve(data, ...)
    x$setInverse(matx)
    matx
}
