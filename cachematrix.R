# Writing code for matrix inversion in cache
#
# The next function returns a list of 4 setter and getter functions, similar to makeVector
makeCacheMatrix <- function(X=matrix()) {
    Xinv <- NULL
    set <- function(Y) {
        X <<- Y
        Xinv <<- NULL
    }
    get <- function() X
    setinverse <- function(inverse) Xinv <<- inverse
    getinverse <- function() Xinv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
# The next function gets the cached matrix inverse if available 
# or calculates it with the "solve" function. 
# It takes as an argument the output of makeCacheMatrix
cacheSolve <- function(x, ...) {
    Xinv <- x$getinverse()
    if(!is.null(Xinv)) {
        message("getting cached data")
        return(Xinv)
    }
    data <- x$get()
    Xinv <- solve(data)
    x$setinverse(Xinv)
    Xinv
}
# Below is the execution part on an example
M <- c(5,1,1,1,5,1,1,1,5)
dim(M) <- c(3,3)
amatrix <- makeCacheMatrix(M)
cacheSolve(amatrix)
# Execute again to check that the cached inverted matrix is called (not recalculated)
cacheSolve(amatrix)