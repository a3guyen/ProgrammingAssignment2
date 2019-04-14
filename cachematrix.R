## The following codes is to cache the inverse of a matrix

##This function creates a special matrix object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## This function computes the inverse of the special matrix returend by makeCacheMatrix above:

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}

## putting together:
aMatrix <- makeCacheMatrix(matrix(1:9, nrow=3, ncol=3))
aMatrix$get() #retrieve the value of x
aMatrix$getInverse() #retrieve the value of inv
aMatrix$set(matrix(1:4, nrow=2, ncol=2)) #reset value with a new matrix
cacheSolve(aMatrix) # get the inverse matrix
aMatrix$getInverse() #retrieve it directly, now that it has been cached
