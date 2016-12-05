## This two functions are doing a job that inverse an input matrix

## makeCacheMatrix does a job to store the matrix and store the inversing result 
##and provide matrix and inversing result to other functions


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve does a job to calculate the inverse matrix for an input matrix(if the matrix is inversible)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)){
                message("getting cache data")
                return(i)
        }
        matrixGet <- x$get()
        inverse <- solve(matrixGet)
        x$setInverse(inverse)
        inverse
}
