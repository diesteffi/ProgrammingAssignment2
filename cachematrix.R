## This pair of functions cache the inverse of a matrix.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## Creates a special "matrix", which represents a list containing a function to 
## 1. set the value of the matrix, 2. get the value of the matrix, 
## 3.set the value of the inverse, 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) I <<- Inv
        getInv <- function() I
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## The following function calculates the inverse of the special "matrix" created
## with the above function. Before calculation it checks whether an inverse is 
## already stored in the cache and if it is the inverse of the current matrix. 
## In that case the inverse is extracted from the cache and the computation skipped. 
## Otherwise, the calculated inverse is  stored in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        I <- x$getInv()
        data <- x$get()
        if(!is.null(I)){
                if(det(data %*% I)==1){
                        message("getting cached data")
                        return(I)
                }
        }
        I <- solve(data, ...)
        x$setInv(I)
        I
}
