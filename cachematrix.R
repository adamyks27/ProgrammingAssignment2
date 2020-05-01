## These two functions create a way to create a matrix and find and store it's in inverse in a local cache.
## The first function, makeCacheMatrix(), creates an object that stores a matrix and, after running, solveCache(), it's inverse.
## The second function, cacheSolve(), takes the object created in makeCacheMatrix(), gets the matrix from it, and calculates it's inverse.
## It then stores the inverse matrix in the object created in makeCacheMatrix() and stores it in a cache in it's environment. 
## If the user attempts to solve for the inverse of a matrix stored in the cache, it cacheSolve() skips the computation for a inverse and merely returns the cached inverse matrix.
## If the user puts in a new matrix, it solves for the inverse of the new matrix, and stores that in the makeCacheMatrix() cache.



## makeCacheMatrix() creates an object which is really a list containing functions to get a matrix, set a matrix, 
## get the inverse of the matrix,and set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrixInv <- function(matrixInv) m <<- matrixInv
        getmatrixInv <- function() m
        list(set = set, get = get,
             setmatrixInv = setmatrixInv,
             getmatrixInv = getmatrixInv)
}


## cacheSolve()takes the matrix object created in makeCacheMatrix and outputs it's inverse.
## It checks first to see if the inverse matrix has already been created and put in the previous function's cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrixInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setmatrixInv(m)
        m
        
}
