# makeCacheMatrix caches a matrix and its inverse.
# cacheSolve computes the inverse of the matrix specified within the 
# special matrix object returned by makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    #' makeCacheMatrix returns a special "matrix object" for a given
    #' input matrix that can cache its inverse.
    #' @param x: Input matrix
    #' @return list of functions that can be used to set and get 
    #'      the matrix and its inverse.
    #' @examples
    #' B_cache = makeCacheMatrix(B) 
    #'   
    matinv <- NULL
    set <- function(y) {
        x <<- y
        matinv <<- NULL
    }
    get <- function() x
    setinv <- function(matrixinv) matinv <<- matrixinv
    getinv <- function() matinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    #' CacheSolve returns the Inverse of matrix object returned
    #' by the function makeCacheMatrix.  If the matrix object
    #' already has the cached inverse, CacheSolve directly returns it.
    #' If the inverse does not exist, it computes it, caches it inside the
    #' object and also returns it.
    #' 
    #' @param x: Matrix object returned by makeCacheMatrix
    #' @return matrix inverse of original matrix packaged within x
    #' @examples
    #' B_cache = makeCacheMatrix(B); cacheSolve(B_cache)  
    #'   
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

