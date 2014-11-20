## a pair of functions that cache the inverse of a matrix to avoid matrix inversion
## is usually a costly computation and their may be some benefit to caching the 
##inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        df <- NULL ##df is the variable to store the inverted matrix, here it 
        ##is set to NULL
        set <- function(y) {
                x <<- y
                df <<- NULL
        }
        
        get <- function() x ## function to get the matrix
        setinverse <- function(dfinv) df <<- dfinv ##function to cache the 
        ##inverted matrix
        getinverse <- function() df ##function to get the cached inverted matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) ##creates an object of list type that 
        ##contains the functions
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
##has not changed), then the cachesolve should retrieve the inverse from the 
##cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        df <- x$getinverse() ##gets the df variable from the object 
        ##created by makeCacheMatrix
        if(!is.null(df)) {
                message("getting cached data")
                return(df)
        } ## if condition checks if the received variable is not null, i.e if 
        ##it is a cached inverted matrix set by makeCacheMatrix function
        data <- x$get() ##in case the above 'if' condition is not true, gets the 
        ##matrix created by makeCacheMatrix
        df <- solve(data, ...) ##inverts the matrix
        x$setinverse(df) ##caches the inverted matrix
        df 
}
