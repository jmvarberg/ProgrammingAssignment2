## will create two functions. makeCacheMatrix will take a matrix as input, and generate
## list of objects that store and allow access to cached inverted matrix. cacheSolve
## will operate on a makeCacheMatrix object to either set or retrieve inverted matrix.

## define makeCacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
        ivm <-  NULL #intialize inverted matrix with NULL value
        
        set <-  function(y) { #define set function as object within list to retrieve matrix x and set inverted matrix to null
                x <<- y
                ivm <<- NULL
        }
        
        get <- function() x
        setivm <- function(invmat) ivm <<- invmat #allow user to define ivm by supplying inverted matrix
        getivm <- function() ivm
        list(set = set, get = get,
             setivm = setivm,
             getivm = getivm)
        

}


## cacheSolve will take input object of type makeCacheMatrix and either return cached inverted matrix or calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ivm <- x$getivm()
        if(!is.null(ivm)) {
                message("getting cached data")
                return(ivm)
        }
        data <- x$get()
        ivm <- solve(data, ...)
        x$setivm(ivm)
        ivm
}



