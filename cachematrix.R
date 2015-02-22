

## This function creates a matrix object that can cache its inverse.
## This function only works for a square matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## makeCacheMatrix is a function that takes a matrix as input.  The
        ## input matrix must be square in order to calculate an inverse
        ## with this function.
        d <- dim(x)
        if(d[1]!=d[2]) {stop("the input matrix must be square")}
        ## These two lines compare the number of columns in the matrix to
        ## the number of row in the matrix.  If they are not the same,
        ## meaning the matrix is not square, then stop the function and 
        ## return an error and a message.
        i <- NULL
        ## this line just initializes the value for i
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## set is a function that clears i every time you run the makeMatrix
        ## function.  That will signal that the matrix is new and the cache
        ## should be cleared.
        get <- function() x
        ## get is a function that returns the matrix x.
        setinverse <- function(inverse) i <<- inverse
        ## setinverse is a function that will hold the inverse of x if it
        ## has been called already.  It looks for i outside of the local
        ## environment.
        getinverse <- function() i
        ## getinverse is a function that returns i, the inverse of x.
        ## It will be null if setinverse has not been called or
        ## if set was called after the last call to setinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        ## the makeCacheMatrix function returns 4 other functions
}



## This function computes the inverse of the matrix object returned by
## makeCacheMatrix. If the inverse has already been calculated and
## the matrix has not changed, then cacheSolve should retrieve the inverse
## from the cache.  If the inverse has not been calculated or the matrix
## has changed, then cacheSolve should calculate the inverse.

cacheSolve <- function(x=matrix(), ...) {
        ## cacheSolve is a function that takes a matrix x as input.
        ## The input matrix x was created with makeCacheMatrix and also carries
        ## four function objects with it: set, get, setinverse, and
        ## getinverse.  I needed to specify that the input
        ## to this function was a matrix otherwise the function defaulted
        ## x to a vector and I got an error when trying to call the objects
        ## of x using the $ operator.
        i <- x$getinverse()
        ## within cacheSolve, set i to be i from makeCacheMatrix, if it exists
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If i already exists, then return the message and also return i.
        ## No more lines are evaluated, meaning the solve step doesn't happen.
        ## If i doesn't exist, because it is the first time through or the
        ## matrix has changed, then the following four lines are evaluated.
        data <- x$get()
        ## data is assigned the matrix x from makeCacheMatrix
        i <- solve(data, ...)
        ## i becomes the inverse of x
        x$setinverse(i)
        ## setinverse knows for next time that the inverse has already been
        ## calculated
        i
        ## return the inverse
}