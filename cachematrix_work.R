##I want to know whether my commit is working

## This function creates a special matrix object that can cache its inverse
## This function only works for a square matrix

makeMatrix <- function(x = matrix()) {
        d <- dim(x)
        if(d[1]!=d[2]) {message("the input matrix must be square")}
        i <- NULL
        ##initializes the value for i
        set <- function(y) {
                x <<- y
                i <<- NULL
        ##clears it if you used it before and changed the value  
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function computes the inverse of the special matrix returned by
## makeMatrix. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Function returns a matrix that is the inverse of 'x'
        ## Input is the matrix x that was created with makeMatrix and also
        ## the four functions that were returned with it: set, get,
        ## setinverse, and getinverse.  Needed to specify that the input to
        ## this function was a matrix otherwise it assumed it was a vector
        ## and I couldn't call the elements of the list using $
        i <- x$getinverse()
        ## within cacheSovle, set i to be i from makeMatrix, if it exists
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        ## if i does exist, then return message and also return i
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}



makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}


> x <- makeVector(c(1,2,3))
> x$get()
[1] 1 2 3
> cachemean(x)
[1] 2
> cachemean(x)
getting cached data
[1] 2
> x$set(c(4,5,6))
> x$get()
[1] 4 5 6
> cachemean(x)
[1] 5
> cachemean(x)
getting cached data
[1] 5


# Test your code
source("cachematrix.R")
#
# generate matrix, and the inverse of the matrix.
size <- 1000 # size of the matrix edge, don't make this too big
mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
mymatrix.inverse <- solve(mymatrix)
#
# now solve the matrix via the cache-method
#
special.matrix   <- makeCacheMatrix(mymatrix)
#
# this should take long, since it's the first go
special.solved.1 <- cacheSolve(special.matrix)
#
# this should be lightning fast
special.solved.2 <- cacheSolve(special.matrix)
#
# check if all solved matrices are identical
identical(mymatrix.inverse, special.solved.1) & identical(mymatrix.inverse, special.solved.2)
#
# should return TRUE