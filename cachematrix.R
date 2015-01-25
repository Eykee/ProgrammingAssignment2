## The makeCacheMatrix function takes a numeric square matrix x, and
## creates a list of four functions that can manipulate, namely the input matrix x. 

## First, the matrix i is set to Null. Then follows the definition of the function
##'set' which allows one to reset the input-matrix if needed. 

## Second, the function 'get' is defined which retrieves the matrix x

## Third, the function 'setinverse' is defined which takes as its argument the inverse and
## then caches/stores this in variable i

## Fourth, 'getinverse' retrieves the inverse a) if cached it returns the inverse of
## x b) it returns NULL if not cached.

## Last, the output is a list, containing all four functions as its elements

makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i 
        list(set = set, get = get, ## return list of functions
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function takes as its input the list that was created in the function
## makeCacheMatrix (see above)

## The first part retrieves, if present the inverse of the input (x)
## if not NULL, it gives as its output the inverse (i).
## Else this function retrieves the input-matrix of the function makeCacheMatrix, computes
## its inverse using the solve() function and then caches it into (i) using 'setinverse'.
## Again, it gives the inverse as its output.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        xmatrix <- x$get()
        i <- solve(xmatrix, ...)
        x$setinverse(i)
        i
}

