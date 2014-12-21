## This makeCacheMatrix function call takes an invertible matrix and is a good example of 
## Lexical Scoping, which searches for free variables that are not defined with in the function.
## m is the free variable not defined within the makeCacheMatrix function, and it will store
## the inverse matrix once it is created.
##
## EXAMPLE call cm<-makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## the cm$get() call returns
## a 2 X 2 matrix
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## the cm$getinverse() call will return NULL, as the inverse matrix has not been created
## cacheSolve(cm) will inverse the matrix, set the results in an environment variable m
## and returns 
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## the cm$getinverse() now will return the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ##setter method to set the passed in matrix x to a local scoped variable
        ## x<<-y superassignment operator to a variable beyond the function
        ## cm$set(matrix(c(1, 2, 1, 2), 2, 2))
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        ## getter method to return the original matrix passed in during the constructor or
        ## set via the setFunction
        get <- function() x
        
        ## setter()/getter() method for the inverse matrix
        ## the setinverse will be called from the cacheSolve after in inverse matrix is created
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
  
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes via the R solve function, caches the results ( setinverse ), 
## and returns an inverse matrix
cacheSolve <- function(x, ...) {
        ## gets the inverse matrix that is set in the makeCacheMatrix function call and 
        ## assigns it to the environment variable m
        m <- x$getinverse()
        
        ## checks to see if m is not null, and if not null, it returns it from a 
        ## cached version.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if m is null, the original matrix set from the makeCacheMatric function
        ## is retrieved via the get() call.
        data <- x$get()

        ## using the R solve function, m is assigned the inverse matrix
        m <- solve(data, ...)
        
        ## the new inverse matrix, is now set in the function makeCacheMatric
        x$setinverse(m)
        
        ## return the new inverse matrix ( m )
        m
}
