## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix: This function creates a special 
#"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #set the value of the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        #set the value of the inverse
        setinverse <- function(inverse) m <<- solve
        #get the value of the inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#cacheSolve: This function computes the inverse of 
#the special "matrix" returned by makeCacheMatrix 

#checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the 
#computation. Otherwise, it calculates the inverse of the 
#data and sets the value of the invere in the cache via the 
#solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
