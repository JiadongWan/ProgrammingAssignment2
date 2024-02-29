##  The functions are designed to cache the inverse of a square invertible matrix  
 

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
    m <- null
    set <- function(y){
        x <<- y
        m <<- null
    }
    setinverse <- function(inverse)m <<- inverse
    getinverse <- function()m
    list(set = set, get = get ,
         setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cacheSolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x,...){
    m <- x$getinverse
    if(!is.null(m)){
        message("getting cached data")
        m
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m        ## Return a matrix that is the inverse of 'x'
        
}
