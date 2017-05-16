# R Programing Assignment 2 
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {  
        mx <- NULL  
        set <- function(y) {    
                x <<- y    
                mx <<- NULL  
        }  
        get <- function() x  
        setinverse <- function(inverse) mx <<- inverse  
        getinverse <- function() mx  
                list(set = set, get = get,       
                     setinverse = setinverse,       
                     getinverse = getinverse)  
}
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {  
        mx <- x$getinverse()  
        # If the inverse has already been calculated (and the matrix has not changed),  
        # then  cacheSolve  should retrieve the inverse from the cache.  
        if(!is.null(mx)) {    
                message("getting cached inverse data")    
                return(mx)  
        }  
        # else, calculate the inverse  
        mx.data <- x$get()  
        mx <- solve(mx.data, ...) 
        
        x$setinverse(mx)  
        return(mx)
}

# Test the code
# mx <- matrix(data = rnorm(4), nrow = 2, ncol = 2)
# mx1 <- makeCacheMatrix(mx)
# cacheSolve(mx1)
