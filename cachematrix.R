
## This following function(makeCacheMatrix) creates a special "metrix"
## which is really a list containing a function to 
## 1. set the value of the metrix
## 2. get the value of the metrix
## 3. set the value of the inverse metrix
## 4. get the value of the inverse metrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## set the value of the metrix
        
        get <- function() x
        ## get the value of the metrix
        
        setinverse <- function(inverse) inv <<- inverse
        ## set the value of the inverse metrix
        
        getinverse <- function() inv
        ## get the value of the inverse metrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This following function(cacheSolve) calculates the inverse of the special "metrix"
## created with the above function(makeCacheMatrix)
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
                
        }
        ## It is a first check to see if the inverse has already been calculated.
        ## If so, it gets the inverse from the cache and skips the computation.
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
