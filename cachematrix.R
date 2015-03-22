## The purpose of the functions "makeCacheMatrix" &
## "cacheSolve" is to reduce processing time by caching  
##  results of the inverse of a matrix so that when it is
##  needed again it can be called from the cache


## makeCacheMatrix creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                ## assign a value to an object in an environment 
                ##that is different from the current environmen
            
                x <<- y
                m <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        setinverse <- function(inverse) m <<- inverse 
        ## get the inverse of the matrix
        getinverse = function() m
        list(set=set, get=get, setinverse=setinv, getinverse=getinverse)
}

}


## cachesolve computes the inverse of the matrix from function "makeCacheMatrix"
## If the results is already in the cache, it will retrievefrom there
## Otherwise it will compute it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        ## get inverse from cache, otherwise solve "m"
        if(!is.null(m)) {
                message("getting cacted data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
   
        x$setinverse(m)
        m
}  
        
        

