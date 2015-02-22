## The makeCacheMatrix caches the matrix in the environment
## cachematrix check for cached matrix, if it is not found
## then the matrix is solved. the solved values are then cached
## by calling makeCacheMatrix


## this caches the  matrix.  
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  
    set<-function(y){
        x<<- y    ## cache the data 
        m<<-NULL  ## variable for caching solved matrix
    }
    get <- function() x ## get the cached data
    
    setinv <- function(mat) m <<- mat  ## cache the matrix
    
    getinv <- function() m  ## get the cached matrix
    
    ## return the list of functions
    list(set=set, get=get, 
         setinv = setinv, 
         getinv= getinv)
}


## this function solves the matrix or returns 
## cached matrix 

cacheSolve <- function(x, ...) {
  
    m <- x$getinv() ## get the cached matrix from earlier function
    
    ## if the the solved matrix is cached already then just
    ## return the value
    if(!is.null(m)){
        message("getting cached data")
        return(m)  ## return the cached solved matrix
    }
    data <- x$get() ## get the data that has been cached
    
    m <- solve(data)    ## Return a matrix that is the inverse of 'x'
    
    x$setinv(m)  ## update the cache for future calls
    
    m
}
