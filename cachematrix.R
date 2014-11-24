## Make a cache for both a matrix and its inverse.  Allow both the matrix and inverse to be 
## set and retrieved.  This will speed up programs by bypassing the solve() function and 
## retrieving inverse matrices from cache.

## Function allows a matrix and its inverse to be both cached and read from cache

makeCacheMatrix <- function(x = matrix()) 
{ 
    ## set the matrix value and set the inverse to NULL
    set <- function(ymatrix)
    {
        xmatrix <<- ymatrix
        xinverse <<- NULL
    }
    
    ## gets the value of the matrix previously stored in xmatrix
    get <- function()
    {
        xmatrix
    }
    
    ## set the inverse matrix cache to xinverse
    setinverse <- function(yinverse)
    {
        xinverse <<- yinverse
    }
    
    ## get the cached inverse matrix
    getinverse <- function()
    {
        xinverse
    }
    
    ## name the functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Retrieve the inverse matrix of x.  Retrieve from cache or create new and store in cache.

cacheSolve <- function(x, ...) 
{
    ## retrieve the value of the inverse matrix of x
    invmatrix <- x$getinverse() 
    
    ## if inverse matrix is already cached, then return it
    if(!is.null(invmatrix))
    {
        return(invmatrix)
    }
    ## else create the inverse matrix, store in cache, and return it
    else
    {
        ## retrieve matrix and store in workmatrix
        workmatrix <- x$get()
        ## create the inverse of x and store in invmatrix
        invmatrix <- solve(workmatrix)
        ## set the value of the cache to invmatrix
        x$setinverse(invmatrix)
        ## return the value of invmatrix
        return(invmatrix)
    }
}
