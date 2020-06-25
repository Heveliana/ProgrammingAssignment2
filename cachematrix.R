
## The functions makeChacheMatrix and chacheSolve calcualte the inverse of an invertible matrix and store it in a cache object.


## makeCacheMatrix delivers a set of four functions that are needed to change and store object in the cache.

makeCacheMatrix <- function(x = matrix()) {
    
    #initialize the object Invs with an empty value:
    Invs <- NULL
    
    # set puts the given value into x in the parent environment and empties the cached Invs( to make sure a new inverse has to be calculated):
    set <- function(y) {
      x <<- y
      Invs <<- NULL
    }
    
    # get deliveres x from the parent environment:
    get <- function(){
      x
    }
    
    #setinverse puts the given value into Invs in the parent environment:
    setinverse <- function(z){
      Invs <<- z
    }
    
    #getinvers deliveres Invs from the parent environment:
    getinverse <- function(){
      Invs
    }
    
    #makeCacheMatrix returns a list of the functions and names them accordingly:
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}





## cacheSolve returns a matrix that is the inverse of the input matrix x. If the inverse was already calculated a cached object is returned.
## Otherwise the inverse is calculated. cacheSolve needs the list that was created by makeCacheMatrix.

cacheSolve <- function(x, ...) {

    # Use the makeCacheMatrix function getinverse to receive the cached value for Invs:
    Invs <- x$getinverse()
    
    # Check if Invs is not empty, if this is the case show a warning. cacheSolve returns the cached inversed matrix Invs
    if(!is.null(Invs)) {
      message("getting cached data")
      return(Invs)
    }
    
    # If the Invs was empty the following code will be executed:
    
    # the matrix is stored in data:
    data <- x$get()
    
    # the inverse of the matrix is calculated:
    Invs <- solve(data, ...)
    
    # the inverse is cached:
    x$setinverse(Invs)
    
    # cacheSolve returns the calculated inversed matrix Invs
    Invs
  
}
