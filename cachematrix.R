## The first function creates a cache matrix
## While the second function generates the inverse of the given function


## This function basically creates a cache matrix
makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
    
    set<-function(y){
      x<<- y
      inv<- NULL
    }
    get <- function() x
    setinverse <- function(solvematrix) inv <<- solvematrix
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


##  This function generates the inverse of the given matrix in the first function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
