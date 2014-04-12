##It creates a special "vector", which is a list containing a function to
##set the value of the vector - get
##get the value of the vector - set
##set the value of the mean - setInverse
##get the value of the mean - getInverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##Checks to see if the inverse(inv) has already been calculated. 
##If so, it gets the inverse(inv) from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and 
##sets the value of the inverse in the cache via the setInverse function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv<-x$getInverse()
        if(!is.null(inv)){
          return(inv)
        }
        data<-x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
