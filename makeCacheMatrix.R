#  Comments here that gives you an overall description of what your functions do #
makeCacheMatrix <- function(x = numeric()) {
     # This starts the inverse function # 
      inv <- NULL 
     # This sets the matrix # 
      set <- function(y) {
            
            x <<- y
            
            inv <<- NULL
      }
     # This gets the matrix #  
      get <- function() {x}
     # This sets and gets the inverse of the matrix #  
      setInverse <- function(inverse) {inv <<- inverse}
      
      getInverse <- function () {inv}
      # This returns the list # 
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) { 
# This returns the x inverse #       
      inv <- x$getInverse()
# This returns a message if its already set #       
      if(!is.null(inv)) {
        
            message("getting cached data")
        
            return(inv)
      }
      
      mx <- x$get()
      
      inv <- solve(mx, ...)
      
      x$setInv(inv)
 # This returns the matrix #      
      inv
  }
