## Put comments here that give an overall description of what your
## functions do

## function that calculates the inverse of the matrix and store it in cache. If the set() function is called, this resets the cache.
makeCacheMatrix <- function(x = matrix()){
      inv_list <- NULL
      set <- function(y){
          x <<- y
          inv_list <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv_list <<- inverse}
      getInverse <- function() { 
         if (!is.null(inv_list)) ## if NULL compute, this means matrix is set using the set() function
         {
            inver<-solve(x) #solve for inverse
         }
      }
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function that gets the inversed matrix and gets the cached data if using the same matrix value, else, recalcs it.

cacheSolve <- function(x, ...){
      inv_list <- x$getInverse()
      if(!is.null(inv_list)){  #check for cached data
            message("getting cached data")
            return(inv_list)
      }
      #calculate inverse
      mat <- x$get()
      inv_list <- solve(mat, ...)
      x$setInverse(inv_list)
      inv_list
}
