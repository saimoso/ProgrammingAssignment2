## Put comments here that give an overall description of what your
## functions do

# Return a list of setter/getter functions 
# for a matrix and its inverse:
## 1. set matrix
## 2. get matrix
## 3. set inverse
## 4. get inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <<- NULL
      
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      
      setinv <- function(inverse) inv <<- inverse
      
      getinv <- function() inv
      
      list(set = set, 
           get = get, 
           setinv = setinv,
           getinv = getinv)
}


## Return the inverse, IF a) it has already been calculated 
##    AND b) the matrix hasn't changed.
## Otherwise, calculate the inverse again.
cacheSolve <- function(x, ...) {
      # try to fetch the inverse in cache
      inv <- x$getinv()
      
      # check if the inv was in cache
      if(!is.null(inv)){
            message("Retrieving cached inverse...")
            # ... and return it
            return(inv)
      }
      
      # otherwise, calculate the inverse
      mat <- x$get()
      inv <- solve(mat, ...)
      # ... set it
      x$setinv(inv)
      ## Return a matrix that is the inverse of 'x'
      inv
}
