## Function makeCacheMatrix stores matrix data in cache
## Second function, cacheSolve returns the inverse of the stored matrix
## where it has not already been computed ie. cacheSolve does not equal makeCacheMatrix

## makeCacheMatrix creates a null object and stores a copy of matrix x

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##save matrix outside local environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##return matrix x
  get <- function() x
  
  ##set solve function
  setsolve <- function(solve) m <<- solve
  
  ##retrive pre solved values
  getsolve <- function() m
  
  ##store 4 functions inside list
  list(set=set, get=get, 
       setsolve=setsolve, 
       getsolve=getsolve)

}


## Invert stored matrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        ## proceed if cache is not empty, return the matrix m
        if(!is.na(m)) {
            message("getting cache data")
            return(m)
        }
        
        ## Otherwise, retrive original matrix
        ## to calc reciprocal of the matrix
        ## set matrix to cache m and
        ## print m
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
      
}
