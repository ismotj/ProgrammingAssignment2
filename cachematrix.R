## Put comments here that give an overall description of what your
## This pair of functions calculates the inverse of a matrix and 
## cache the inverse of a matrix form the memory if available

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## Define the argument if the matrix
  inv <- NULL                               ## initiates inv as null
  set <- function(y) {                      ## Define te set function to assign new value 
    x <<- y                                 ## to parent environment
    inv <<- NULL                            ## if there is a new matrix, set inv to null
  }
  get <- function() x                       ## returns the value to the get argument
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
  ## you need this in order to refer to the functions with the $ operator
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")            ## check whether the matrix already exist
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
