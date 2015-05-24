## Manipulate the cached instance of the matrix. Whenever a new calculation occurs,
## cache is being emptied and the new object is saved.

makeCacheMatrix <- function(x = matrix()) 
{
  cachedMatrix <- NULL
  
  set <- function(newMatrix) 
  {
    x <<- newMatrix
    cachedMatrix <<- NULL
  }
  
  get <- function() 
  {
    x
  }
  
  setInverse <- function(solve) 
  {
    cachedMatrix <<- solve
  }
  
  getInverse <- function() 
  {
    cachedMatrix
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Checking cache whether an inverse Matrix object exists. if not null return this instance
## otherwise recalculate, via solve function, a brand new inverse matrix, cache it
## and return the matrix object

cacheSolve <- function(x, ...) 
{
  inverseMatrix <- x$getInverse()
  
  if(!is.null(inverseMatrix)) 
  {
    message("getting cached inverse matrix")
    return(inverseMatrix)
  }
  
  matrix <- x$get()
  inverseMatrix <- solve(matrix, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
