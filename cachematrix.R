## contains 4 functions to get/set value of matrix & it's inverse
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse
## 4 - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {  
  # 1 - set inverse = null since new matrix
  # 1 - set the matrix value
  inv <- NULL
  set <- function(y)
    {
      # `<<-` persists value to an object to a different environment
      x <<- y
      inv <<- NULL
    }
  # 2 get's the value of the matrix
  get <- function() x
  # 3 sets the value of the inverse
  setInv <- function(inverse) inv <<- inverse 
  #4 gets the value of the inverse
  getInv <- function() inv
  #  allows 4 functions to be stored in makeCacheMatrix and makes them accessible
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}

## this is object where input of makeCacheMatrix is stored
## returns matrix that is inverse of 'm'
cacheSolve <- function(m, ...) {
  ## check to see if inverse already exists, if present, it returns it
  invM <- m$getInv()
  if (!is.null(invM)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(invM)  # ends function since the inverse already exists
  }
  
  # not present -> calculate the inverse and set as invM
  tempMatrix <- m$get()
  invM <- solve(tempMatrix, ...)
  m$setInv(invM)
  
  return(invM)
}
