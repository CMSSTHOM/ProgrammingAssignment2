##Caching the Inverse of a Matrix

##makeCacheMatrix: This function creates a special "matrix" 
##object that can cache its inverse.

## Creat function and set array
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(matrix) {
    i <<- NULL
    m <<- matrix
    }
## Get the matrix
  get <- function() m
## Setting the inverse of the created matrix
  setInv <- function(inv) i <<- inv
## Get the inverse of the created matrix and set list
  getInv <- function() i
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}
##cacheSolve: This function computes the inverse 

##of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

## Creat function to calulate and return the inverse matrix of 'm'
cacheSolve <- function(m, ...) {
  i <- m$getInv()
  if (!is.null(i)) {
    message
    return(i)
  }
## Calculate the values of the inverse of the input matrix, set and return
  mat <- m$get()
  i <- solve(mat, ...)
  m$setInv(inverse)
  i
}
                           
