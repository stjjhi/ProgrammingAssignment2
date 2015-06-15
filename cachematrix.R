## this file contains  a pair of functions that cache the inverse of a matrix. 

## The first function,input a matrix, makeVector creates a special "vector", 
## which is really a list containing four functions to
## set and get the value of the matrix and it's inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inv_x = NULL
      set <- function(y){
      x<<- y
      inv_x <<- NULL
      }
      get<- function() x
      setInv <- function(inverseMat) inv_x<<- inverseMat
      getInv <- function() inv_x
      list(set = set,get = get,setInv = setInv,getInv = getInv)
}


## The second function,caculate the inversion of the matrix,
## if it's inverse matrix has computed before ,with a cache,
## then return the cache,
## otherwise,compute it's inversion and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getInv()
        if(!is.null(inv_x)){
            message('getting cached inverse matrix')
            return(inv_x)
        }
        t <- x$get()
        inv_x <- solve(t)
        x$setInv(inv_x)
        inv_x
}
