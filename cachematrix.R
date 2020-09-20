## Put comments here that give an overall description of what your
## functions do
##I have two functions
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL ##making inverse to Null
          set <- function(y)
            {
               x <<- y
               inv <<- NULL
            }
      get<- function (){x} ##getting Matrix x function
      setInverse<-function(inverse){inv<<-inverse}
      getInverse<-function(){inv}
      list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
  }


## Write a short comment describing this function
##This is used to cache the data

cacheSolve <- function(x, ...) ##getting cache data
  {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getInverse()
         if(!is.null(inv)) {  ##checking for Nulls
              message("getting cached data")
             return(inv)
           }
        tat <- x$get()
        inv<-solve(tat, ...) ##calculating the inverse
        x$setInverse(inv)
        inv ## Inverse of X matrix is returned 
}
