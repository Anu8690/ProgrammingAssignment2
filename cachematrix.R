## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This fnction will create a special matrix which 
## is actually a list of functions
makeCacheMatrix <- function(x = matrix()) 
{
  m<- NULL
  set <- function(y){
    x<<- y
    m<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) m<<- inverse
  getinverse <- function() m
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}



## Write a short comment describing this function
## This function first checks whether the inverse
## of the given matrix is available in cache or not.
## If available then it retrieves it, otherwise
## this function calculates it and stores it in 
## cache memory
cacheSolve <- function(x, ...) 
{
  m<- x$getinverse()
  if(!is.null(m))
  { 
    message("getting cached data")
    return (m)
  }
  data<- x$get()
  m<- solve(data,...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
