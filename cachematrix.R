##Together makeCacheMatrix and cacheSolve allow one to find the inverse of an
##invertible matrix and cache the inverse so that if it is needed again later
##R will first check to see if it has already been computed, and if so
##will retrieve the cached value rather than compute the inverse again
##which saves time.

#The function makeCacheMatrix takes the argument x, which is assumed here to be
#an invertible matrix, and returns a vector of function: set, get, setinverse, getinverse
#set can be used to set the value of x without creating a new instance of makeCacheMatrix
#get returns x
#setinverse computes the inverse of x and assigns it to i in the parent environment
#getinverse returns i; should not be used until the inverse has actually been
#computed and stored in i using cacheSolve

makeCacheMatrix <- function(x = matrix()) 
  {
  i <- NULL
  set <- function(y) 
    {
    x <<- y
    i <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }

#The function cacheSolve takes an instance of makeCacheMatrix and first checks
#to see if the inverse of the matrix x defined by the instance of makeCacheMatrix
#has been computed and cached.  If it has, cacheSolve returns the cached value
#without computation.  If it has not, cacheSolve computes the inverse of x, 
#stores the value in i using the function setinverse, and returns i, the inverse

cacheSolve <- function(x, ...) 
  {
  i <- x$getinverse()
  if(!is.null(i)) 
    {
    message("getting cached data")
    return(i)
    }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  }
