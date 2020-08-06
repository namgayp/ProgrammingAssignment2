## Put comments here that give an overall description of what your
## functions dosads

## Write a short comment describing this function


##The first function, makeCacheMatrix creates and cache the inverse matrix, cntaining the following 
##functions to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse

#example 
#m1<- matrix(c(1,2,3,4),nrow = 2,ncol = 2)
#m1
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#m2<- makeCacheMatrix(m1)
#cacheSolve(m2)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setinverse <- function(i) invers <<- i
  getinverse <- function() invers
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invers <- x$getinverse()
  if(!is.null(invers)) {
    message("getting cached matrix")
    return(invers)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
