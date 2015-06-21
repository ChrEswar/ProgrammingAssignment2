##  Programming Assignment 2 

##
## makeCacheMatrix  is the function created
## setinverse  will store the inverse of the matrix
## getinverse  will reterive the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { 
  
  i  <- NULL 
  
  set  <- function(y){ 
    x <<- y 
    i <<- NULL  
  } 
  
  get  <- function() x 
  
  setinverse  <- function(inverse) 
    i  <<- inverse 
  
  getinverse  <- function() i  
  
  list(set= set, get = get,  
       setinverse = setinverse,  
       getinverse = getinverse)   
} 

## Cachesolve will prepares  the inverse of the matrix.
## solve will determine the inverse of the matrix
## getinverse() will get inverse matrix from the cache.

cacheSolve <- function(x, ...) {  
  i  <- x$getinverse()  
  
  if (!is.null(i)){  
    message("getting cached data")  
    return(i)  
  } 
  
  data  <- x$get()  
  i  <- solve(data, ...)  
  x$setinverse(i)  
  i 
  
} 

### How to execute this Program :: steps to be followed.

### m1 = matrix(c(1,2,3,4),nrow=2,ncol=2)
### > m2 <- makeCacheMatrix(m1)
###
### > m2$get()
### [,1] [,2]
### [1,]    1    3
### [2,]    2    4
###
### > m2$getinverse()
### NULL
###
### > m3 <- cacheSolve(m2)
###
### > m2$getinverse()
### [,1] [,2]
### [1,]   -2  1.5
### [2,]    1 -0.5
###
### > cacheSolve(m2)
### getting cached data
### [,1] [,2]
### [1,]   -2  1.5
### [2,]    1 -0.5



