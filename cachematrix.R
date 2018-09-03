## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCaheMatrix take a matrix as input and out put a list containing 
## set and get functions of the matrix, and set and get functions for the inverse
## it will store the value of the matrix input and allow the cacheSolve function
## to assess both the matrix and the cached inverse (if it exists)


makeCacheMatrix <- function(x = matrix()) {
## Initialize the Inverse.  x and minv will be used to store the matrix
## and its inverse
    minv <- NULL
## define the set function.  This will earse the inverse already exist
## from last execution of makeCachematrix
    
    set <- function(y) {
    x <<- y
    minv <<- NULL
  }

  get <- function() x

  setinv <- function(inv) minv <<- inv
  getinv <- function() minv

## prepare the output as a list
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve take the output of makeCachematrix
## and check whether the inverse of the matrix was computed
## if not the inverse is computed and returned.  Otherwise return the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
##check if inverse is there; if so return the value from cached data
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }

## if inverse is not computed, get the matrix and store in data  
    data <- x$get()

## compute using solve function, set the inverse to be the answer
## just obtained and return the answer 
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
  
  }
