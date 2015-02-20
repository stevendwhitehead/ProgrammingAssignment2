## These functions provide a means for efficiently obtaining the inverse of a matrix
## over and over again.
## The functions work by caching the inverse of a matrix in a *special* matrix object,
## which encapsulates a matrix in a set of accessor functions. These accessor functions 
## provide efficient access to the underlying matrix and its inverse.

## Note: these functions do not perform rigorous type checking on their arguments.
## Therefore the user is responsible for passing in the correct type of argument
## to each function.

## Given a square invertable matrix, x, return the *special* matrix object that wraps the original matrix.
## in a manner that provides fast access to its inverse via the cacheSolve() function.
## NOTE WELL: this function does not do type checking. Therefore the user must pass in 
## appropriate data types (namely a square invertable matrix!). 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL     ## start with empty cache.
  # define lexically scoped 'set' function.
  set <- function(y) {
    x <<- y
    inverse <<- NULL   ## flush the cache.
  }
  # define lexically scoped 'get' function.
  get <- function () {
    x
  }
  # define lexically scoped 'setInverse' function.
  setInverse <- function(invX) {
    inverse <<- invX
  }
  # define lexically scoped 'getInverse' function.
  getInverse <- function() {
    inverse
  }
  # create and return list of lexically scoped accessors as the *special* matrix object
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Given a *special* matrix object created by the function makeCacheMatrix()
## return the inverse of the underlying matrix associated with the *special* matrix
## object.
## NOTE WELL: this function does not do type checking. Therefore the user must pass in 
## appropriate data types (namely a *special* matrix created via 'makeCasheMatrix'!)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## First, check the cache, if a value is cached, return it, 
  ## otherwise, compute the inverse, cache it, and return the value.
  cacheVal <- x$getInverse()
  if(!is.null(cacheVal)) {
    return(cacheVal)
  } else {
    data <- x$get();
    inv <- solve(data, ...)
    x$setInverse(inv)
    return(inv)
  }
}

##############################################################################
## OK, let's add some tests to verfiy the above functions work as expected. ##
##############################################################################

## First, a test utility that takes a matrix, computes its inverse, multiple times
## and verifies that the original matrix multiplied by its inverse results in the identity
## matrix. The function takes a matrix and uses it to drive a series of tests on the
## cache matrix functions.
## The function returns TRUE if the tests pass; otherwise FALSE
unitTestMatrix <- function(aMatrix) {
  specialMatrix <- makeCacheMatrix(aMatrix)
  invX1 <- cacheSolve(specialMatrix)  ## 1st use, inverse is computed.
  invX2 <- cacheSolve(specialMatrix)  ## 2nd use, inverse is cached.
  
  ## test to make sure inverses values are correct.
  ### first, take the products.
  prod1 <- aMatrix %*% invX1
  prod2 <- aMatrix %*% invX2
  ### next, verfiy that the prods are the appropriate identity matrix.
  if(!all(prod1 - diag(nrow(aMatrix)) < 1e-10)) {
    ## not identity matrix: so complain
    message("Failed to verify inverse for computed inverse value")
    return(FALSE)
  } 
  if(!all(prod2 - diag(nrow(aMatrix)) < 1e-10)) {
    ## not identity matrix: so complain
    message("Failed to verify inverse for cached inverse value")
    return(FALSE)
  } 
  return(TRUE)
}

## OK, let's test some sample matricies.
## the following are all invertable matricies:
m1 <- matrix(c(1,2,5,7), 2, 2)
m2 <- matrix(c(5,10,-3,14,-7,11,55,-6,-9), 3, 3)
m3 <- diag(4)

res1 <- unitTestMatrix(m1)
res2 <- unitTestMatrix(m2)
res3 <- unitTestMatrix(m3)

if(!res1) {
  message("Unit test 1 failed.")
}

if(!res2) {
  message("Unit test 2 failed.")
}

if(!res3) {
  message("Unit test 3 failed.")
}

if(res1 && res2 && res3) {
  message("All unit tests succeeded.")
}

