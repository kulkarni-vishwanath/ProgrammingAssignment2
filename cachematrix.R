## Put comments here that give an overall description of what your functions do

# This file basically contains two functions, The first function basically stores a matrix and its inverse, The second function is used to populate or retrieve the inverse from an object of makeCacheMatrix().


## Write a short comment describing this function
# The makeCacheMatrix() function takes a default input x which is a invertible matrix. We initialize the inverse of a matrix to NULL
# Then we use a set() function that takes an argument y which is again an invertible matrix and assigns it to the x variable in the parent environment (which is why the use of <<- assignment operator)
# This function also assigns a value of NULL to the inv variable (clears any pre-assigned value).
# The get function gets the value of x
# In the setInverse function since the inv variable is defined in the parent function, it can be accessed only using the <<- assignment operator.
# the getInverse function gets the inverse value of the matrix.
# The last part of this function assigns each function as an element of a list and returns to the parent env. The naming convention can help using the $ operator to extract.


# So In a Nutshell, any object created from makeCacheMatrix() can access all the functions get(), set(), setInverse(), and getInverse()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                        # Initialize the inverse to NULL
  
  # the set function can also be used to reset the value of the matrix with a new value. 
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x                                # retrieve the value of matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv                       # retrieve the value of the inverse of matrix x
  list(set=set, 
       get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}


## Write a short comment describing this function
# This function basically gets the inverse using getInverse() at the beginning. Then checks if it is NULL, if it is , it calculates the inverse
# If it is not null, it returns the cached inverse value. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()                              # attempts to retrieve the inverse of the matrix using getInverse()
  if (!is.null(inv)){                                # checks if the inverse obtained is NULL, if the result of the condition (!is.null(inv)) is TRUE, returns the cached inverse
    message("Getting Cached Data")
    return(inv)
  }                                                  # if the result of the condition (!is.null(inv)) is False, calculates the inverse and returns it as inv.
  matr <- x$get()
  inv <- solve(matr,...)
  x$setInverse(inv)
  inv
  
}

