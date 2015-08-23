# Matrix inversion is usually a costly computation operation 
# and there may be some benefits to caching the inverse of a matrix rather than
# repeatedly computing it. The following two functions are used to cache the 
# inverse of a matrix.

# coursera R programming assignment two


# makeCacheMatrix creates a list of four functions. The functions are:-
# set - stores the value of the matrix
# get - returns the value of the matrix
# setinverse - stores the value of inverse of the matrix using a cache operation
# getinverse - returns the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL             # defines the inverse value and indicates
                          # that the inverse matrix has not been cached
  
  # defines the set matrix function
  set <- function(y) {    
    x <<- y               # cache the matrix
    inv <<- NULL          # indicates the inverse matrix has not been cached
  }
  
  # defines the function to get the stored matrix
  get <- function() x     
  
  # defines the function to cache the inverse matrix
  setinverse <- function(inverse) inv <<- inverse 
  
  # defines the function to get the stored inverse matrix
  getinverse <- function() inv
  
  # define the list to hold the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# CacheSolve defines a function that takes a matrix as a parameter and returns
# its inverse.
# If the inverse matrix has already been stored then the function returns the
# cached data. Otherwise it calculate the inverse and return the newly
# calculated inverse.

# caution - This function always assumes that the matrix is invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()             # look up the inverse matrix
        if (!is.null(inv)){               # if the inverse is already stored
          message("getting cache data")   # tell the user it has been stored
          return(inv)                     # return the stored inverse matrix
        }
        data <- x$get()                   # get the stored matrix
        inv <- solve(data,...)            # calculate the inverse matrix
        x$setinverse(inv)                 # store the inverse matrix
        inv                               # return the inverse matrix
}


# Example usage:
# source("cacheMatrix.R")
# x <- matrix(c(10,20,30,40), nrow=2, ncol=2) # create a matrix
# display the matrix
#x
#[,1] [,2]
#[1,]   10   30
#[2,]   20   40

#m = makeCacheMatrix(x) # store the matrix
# get the stored matrix
#m$get()
#[,1] [,2]
#[1,]   10   30
#[2,]   20   40

# The first run will return the non-cached version of the inversed matrix
#cacheSolve(m)
#[,1]  [,2]
#[1,] -0.2  0.15
#[2,]  0.1 -0.05

# The second run will return the cached version of the inversed matrix
#cacheSolve(m)
#getting cache data
#[,1]  [,2]
#[1,] -0.2  0.15
#[2,]  0.1 -0.05


