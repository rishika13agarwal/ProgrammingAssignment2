## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
#can cache its own object. 

#<<- operator is used to assign a value to an object in an environment that is different 
#from the current environment 

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL                               ## this will hold value of matrix inverse 
  set <- function(y) {                      ## define the set function to assign new 
    x <<- y                                 ## value of matrix in parent environment
    inv <<- NULL                            ## if there is a new matrix, reset "inv" to NULL

  }
  get <- function() x                     ## define the "get" fucntion - returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns "inv" to parent environment
  getinverse <- function() inv                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer to the functions with the $ operator 


  ## Write a short comment describing this function
  ## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
  # input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
  # In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
  # and set the inv  matrix by using the solve function.
  # In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
  #after running the code 1st time), it returns a message  "getting cached data" 
  #and the cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {                               #if inverse matrix is not NULL
    message("getting cached data")                  #Type message: getting cached data
    return(inv)                                     #return the invertible matrix
  }
  data <- x$get()                                   #get the original Matrix Data 
  inv <- solve(data, ...)                           #use solve function to inverse the matrix
  x$setinverse(inv)                                 #set the invertible matrix
  inv
}
}
