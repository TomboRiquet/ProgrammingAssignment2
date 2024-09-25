## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL                           
  set <- function(y) {       #setting the value of the matrix
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv_matrix <<- inverse
  get_inverse <- function() inv_matrix                    #function that gives the inverse of the matrix       
  list(set = set, get = get,                           #return the list
       set_inverse = set_inverse,
       get_inverse = get_inverse)    
}

### This function computes the inverse of the matrix returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)) {      #Check whether inverse of the matrix exists, check if already cached
    message("getting cached data")
    return(inv_matrix)
  }
  
  data <- x$get()
  inv_matrix <- solve(data)               #calculate inverse of the matrix
  x$set_inverse(inv_matrix)                  
  inv_matrix                #return inverse of the matrix
}
