

makeCacheMatrix <- function(x = matrix()) {
  
  # Creating a makeCacheMatrix object will consist of
  # four functions 
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse of the matrix
  # 4. get the inverse of the matrix
  
  
  # Changes when the user sets the value
  inver <- NULL
  
  #function
 
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  # get function
  
  get <- function() x
  
  # Manually set the inverse
  setinverse <- function(inverse) inver <<- inverse
  
  # Get the inverse
  getinverse <- function() inver
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}

## applying cacheSolve:
   

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inver <- x$getinverse()
  
 
  if(!is.null(inver)) {
    # Simply return the computed inverse		
    message("Getting cached matrix")
    return(inver)
  }


  data <- x$get()
  
  
  inver <- solve(data, ...)
  
 
  x$setinverse(inver)
  

  inver    
  
