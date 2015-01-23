## This series of functions takes a matrix given by the user, calculates and stores the inverse, 
##and outputs the inverse

##makeCacheMatrix cache's the inverse of a matrix


## makeCacheMatrix takes a matrix, sets up a chache for the inverse of the matrix to be stored,
##and creates a place for the inverse to be retrieved. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #sets m to null until it is filled later
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- solve #gives a place for inverse to be calculated
  getinv <- function() m #gives a place for function to get the inverse 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
} 


## cacheSolve retrieves the matrix. If no inverse has been caculated (would be stored in getin), 
##then it caculated the inverse and prints it out. 
##If an inverse has already been calculated, it returns the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) { #if m is already a solved inverse, we just print it out
    message("getting cached data")
    return(m)
  }
  data <- x$get() #if not, we calculate the inverse and print it
  m <- solve(data, ...)
  x$setinv(m)
  m
}

a=matrix(c(1,2,3,4),2,2)
temp=makeCacheMatrix(a)
cacheSolve(temp)
