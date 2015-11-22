## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## 

## 

makeCacheMatrix <- function(x = matrix()) {
  
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) mat_inv <<- inverse
  getinv <- function() mat_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mat_inv <- x$getinv()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv<- solve(data)
  x$setinv(mat_inv)
  mat_inv
}

## Sample Run
## > xx=matrix(1:4,2,2)
## > mm=makeCacheMatrix(xx)
## > cacheSolve(mm)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Now, during the second time it retrives from cache
# > cacheSolve(mm)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5