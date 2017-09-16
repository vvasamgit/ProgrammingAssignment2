## makeCacheMatrix function  stores matrix object and  inverse of matrix object
## cacheSolve function will be used to compute inverse of the matrix object stored  
## in makeCacheMatrix function
## 
## 

## makeCacheMatrix allows to store a matrix object and its mean

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix<- function(invMatrix) inverseMatrix <<- invMatrix
  getInverseMatrix<- function() inverseMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)

}


## cacheSolve expects input parameter as a function with matrix object and
## capability to store matrix inverse. casheSolve funcion assumes that it always gets a 
## function with matrix object for which inverse matrix can be created.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
  
}


