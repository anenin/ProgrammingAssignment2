## makeCacheMatrix function fist creates a spesific structure to store matrix and its result.
## structure is the list of functions with subenvironments
## then, cacheSolve gets the structure created by makeCacheMatrix as an input and either
## solves the matrix and stores the result, or retrieves the result
## usage example: 
##  example<-makeCacheMatrix(matrix(5:8,2,2))
##  cacheSolve(example)

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  result<-NULL #initially set result to null
  set <- function(y){
    x<<-y # storing value of x in another environment
    result<<-NULL # and setting global result to NULL (since the matrix is new) 
  }
  get <- function(){
    x # returning stored x
  }
  setSolve <-function (xSolve) { 
    result <<- xSolve  #storing result of calculation in the global variable
    
  }
  getSolve <-function() {
    result #getting result out of memory
  }
  list(set = set, get = get, setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  result<-x$getSolve()
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  data <- x$get()
  result <- solve(data, ...)
  x$setSolve(result)
  result
}
