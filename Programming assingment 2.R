##Encoding: UTF-8
## R Programming 
##Programming assignment
## Lexical scoping assignment - Week 3
## Pedro Beltramino

## Creates functios makeCaheMatrix and cacheSolve following instructions



makeCaheMatrix<- function(x = matrix()) {
  inverse.matrix<- NULL
  
  set<- function(y){
    x <<- y
    inverse.matrix <<- NULL 
  }
  get<- function() x
  setInverse<- function(solve) inverse.matrix <<- solve
  getInverse <- function() inverse.matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

cacheSolve<- function(x, ...) {
  inverse.matrix<- x$getInverse()
  
  if(!is.null(inverse.matrix)) {
    message("gettting chaced data")
    return(inverse.matrix)
  }
  mat<- x$get()
  inverse.matrix<- solve(mat, ...)
  x$setInverse(inverse.matrix)
  inverse.matrix
}

## Test created functions


a <- matrix( c(10, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
det(a)

#As determinant of a<>0, it should exist inverse matrix
#Test with Inverse function from matlib package
# Inverse matrix
solve(a)

b <- makeCaheMatrix(a)

#b is a lis of 4, containning created elements

# b$get shows original matrix
b$get()

# b$getInverse is NULL
b$getInverse()

#Let's create C, which if inverse.matrix is NULL, it solves it

c<-cacheSolve(b)
c
