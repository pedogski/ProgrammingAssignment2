## ## Caching the Inverse of a Matrix:
## Matrix inversion is usually computationally intensive, hence there might be some benefits in caching the inverse of a matrix rather than performing repeated computation.

## The following pair of functions are used to create a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

##initialize the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  #define the set function for the matrix
  setmat <- function(y) {
    x <<- y
    matinv <<- NULL
  }
##define the return matrix object by defining it as 'get'
  getmat <- function() x ##return special matrix
  setInverse <- function(inverse) matinv <<- inverse ##setting the inverse matrix and assign the inverse matrix to the environment
  getInverse <- function() matinv
  list(set = setmat,
       get = getmat,
       setInverse = setInverse,
       getInverse = getInverse)
}

## ## This cacheSolve function computes the inverse of the special "matrix" created by
## makeCacheMatrix  function above. However,it will first checks to see if the inverse has already been calculated. If the inverse has already been calculated it gets the inverse from the cache and skips the computation(and the matrix has not changed), then it should retrieve the inverse from the cache.

## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getInverse()
  if (!is.null(matinv)) {
    print("getting cached data")
    return(matinv)
  }
  mat <- x$get()
  matinv <- solve(mat, ...)
  x$setInverse(matinv)
  matinv
}

## The cacheSolve function performs the following:
## 1. Check if the inverse object exist and the matrix is unchanged by getting
##    the stored inverse matrix calling from makeCacheMatrix
## 2. Return the inverse object in cache if it satisfy the first rules.
## 3. Else, compute the inverse of the special 'matrix', store in
##    makeCacheMatrix for future use and return the new inverse object.

###TEST RUN THE FUNCTION
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()

my_matrix$set(matrix(c(4, 3, 3, 2), 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)

my_matrix$set(matrix(c(1, 2, 3, 0, 4, 5, 1, 0, 6), 3, 3))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()


#### It should be noted that if the matrix provided is a singular matrix, inessence it has value for determinant equal zero (0), then the matrix inverse will not ne computed and an error message will be produced. Hence, getInverse() will produce a logical False response.
