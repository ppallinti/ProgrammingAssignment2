## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of a matrix
## 4. get the value of the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(y) {
	  x <<- y
	  im <<- NULL
	}

	get <- function() x
	setinverse <- function(inverse) im <<- inverse
	getinverse <- function() im
  	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	      im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}

## Example to run the above functions, use the following 4 commands after sourcing this file:
## d<-c(2.2,4.2,6.8,11.6)
## b<-matrix(d,2,2)
## z<-makeCacheMatrix(b)
## cacheSolve(z)
##
## This will calculate the inverse on the first attempt
##          [,1]       [,2]
## [1,] -3.815789  2.2368421
## [2,]  1.381579 -0.7236842
##
## Second run of cacheSolve will return the following cached result:
## cacheSolve(z)
## getting cached data
##          [,1]       [,2]
## [1,] -3.815789  2.2368421
## [2,]  1.381579 -0.7236842
