## Put comments here that give an overall description of what your
## functions do

#These functions work in conjunction to store a matrix and to calculate its
#inverse through a series of sub-functions. The sub-functions of the first 
#function are saved as eponymously named elements of a list. This allows the 
#second function to call on each individual sub-function of the first function.
#However, before the second function performs any calculation, it checks to see
#if the inverse has already been calculated and stored.

## Write a short comment describing this function

#This function creates and associates a list of four functions with the input 
#matrix. Each sub-function performs some read or write function of the data 
#calculated by cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}
## Write a short comment describing this function

#This function performs calculations of the data it calls from makeCacheMatrix.
#It is only able to do so because makeCacheMatrix stores certain variables using
#the super-assignment operator.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if(!is.null(i)) {
	        message("Cached data")
		return(i)
	}
	d <- x$get()
	i <- solve(d, ...)
	x$setinv(i)
	i
}
