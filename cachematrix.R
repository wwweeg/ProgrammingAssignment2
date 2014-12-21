## There are two functions in this R script: (1) makeCacheMatrix()
## and (2) cacheSolve(). They are both closely modeled on the
## examples for programming assignment 2, but using a matrix/solve()
## rather than vector/mean()...
## Together, they allow you to nominate a matrix x to be analyzed (as
## input to (1)); and then to compute & cache the solution to the matrix's
## inverse (via a call to (2)). Once (2) has been called, as long
## as is not called again, subsequent calls to (2) will retrieve the
## cached solution rather than recomputing it. (2) will return an error
## if x is not a square matrix.

## This function takes a matrix input, and sets up a get/set
## infrastructure for the matrix's inverse, which can be called
## in the 2nd function below
makeCacheMatrix <- function(x = matrix()) {
	#inv1 is a holder for the inverse matrix, local to makeCacheMatrix
	inv1 <- NULL
	
	#The set function is apparently not strictly necessary for this
	#assignment...but it allows you to set x via a call to the set()
	#command [e.g., if you do a<-makeCacheMatrix(matrix(1:4, 2, 2),
	#then you could redefine x as matrix(i:j, n, n) by simply calling
	#a$set(matrix(i:j, n, n)] rather than calling makeCacheMatrix again.
	set <- function(y) {
		x <<- y
		inv1 <<- NULL
	}
	#get is a function that merely returns matrix x (as defined in args)
	get <- function() {x}
	
	#setinv takes input z (aka inv2, in cacheSolve()) and sets inv1 == z
	setinv <- function(z) {inv1 <<- z}
	
	#getinv is a function that returns inv1
	#if cacheSolve() has not been called yet, inv1 == NULL
	#when cacheSolve() is 1st called, inv1 is cached == inverse of matrix x
		#b/c data is set == x via call to get()
		#then inv2 is set == solve(data)
		#then inv1 is set == inv2 via call to q$setinv(inv2)
	getinv <- function() {inv1}
	
	#putting this last returns these functions, making them callable
	#the XYZ=XYZ syntax seems to be required in order that they
	#be interpreted as function objects
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function takes as its input the output of the 1st function.
## The first time this function is called, it solves matrix x, and
## caches this value. Subsequent calls to cacheSolve() will return
## cached solution (unless makeCacheMatrix() is called again, which
## starts the process over because the cache gets redefined as NULL).
cacheSolve <- function(q, ...) {
	#set inv2 == inv1 (will == NULL 1st time cacheSolve() is called)
	inv2 <- q$getinv()
	
	#retrieves cached value of solve(x) if x$setinv() --> getinv()
	#cascade has been run already (resulting in inv2 != NULL)
	if(!is.null(inv2)) {
		message("getting cached data")
		return(inv2)
	}
	
	#sets data == matrix x
	data <- q$get()
	
	#sets inv2 == inverse of matrix x
	inv2 <- solve(data, ...)
	
	#caches inv1 == inverse of matrix x (i.e., current value of inv2)
	q$setinv(inv2)
	
	#returns inv2
	inv2
}
