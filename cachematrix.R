## makeCacheMatrix is a function that creates a special function with a 
## characteristics to be able to return a solved matrix obtaining it the 
## from the cache. To do that, after must apply the cachesolve function.

## the first function has several parts:
## Firstly, It assign a value NULL for "slv".
## Secondly, create a function named "set", who assign the "y" to "x" (possibility
## of set a new value to the vector) and NULL to slv, not only in the
## enclosing enviroment but also the current enviroment.
## The third part create "get" function, who give you the "x" value.
## The fourth part "set.slv", create a function to fix solve to "slv" value in
## the current enviroment.
## The set.slv function give you the "slv" value assigned by set.slv.
## The last function creates a list with all the functions with the same name for
## every one. So, you can extract the function or the value of the list with
## "$" and the name.

makeCacheMatrix <- function(x = matrix()) {
	slv <- NULL
	set <- function(y) {
		x <<- y
		slv <<- NULL
	}
	get <- function() x
	set.slv <- function(solve) {
		slv <<- solve
	}
	get.slv <- function() {
		slv
	}
	list(set = set, get = get,
		set.slv = set.slv,
		get.slv = get.slv)
}


## The cacheSolve function solves the matrix named "x".
## Firstly, it is assigned the function x$get.slv() to slv. This value is NULL
##  if solved matrix has not been assigned to set.slv.
## After, it applies an "if". If slv is no NULL (a cacheSolve has been called  
##  before) return the "slv" value and the message "getting cached data".
## if not, assign the function x$get() (the value of the x matrix) to "data".
## It is applied "solve function" to data and assign it to "slv".
## It is applies the x$set.slv function to slv (the matrix solved) and it is  
##  fixed in current enviroment, so the x$get() is not NULL (and slv is not 
##  NULL, therefor the loop if will act the next time).
## "slv" value (solved matrix) is given.

cacheSolve <- function(x, ...) {
	slv <- x$get.slv()
	if(!is.null(slv)) {
		message("getting cached data")
		return(slv)
	}
	data <- x$get()
	slv <- solve(data, ...)
	x$set.slv(slv)
	slv
}


## examples
## m <- matrix(rnorm(16), 4, 4)

## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h8 <- hilbert(8); h8
## #"an inner example" sh8 <- solve(h8)
## #"an inner example" round(sh8 %*% h8, 3)

## execution:
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h8 <- hilbert(8); h8
## t_matrix <- makeCacheMatrix(h8)
## cacheSolve(t_matrix)

## m <- matrix(rnorm(16), 4, 4)
## m_matrix <- makeCacheMatrix(x)
## cacheSolve(m_matrix)
