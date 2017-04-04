## Cachematrix.R
## The functions will cache the inverse of a matrix
## We will consider all matrixes used as input will be inversible
## The cached mechanism will ensure if the inverse is already calculated then return the cached value

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

## Assignment NULL for first initialization
CacheMatrix <- NULL

##Define function Setmatrix
SetMAtrix <- function (y) {

			x <<- y
			CacheMatrix <<- NULL
			}
##Define function Getmatrix
GetMatrix <- function () x

##Define function SetCacheMatrix
SetCacheMatrix <- function (inverse) CacheMatrix <<- inverse

##Define function GetCacheMatrix
GetCacheMatrix <- function () CacheMatrix

list(SetMAtrix = SetMAtrix,GetMatrix = GetMatrix, SetCacheMatrix = SetCacheMatrix,   GetCacheMatrix = GetCacheMatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	CacheMatrix <- x$GetCacheMatrix()

##if the content is not null then: return the cached result
	if(!is.null(CacheMatrix)) {

					message("getting cached data")
					return(CacheMatrix)
				}

##if the content is empty then calculate and return

	data <- x$GetMatrix ()
	
	CacheMatrix <- solve(data, ...)

	x$SetCacheMatrix(CacheMatrix)

	CacheMatrix
}

## example usage:
## source("cachematrix.R")
## DatMatrix <- makeCacheMatrix(matrix(0:2, 2, 2))
## DatMatrix$GetMatrix()
## DatMatrix$GetCacheMatrix() 
## cacheSolve(DatMatrix)
## DatMatrix$GetCacheMatrix()
## cacheSolve(DatMatrix)
## DatMatrix$GetCacheMatrix()