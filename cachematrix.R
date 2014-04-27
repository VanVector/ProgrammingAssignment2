## Calculating inverse of a matrix is an expensive operation.
## makeCacheMatrix function created an object with cache for matrix 
## inverse.
## cacheSolve works with this object and utilizes cache,
## populates it for future uses if its empty.

## The function takes a matrix as an input and creates 
## a list object with get/set methods for matrix and its inverse.
makeCacheMatrix <- function(matrixArg = matrix()) {
		matrixInverse <- NULL
		matrix <- matrixArg
		
		set <- function(newMatrix) {
				matrix <<- newMatrix
				matrixInverse <<- NULL
		}
		
		get <- function() {
				matrix
		}
		
		setMatrixInverse <- function(value) {
				matrixInverse <<- value
		}
		
		getMatrixInverse <- function() {
				matrixInverse
		}
		
		list(
				set = set, 
				get = get,
				setMatrixInverse = setMatrixInverse,
				getMatrixInverse = getMatrixInverse
		)
}


## The function takes a list object returned by makeCacheMatrix
## function, retrieves an inverse matrix either from cache
## or calculates the inverse and populates the cache. The inverse
## is returned.
cacheSolve <- function(matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- matrix$getMatrixInverse()
		if(!is.null(inverse)) {
				message("getting cached data")
				return(inverse)
		}
		data <- matrix$get()
		inverse <- solve(data)
		matrix$setMatrixInverse(inverse)
		inverse
}
