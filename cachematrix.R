## Function i design get special matrix object at early time

makeCacheMatrix <- function(x = matrix()) {
## Initialize the inverse property
    inv <- NULL

    ## Matrix set 1
    set <- function( matrix ) {
            x <<- matrix
            inv <<- NULL
    }

    ## Matrix get 1
    get <- function() {
    	## Return the matrix
    	x
    }

    ## Set inverse matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ## Inverse of the matrix get
    getInverse <- function() {
        ## Inverse property return
        inv
    }

    ## Method list return
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

	
}


## Inverse of the special matrix returned by makecachematrix is calculate , not exist return cache inverse

cacheSolve <- function(x, ...) {

  ## inverse of 'x' is return
    y <- x$getInverse()

    ## cache is return if there 
    if( !is.null(y) ) {
            message("Cache data is get")
            return(y)
    }

    ## Matrix is get from special object
    data <- x$get()

    ## Matrix multiplication get inverse
    y <- solve(data) %*% data

    ## Object inverse set
    x$setInverse(y)

    ## Return the matrix
    y
   
}
