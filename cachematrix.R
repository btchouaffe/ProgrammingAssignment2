##------------------------------------------------------------------------------
## This function returns a list of functions defined in its scope
## to facilitate the caching and computation of inversible matrix.
##
##------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
          ## x is an inversible matrix passed in as parameter.
          ## x will be used to compute it's solve/inverse.

          ## This function returns a list of its functions
          ## to the invoker. ( set, get, setsolve, getsolve )

          ## initialize the solve to NULL
          s <- NULL

          ## function to store the matrix
          set <- function(y) {
                  x <<- y
                  s <<- NULL
          }

          ## function to return the matrix
          get <- function() x

          ## function to store the solve
          setsolve <- function(solve) s <<- solve

          ## function to return the solve
          getsolve <- function() s

          ## return a list of all the functions
          list(set = set, get = get, setsolve = setsolve, getsolve = getsolve )
}

##------------------------------------------------------------------------------
## Write a short comment describing this function
##
##------------------------------------------------------------------------------
cacheSolve <- function(smatrix, ...) {
        ## smatrix is a list containing the functions to compute and cache
        ## solve of inversible matrix.

        ## This function returns the solve of the matrix if it exists in cache,
        ## otherwise it computes it, stores a copy in cache and the solve
        ## to the invoker.

        ## Return a matrix that is the solve of 'x'
        s <- smatrix$getsolve()

        ## Read the matrix from the last computed environment (old matrix)
        om = get("x", environment(smatrix$set))
        print(om)

        ## Read the matrix from its current object ( new matrix )
        nm = smatrix$get()
        print(nm)

        if( !is.null(s) && matchingMatrix(nm, om) ) {
        #if( !is.null(s) ) {
                message("getting cached data")
                return(s)
        }
        data <- smatrix$get()
        s <- solve(data, ...)

        smatrix$set(data)

        smatrix$setsolve(s)
        s
}

##------------------------------------------------------------------------------
## Compares two matrixes for equality and returs a boolean.
##

matchingMatrix <- function(m1 , m2) {

        ## get matrixes dimensions
        d1 <- dim(m1)
        d2 <- dim(m2)

        ## convert matrixes into vectors
        v1 <- as.vector(m1)
        v2 <- as.vector(m2)

        ## Create a boolean vector comparison of dimensions and values
        bvc <- (d1 == d2) && (v1 == v2)

        ## Returns the equality test
        all(bvc)
}
