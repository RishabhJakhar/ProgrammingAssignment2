## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix() returns a list of 4 functions: 
##                1. set() to input the value of the matrix
##                2. get() to fetch the value of the matrix
##                3. set_inv() to input the value of the matrix inverse
##                4. get_inv() to fetch the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL 
        set <- function(input_matrix) { 
                x <<- input_matrix
                mat_inv <<- NULL
        }
        get <- function() x
        set_inv <- function(input_inverse) mat_inv <<- input_inverse
        get_inv <- function() mat_inv
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## Write a short comment describing this function
## cacheSolve() returns the matrix inverse, either by fetching it from the cache, 
## or solving and then storing it in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$get_inv()
        if (is.null(mat_inv)){
                calculated_inv = solve(x$get())
                x$set_inv(calculated_inv)
        } else {
                message("returning cached data")
                return(mat_inv)
        }
}
