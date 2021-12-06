## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
