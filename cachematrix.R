##########################################################
#                 Programming Assignment 2               #
#    Course: R Programming - Data Science Specialization #
#         https://www.coursera.org/course/rprog          #
#                    Manuel Mateus                       #
##########################################################

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#
# This matrix has a list containing functions to:    
#   - set the values of the matrix
#   - get the values of the matrix
#   - set the value of the inverse of the matrix
#   - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    # sets the new matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # gets the existing matrix
    get <- function() x
    # stores the inverse of the matrix in cache - in the inverse variable
    setinverse <- function(calculated_inverse) inverse <<- calculated_inverse
    # returns the inverse of the matrix. NULL if no cached inverse yet created.
    getinverse <- function() inverse
    
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
      
}


# cacheSolve: This function computes the inverse of the special "matrix" 
#             returned by makeCacheMatrix above. If the inverse has already been calculated 
#             (and the matrix has not changed), then the cachesolve should retrieve the inverse 
#             from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(is.null(inverse)){
        # No cached inverse. Calculate new inverse
        message("No inverse cached. Calculating and storing it.")
        
        data <- x$get()
        new_inverse <- solve(data)
        x$setinverse(new_inverse)
        return(new_inverse)
    } else {
        # Existing inverse of the matrix in cache. Return it.
        message("Here is the cached inverse of the given matrix.")
        return(inverse)
    }
}

##################################
# TEST CODE (Uncomment to run)
##################################


# exp_matrix <- matrix(c(2,4,1,3), ncol=2, nrow=2)
# special_matrix <- makeCacheMatrix(exp_matrix)
# cacheSolve(special_matrix)

