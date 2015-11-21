#code uses the framework of the makeVector and cachemean examples to complete the
#Caching the Inverse of a Matrix assignment.
#elements of those examples were changed to complete the assignment otherwise everything is
#exactly the same as original example code.

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#Assign function expects a matrix as input.
#input n x n matrix (will error if matrix is not invertable)
#I tried to add tests to validiate if matrix was invertable or return an error if not,
#but as of right now that was beyond my R skills and it was more important to submit the assignment.
#usage: $variable <- makeCacheMatix(input_matrix)
makeCacheMatix <- function(x = matrix()) {
    
    #local m assigned to Null value
    m <- NULL
    
    #set the vector, y values are the function input, y is assigned to x
    #cache m is reset to null as new y values are going to generate a new m value
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get value of matrix assigned to x
    get <- function() x
    #inverted matrix stored to cache
    setm_var <- function(m_var) m <<- m_var
    #recall value of matrix variable stored to cache
    getm_var <- function() m
    #listing the four functions in this function
    #lets you make specific calls to these functions as needed
    #from the working environment
    list(set = set, get = get,
    setm_var = setm_var,
    getm_var = getm_var)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
#usage: cacheSolve($variable)
cacheSolve <- function(x, ...) {
    #function call to makeCacheMatix get matrix variable stored in cache
    m <- x$getm_var()
    #if m is not null print "getting cached data, return m
    if(!is.null(m)) {
        message("Yo dawg, I heard you liked cached matrices:")
        return(m)
    }
    #assign matrix to data from makeCacheMatix get
    data <- x$get()
    #compute inverse of matrix using solve function
    m <- solve(data, ...)
    #store inverse of matrix in cache using makeCacheMatix set matrix_variable function
    x$setm_var(m)
    #return inverse matrix
    m
}

#Thank you for taking the time to review my code, I hope you're doing well with this course, best of luck to you with
#the rest of this course and the other courses in the data scientist track.