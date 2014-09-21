# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set,
         get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


#The following function returns the inverse of the matrix. 
# If It is already computed, then gets the results and skips the computation,
#If It is not computed, then it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# Example 3 by 3 matrix

# x<- rbind(c(1,5,-10), c(1/20,-1/30, -1/5), c(1/2, 5, -50))

# m$get() --provide matix

#[,1]        [,2]  [,3]
#[1,] 1.00  5.00000000 -10.0
#[2,] 0.05 -0.03333333  -0.2
#[3,] 0.50  5.00000000 -50.0

#cacheSolve(m)-- first time therefor calcuate iverse of matrix

#       [,1]       [,2]        [,3]
#[1,] 0.22222222 16.6666667 -0.11111111
#[2,] 0.20000000 -3.7500000 -0.02500000
#[3,] 0.02222222 -0.2083333 -0.02361111

# Retriving from the cache in second run
#cacheSolve(m) -- second time therefore gets inverse of matrix from memory
#getting cached data. -- message recived because there was inverse of matrix catched 

#       [,1]       [,2]        [,3]
#[1,] 0.22222222 16.6666667 -0.11111111
#[2,] 0.20000000 -3.7500000 -0.02500000
#[3,] 0.02222222 -0.2083333 -0.02361111