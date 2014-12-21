## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.

## Test and verify the correctness by running the codes in the two examples given below, and verify the answers.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse. Computing the inverse of a square matrix can be done with the "solve" function.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}

# Example 1: Inverse of 2x2 matrix 
# x <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=T)
# make <- makeCacheMatrix(x)
# cacheSolve(make)
# Expected Ans:
#       [,1]    [,2]
# [1,] -2.0     1.0
# [2,]  1.5     -0.5               


# Example 2: Inverse of 3x3 matrix 
# y <- matrix(c(1,2,3,0,4,5,1,0,6), nrow=3, ncol=3, byrow=T)
# make <- makeCacheMatrix(y)
# cacheSolve(make)
# Expected Ans:
#         [,1]        [,2]        [,3]
#[1,]  1.0909091 -0.54545455 -0.09090909
#[2,]  0.2272727  0.13636364 -0.22727273
#[3,] -0.1818182  0.09090909  0.18181818

