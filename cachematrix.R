## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##making cache matrix
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y ##set value of matrix
                s <<- NULL
        }
        get <- function() x ## get value of matrix
        
        ##set inverse of matrix 
        setinversematrix<- function(solve) s <<- solve
        ##get inverse of matrix
        getinversematrix <- function() s
        
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        s <- x$getinversematrix()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...) #solving inverse of matrix
        x$setinversematrix(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
