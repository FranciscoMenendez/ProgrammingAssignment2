## These functions calculate and store the inverse of a matrix. makeCacheMatrix()
## sets global environment for cachedMatrix(); cachedMatrix() displays cached matrix inverse 
## or calculates, caches and displays matrix Inverse 

## This function creates a list of functions that serve ac a chache for cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    cachedMatrix<-NULL ##set global environment
    set<- function (y) {
        x<<-y ## this sets "x" in the global environment
        cachedMatrix<<-NULL ##creates a object to store outputs
    }
    get<-function() x ##this function will store the matrix for further operations
    setinverse<-function(inverse) cachedMatrix<<-inverse ##this stores the inverse
    getinverse<-function() cachedMatrix ##displays inverse stored
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse) ## Gives a list output
}




## Displays cached Inverse; else calculates the matrix inverse

cacheSolve <- function(x, ...) {
    cachedMatrix<-x$getinverse()
    if (!is.null(cachedMatrix)) {
        message("getting cached data")
        return(cachedMatrix)
        
    }
    data<-x$get()
    cachedMatrix<-solve(data, ...)
    x$setinverse(cachedMatrix)
    cachedMatrix ## Return a matrix that is the inverse of 'x'
        
}
