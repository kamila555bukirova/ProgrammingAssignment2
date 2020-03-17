## makeCacheMatrix does create a special matrix object that can cache its inverse
## cacheSolve computes the inverse of special matrix returned by makeCacheMatrix above. 
## if the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache


makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y) {
                x<<-y*
                s<<-NULL
        }
        get<-function()x
        setinverse<-function(solve) s<<-solve
        getinverse<-function()s
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
             }
## 1.set the value of the matrix
## 2. get the value of the matrix
## 3.set the value of the inverse
## 4. get the value of the inverse



## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s<-x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data<-x$get()
        s<-solve(data, ...)
        x$setinverse(s)
        s
   }

