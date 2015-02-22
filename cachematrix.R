## I've used solve to cache the inverse matrix
## Replaced mean with solve in the function

makeCacheMatrix<-function(x=matrix()){
        m<-NULL
        set<-function(y){ ##set the value of the matrix
                x<<-y
                m<<-NULL 
        }
        get<-function() x
        setmatrix<-function(solve) m<<-solve
        getmatrix<-function() m ## get the value of the matrix
        list(set=set,get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}
cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix() ## set the value of the inverse matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get() ## get the value of the inverse matrix
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
##This is just an example:
data<-makeCacheMatrix()
data$set(matrix(5:8,2,2))
cacheSolve(data)
