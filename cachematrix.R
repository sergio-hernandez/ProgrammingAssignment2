## These functions are intended to cache the inverse
## of a matrix, compute it and print it to the
## console. This is useful to avoid repeatedly and
## costly computations of matrices inverse.

## This function creates a list containing the
## functions which cache the inverse. It also warns
## stops the execution and provides an error message
## communicating the user if the matrix isn't a
## squared matrix.

makeCacheMatrix<-function(smatrix1=matrix()){
        if(dim(smatrix1)[1]!=dim(smatrix1)[2]){
                stop("smatrix1 (first argument) not a squared matrix")
        }
        inverse<-NULL
        set_values<-function(nmatrix1){
                smatrix1<<-nmatrix1
                inverse<<-NULL
        }
        get_smatrix1<-function() smatrix1
        setinverse<-function(solvedinverse) inverse <<- solvedinverse
        getinverse<-function() inverse
        list(set_values = set_values,
             get_smatrix1 = get_smatrix1,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function evaluates if the inverse already exists
## and if it isn't NULL. If so, it retrieves the inverse
## calculated before. If it hasn't been calculated, it
## calculates the inverse, caches it and print it to the
## console.

cacheSolve<-function(smatrix1,...){
        inverse<-smatrix1$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data_smatrix1 <- smatrix1$get_smatrix1()
        inverse <- solve(data_smatrix1,...)
        my_inverse$setinverse(inverse)
        inverse
}