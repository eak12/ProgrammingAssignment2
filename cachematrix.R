## MakeCacheMatrix function creates a square matrix that cache its inverse,
## i.e. the function may contain its respective inverse matrix(in a list) in MakeCacheMatrix
## which the cacheSolve functions checks if the inverse of a particular matrix already exists in MakeCacheMatrix
## if the inverse of a particular matrix exists then cacheSolve returns the inverse matrix.
## if not cacheSolve sets the inverse matrix of the special matrix.

## makeCacheMatrix sets and gets the value of matrix, and sets and gets value of inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
        m<- NULL
        set<- function (y)
        {
                
                x<<-y
                m<<- NULL
                
        }
        
        get <- function() x
        setInverseMatrix<- function(InverseMatrix) m<<-InverseMatrix
        getInverseMatrix<-function() m
        
        list(set=set, get=get, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
        
}

## If the respective inverse matrix exists in makeCacheMatrix, then cacheSolve returns that
##else, cacheSolve calculates the respective inverse matrix

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m<- x$getInverseMatrix()
        if(!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        
        data<- x$get()
        m<- solve(x, ...)
        x$setInverseMatrix(m)
        m
}
