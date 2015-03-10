##My functions are basically a copy of the example with minor changes
##to accomplish it's disired objctive

##To test I sujest the following code 
#set.seed(10)
#M <- matrix(rnorm(9,3,3)#Creates a 3x3 matrix 
#I <- makeCacheMatrix(M)
#cacheSolve(I)
#cacheSolve(I)
#solve(M)

##Result Should be the matrix 
##[,1]      [,2]       [,3]
## [1,]  0.6404624  2.744233 -1.0891800
## [2,] -0.3777843  3.202915 -0.4355093
## [3,] -0.6304550 -1.545961  0.1990977


#The code is the same as the example but insted of x beeing a numeric() 
#is now a matrix() , other then that insted of using mean() I used solve()
#and of course I changed the variables names.

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(INVERSE) Inv <<- INVERSE
        getInv <- function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}

cacheSolve <- function(x, ...) {
        Inv <- x$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInv(Inv)
        Inv
}
