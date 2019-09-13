#What are we doing here? We are caching the inverse of my randomly generated matrix "x" 
#We'll follow the pattern of the instructions with a few changes:

#First we create the function that modifies a matrix that cacheinv can use later on

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#Next we input the modified matrix and actually invert it in cacheinv

cachesolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
#I created a randomized matrix "x" to test the code
#> x
#            [,1]       [,2]       [,3]       [,4]
#[1,] -0.04528426  2.3679803  0.5040142 -1.7625963
#[2,] -0.41071506  0.1381090 -1.3251268  0.6735820
#[3,] -0.58841686 -2.3545800 -0.5238336 -0.3042536
#[4,]  0.64109285  0.4640877  0.7098748  0.3425776

# x2<-makeCacheMatrix(x)
#cachesolve(x2)

#getting cached data
#            [,1]       [,2]       [,3]      [,4]
#[1,]  1.82768681  2.9216598  3.3100835  6.598801
#[2,] -0.07831979 -0.1450634 -0.6487481 -0.693910
#[3,] -1.10032132 -2.1207978 -1.8484987 -3.133023
#[4,] -1.03415780 -0.8763917 -1.4851881 -1.997664

#These numbers are correct for x2^-1
#So it looks like it works
