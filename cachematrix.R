##cachematrix.R
##This function returns a inverse of a matrix

maketempDataMatrix <- function(x = matrix()) {
        tempData <- NULL
        set <- function(y) {
                x <<- y
                tempData <<- NULL
        }
        get <- function() x
        setMatrix <- function(inverse) tempData <<- inverse
        getInverse <- function() tempData
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}
functionObj1 <- function(x, ...) {
        tempData <- x$getInverse()
        if (!is.null(tempData)) {
                message("Loading DataBase ....")
                return(tempData)
        }
        matrix <- x$get()
        tryCatch( {

                tempData <- solve(matrix, ...)
        },
        error = function(e) {
                message("A Error ocurred:")
                message(e)
                return(NA)
        },
        finally = {

                x$setMatrix(tempData)
        } )
        return (tempData)
}
