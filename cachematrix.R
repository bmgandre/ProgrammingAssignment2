library(MASS)

#' A object that contains a matrix and its inverse matrix
#' @author Andre B.
#' @param matrix a matrix object
#' @return a object list containing the methods to access the cache
#' @import MASS
#' @examples 
#' m <- makeCacheMatrix(matrix(nrow=3, ncol=3, 3))
#' @seealso \code{\link{cacheSolve}} function for cache solving
makeCacheMatrix <- function(matrix = matrix()) {
    # inverse matrix
    invMatrix <- NULL
    
    #' This method sets the matrix.
    #' Setting a matrix invalidates its inverse matrix
    #' @author Andre B.
    #' @param mtrx object of matrix class
    #' @example m$setMatrix(matrix(nrow=3, ncol=3, 3))
    setMatrix <- function(mtrx) {
        matrix <<- mtrx
        invMatrix <<- NULL
    }
    #' This method returns the current matrix object
    #' @author Andre B.
    #' @return the current matrix
    #' @example m$getMatrix()
    getMatrix <- function() matrix
    
    #' This method sets the inverse matrix for the 
    #' current matrix
    #' @author Andre B.
    #' @param inv object of matrix class
    #' @example m$setInvMatrix(inv)
    setInvMatrix <- function(inv) invMatrix <<- inv
    #' This method returns the current matrix object
    #' @author Andre B.
    #' @return the current inverse matrix
    #' @example m$getInvMatrix()
    getInvMatrix <- function() invMatrix
    
    # return the object list
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}

#' A object that solves the retrieving of an inverse of a matrix
#' in a makeCacheMatrix object
#' @author Andre B.
#' @param matObj list object returned by makeCacheMatrix
#' @param ... aditional parameter to ginv function 
#' @return the inverse matrix
#' @import MASS
#' @examples
#' m <- cacheSolve(makeCacheMatrix(matrix(nrow=3, ncol=3, 3)))
#' @seealso \code{\link{makeCacheMatrix}} function for creating the matObj,
#' \code{\link{ginv}} for getting the inverse of matrix
cacheSolve <- function(matObj, ...) {
    # get the current inverse matrix
    inv <- matObj$getInvMatrix()
    # check if the inverse is cached
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    # get the matrix data
    matrix <- matObj$getMatrix()
    # calculate the inverse of the matrix with ginv
    inv <- ginv(matrix, ...)
    # cache the inverse matrix back
    matObj$setInvMatrix(inv)
    # return the inverse matrix
    inv
}
