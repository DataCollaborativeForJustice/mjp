#' Vertically displayed column name function, a MJP team defined function
#' 
#' @param data     Input dataset to extract column.
#' 
#' @return     A matrix displaying the columnor variable names vertically
#' 
#' @examples
#'     nameCol(mtcars)
#' @export
nameCol <- function(data=mtcars){
    nameCol <- matrix(colnames(data),ncol=1)
    return(nameCol)
}

