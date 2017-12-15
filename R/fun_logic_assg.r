
#' Logical assignment, a MJP team defined function
#' 
#' @param x A input variable/ column/ vector of strings to perform the logical assignment on
#' @param reference A reference table (lookup table) having at least 2 column for matching the typo and returning the replacement 
#' @param typo_col The name ( as a "character") of the column in the reference table that allows the function to match with the input column "x" 
#' @param replace_col The name (as a "character) of the column in the reference table that contains the coresponding values to be returned.
#' 
#' @return  A vector (character) that contains the replacement values from the reference table
#' 
#' @examples
#       ref_table <- data.frame(cars = rownames(mtcars), cyl = mtcars$cyl)
#       fun_logic_assg(rownames(mtcars) , ref_table, typo_col = "cars", replace_col = "cyl")
#' @export
fun_logic_assg <- function(x , reference, typo_col = "TYPO", replace_col = "REPLACEMENT"){
    #Count the initial NAs or blanks
    nas <- sum(x =="" | is.na(x))
    #Replace the blanks with NA
    x[x ==""] <- NA
    #Match the value in the column, return the replacement into a variable "output"
    output <- reference[match(x, reference[,typo_col]),replace_col]
    #Count the non-matched values, which was not NA initially but turned into NA after matching
    invs<- length(output[!is.na(x) & is.na(output)])
    #Pop warning message, string those pieces into one sentence
    warning(paste0(invs," values cannot be matched, about " ,round(invs / length(x) *100,2),"% , saved in the variable 'no_match'.  there are ", nas, " values are initially empty."))
    #Assign the non-matched values into a global environment variable for future usage.
    no_match <<- unique(x[!is.na(x) & is.na(output)])
    #return the placement column.
    return(output)
}
