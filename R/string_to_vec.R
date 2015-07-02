#' Converts strings to vectors
#' 
#' Takes in a character string and generates a vector whose elements are
#'  each a character from the string.
#'  
#'  Useful for such things as converting genotype or haplotye data that is
#'    stored in a string.
#' 
#' @param char character string that will be converted to vector
#' @return character vector
#' @export
string_to_vec <- function(char) {
  
  substring(char,
            seq(1, nchar(char)),
            seq(1, nchar(char)))
}