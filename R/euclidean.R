



#'The function euclidean calculates the greatest common divisor of two numbers
#'
#'@param a,b Numeric scalar or integer. The inputs for the euclidean calculation.
#'@param k Numeric scalar or integer. This variable is used temporarily for storing value of b during the calculation.
#'
#'@description
#'The function takes two integers or numeric scalars. The algorithm has while loop for the calculation.
#' The loop continues until the value of b is not equal to zero.
#' Inside the loop value of be is stored in k temporarily.
#' Then calculating the remainder with mod. This calculation result is stored in b.
#' Variable a gets the value of k to make the calculation go on.
#' When the value of b is equal to zero function returns the updated value of a which is the greatest common divisor of the initial two numbers.
#'
#'@return If all input are numeric scalars or integers then the output will be scalar or integer. The function will return the greatest common divisor of the two input numbers.
#'If the input has different type then the function with return error message.
#'
#'@examples
#'#example 1
#'euclidean(123612, 13892347912)
#'
#'#example 2
#'euclidean(100, 1000)
#'
#'\dontrun{
#'euclidean(TRUE,50)
#'}
#'
#'@seealso [Euclidean Algorithm Wikipedia Page] (https://en.wikipedia.org/wiki/Euclidean algorithm)
#'
#'@export
euclidean <- function(a, b) {
  if (!is.numeric(a) || !is.numeric(b) || !length(a) == 1 ||
      !length(b) == 1) {
    stop("check your input")
  }

  a <- abs(a)
  b <- abs(b)

  while (b != 0) {
    k <- b
    b <- a %% b
    a <- k
  }

  return(a)
}
