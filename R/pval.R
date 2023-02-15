#' P value formater
#' @description Formats p vales as characters and <0.001 as if needed
#' @param p The p value
#' @param addstars adds stars to the p values, specify a vector of values in decreasing order, under each value a star is added
#' @param lessthan at what value is <p used rather than exact value
#' @param format the format the which the result is return, default is 3 decimal places
#' @returns formatted p value
#' @examples
#' \dontrun{
#' pval(0.02, addstars = c(0.05, 0.01))
#' # "0.020*"
#' }
#' @export

pval <- function(p, addstars = NULL, lessthan = 0.001, format = "%.3f") {
  if (p < lessthan) {
    out <- paste("<", sprintf(format, lessthan), sep = "")
  } else {
    out <- sprintf(format, p)
  }
  if (!is.null(addstars)) {
    for (i in 1:length(addstars))
    {
      if (!all(addstars == addstars[order(-addstars)])) {
        stop("Values for stars must be in decreasing order")
      }
      if (p < addstars[i]) {
        out <- paste(out, "*", sep = "")
      }
    }
  }
  return(out)
}
