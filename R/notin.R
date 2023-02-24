#' Notin
#' @description The opposite of \%in\%
#' @usage ... \%notin\% ...
#' @param ... A vector of values to test against another vector
#' @param ... The target vector
#' @returns Logical for each values and whether it is in the target vector
#' @examples
#' \dontrun{
#' c(1, 3) %in% c(1, 2)
#' # [1]  FALSE TRUE
#' }
#' @export

`%notin%` <- Negate(`%in%`)
