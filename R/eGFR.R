#' eGFR calculator
#' @description Calculates eGFR using the CKD-EPI equations
#' @param creatinine creatinine value mmol/L
#' @param sex sex "M" for male, "F" for female
#' @param age age in years
#' @returns eGFR in ml/min/1.73m2
#' @examples
#' \dontrun{
#' eGFR(creatinine = 100, sex = "M", age = 40)
#' # 78.6677
#' }
#' @export
eGFR <- function(creatinine, sex, age) {
  if (sex %in% c("Male", "M", "m", "male")) {
    k <- 0.9
    m <- 1
    alpha <- -0.302
  } else if (sex %in% c("Female", "F", "f", "female")) {
    k <- 0.7
    m <- 1.012
    alpha <- -0.241
  }
  egfr <- 142 * ((0.0113 * creatinine / k)^alpha) * ((0.0113 * creatinine / k)^-1.2) * (0.9938^age) * m
  return(egfr)
}
