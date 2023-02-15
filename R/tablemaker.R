#' tablemaker
#' @description Writes a table for pasting into a github markdown
#' @param df A dataframe for conversion
#' @param path The path of the text file
#' @export

tablemaker <- function(df, path) {
  n <- names(df)
  out <- character(nrow(df))
  df[is.na(df)] <- ""
  for (i in 1:nrow(df))
  {
    out[i] <- paste("|", paste(df[i, ], collapse = "|"), "|", sep = "")
  }

  out <- gsub("\\r\\n", "<br />", out)
  out <- gsub("NA", "", out)
  h <- paste("|", paste(n, collapse = "|"), "|", sep = "")
  div <- paste("|", paste(rep("---|", ncol(df)), collapse = ""), sep = "")

  fileConn <- file(path)
  writeLines(c(h, div, out), fileConn)
  close(fileConn)
  return(invisible(NULL))
}
