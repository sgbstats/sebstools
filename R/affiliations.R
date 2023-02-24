#' affiliations
#' @description Makes the nice affiliations list at the top of a paper
#' @param affiliations A tribble of affiliations. Headers should be id (number, which then gets sorted), name, affiliation (if multiple use list()). Non tribbles will be converted
#' @param outfile A .tex file with the destimation
#' @param affiliations_col_names The column names for id, name and affiliation in the tribble
#' @param nooutfile If TRUE then returns the two dataframes with the authors and affilliations
#' @examples
#' \dontrun{
#' x <- tibble::tribble(
#'   ~id, ~name, ~affiliation,
#'   1, "Edmund Blackadder", list("University of Life", "School of Hard Knocks", "Kindergarten of Getting the Shit Kicked Out of Me"),
#'   2, "George Colthurst St. Barleigh", "Trinity College Cambridge",
#'   3, "Sodoff Baldrick", "School of Hard Knocks",
#'   4, "Anthony Cecil Hogmanay Melchett", "Trinity College, Cambridge"
#' )
#' affilliations(x, nooutfile = T)
#' }
#' @export



affilliations <- function(affiliations, outfile = "", affiliations_col_names = c("id", "name", "affiliation"), nooutfile = FALSE) {
  if (!nooutfile & !grepl("*.tex", outfile)) {
    stop("Must output .tex file")
  }
  x <- affiliations %>%
    dplyr::rename(
      "id" = affiliations_col_names[1],
      "name" = affiliations_col_names[2],
      "affiliation" = affiliations_col_names[3]
    )


  x2 <- cbind.data.frame("name" = x$name, "affil" = sapply(x$affiliation, paste, collapse = ";"))

  num <- data.frame(affil = unique(unlist(x$affiliation)), num = 1:length(unique(unlist(x$affiliation)))) %>%
    dplyr::mutate(out = paste("\\affil[", num, "]{", affil, "}", sep = ""))


  suppressWarnings({
    y <- x2 %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      tidyr::separate(col = affil, into = paste("X", 1:max(lengths(x$affiliation)), sep = ""), sep = ";") %>%
      # select(-X1) %>%
      tidyr::pivot_longer(cols = -c(name, id), names_to = "Dummy", values_to = "affil") %>%
      dplyr::select(-Dummy) %>%
      merge(num, by = "affil") %>%
      dplyr::arrange(id, num) %>%
      dplyr::group_by(id, name) %>%
      dplyr::summarise(affils = paste(num, collapse = ",")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(out = paste("\\author[", affils, "]{", name, "}", sep = ""))
  })


  if (nooutfile) {
    return(list("names" = y, "institutions" = num))
  }

  preamble <- c(
    "\\documentclass[a4paper,11pt]{article}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}",
    "\\usepackage{authblk}",
    "\\usepackage[margin=0.5in]{geometry}",
    "\\makeatletter",
    "\\renewcommand{\\maketitle}{\\bgroup\\setlength{\\parindent}{0pt}",
    "\\begin{flushleft}",
    "\\textbf{\\@title}",
    "\\@author",
    "\\end{flushleft}\\egroup",
    "}",
    "\\makeatother",
    "\\title{Authors}"
  )

  postamble <- c(
    "\\begin{document}",
    "\\maketitle",
    "\\end{document}"
  )

  out <- c(preamble, y$out, num$out, postamble)

  readr::write_lines(out, file = outfile)
  return(invisible(NULL))
}
