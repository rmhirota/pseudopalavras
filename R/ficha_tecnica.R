#' relocate_refs
#'
#' @param html html string
#'
#' @return html string
#' @export
#'
relocate_refs <- function(html) {
  refs <- html %>%
    stringr::str_extract(paste0(
      "<div id=\"ficha-técnica\"[\\s\\S]+",
      "<!--chapter:end:06-referencias.Rmd-->[\\s\\S]+?</div>\n</div>"
    ))
  pref <- html %>%
    stringr::str_extract("<div class=\"preface\">[\\s\\S]+?</div>")
  text <- paste(refs, pref, sep = "\n")
  novo_html <- html %>%
    stringr::str_remove(stringr::fixed(refs)) %>%
    stringr::str_replace(pref, stringr::fixed(text)) %>%
    stringr::str_remove(
      "<li><a href=\"#ficha-técnica\">Ficha técnica</a>[\\s\\S]+?</ul></li>\n"
    ) %>%
  return(novo_html)
}

#' add_footer
#'
#' @param html html string
#' @param text footer text
#'
#' @return html string
#' @export
#'
add_footer <- function(html, footer) {
  footer <- paste("<body>\n<div class='footer'><hr>", footer, "</div>")
  html <- html %>%
    stringr::str_replace(
      "<body>",
      stringr::fixed(footer)
  )
  return(html)
}
