import::from("magrittr", "%>%")

rmarkdown::render_site("inst/book")

readr::read_file("inst/book/pseudopalavras.html") %>%
  pseudopalavras::relocate_refs() %>%
  pseudopalavras::add_footer("CENTRO DE ESTATÍSTICA APLICADA  - IME/ USP") %>%
  readr::write_file("inst/book/relatorio.html")

pagedown::chrome_print(
  "inst/book/relatorio.html",
  output = "inst/book/relatorio/pseudopalavras.pdf",
  extra_args = c("--disable-gpu", "--no-sandbox")
)
