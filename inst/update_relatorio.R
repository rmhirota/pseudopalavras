rmarkdown::render_site("inst/book")

pagedown::chrome_print(
  "inst/book/pseudopalavras.html",
  output = "inst/book/relatorio/pseudopalavras.pdf",
  extra_args = c("--disable-gpu", "--no-sandbox")
)
