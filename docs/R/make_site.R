pages <- list.files(path = '.', pattern = '.Rmd')

purrr::map(pages, rmarkdown::render, output_format='html_document')
rmarkdown::render_site()
