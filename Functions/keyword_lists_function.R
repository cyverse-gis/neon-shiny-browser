keyword_lists <- function(list) {
  .NEON_keywords <<- new.env()
  for (site in list) {
    products_list <- NEONproducts_product[filter_site(site = site),]
    keywords <- NULL
    for (i in 1:length(products_list$keywords)) {
      keywords <- c(keywords, products_list$keywords[[i]])
    }
    keywords <- unique(keywords)
    keywords <- sort(keywords)
    assign(x = site, value = keywords, envir = .NEON_keywords)
  }
}