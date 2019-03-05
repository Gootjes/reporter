

fill_in <- function(template, x) {
  e <- list2env(x = x) #envir = parent.env(globalenv()) is safer? custom template functions not available?
  knitr::knit(tangle = F, text = template, quiet = T, envir = e, output = NULL)
}
