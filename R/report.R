
.find <- function(f) {
  UseMethod(".find", f)
}


.find.function <- function(f) {
  .find(f = as.character(match.call()$f))
}

.find.name <- function(f) {
  .find(f = as.character(f))
}

.find.call <- function(f) {
  .find(f = as.character(match.call()$f))
}

.find.character <- function(f) {

  if(length(f) == 3) {
    return(
      paste0(
        f[2],"..",f[3]
        )
      )
  }

  s <- stringr::str_match(string = f, pattern = "([a-zA-Z0-9.]+)::([a-zA-Z0-9.]+)")

  if(any(is.na(s))) {
    s <- stringr::str_match(string = f, pattern = "([a-zA-Z0-9.]+)")



    #ff <- getFunction(s[1,2])

    #e <- environment(ff)

    # if(isNamespace(e)) {
    #   return(
    #     paste0(
    #       getNamespaceName(e), "..", f
    #     )
    #   )
    # } else {
    #   stop("There exists no namespace for the function")
    # }

    e <- findFunction(s[1,2])

    print(f)

    stopifnot(length(e) != 0)

    e <- e[[1]]

    return(
      paste0(
        stringr::str_split_fixed(string = attributes(e)$name, pattern = ":", n = 2)[2],
        "..",
        s[1,2]
        )
      )

  } else {
    return(
      paste0(
        s[1,2], "..", s[1,3]
      )
    )
  }
}

#' @export
report <- function(x, fun, template, options) {

  x <- structure(x, reporter = list(options = options))

  e <- extract(x, fun = match.call()$fun, options)

  render(e, template, options)
}

#' @export
extract <- function(x, fun, options) {

  f <- .find(f = fun)

  ff <- getFunction(paste0("extract", ".", f))

  ff(x, options)
}

#' @export
render <- function(x, template, options) {

  stopifnot("reporter.Extract" %in% class(x))

  stopifnot(!is.null(attributes(x)$reporter))

  stopifnot(!is.null(attributes(x)$reporter$extract.type))

  f <- getFunction(paste0("render", ".", template))

  f(x, options)

}

