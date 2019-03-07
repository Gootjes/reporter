
.namespace_separator <- ".."

.default_template <- "apa6"

.as_valid_function_path <- function(x) {
  stringr::str_replace(string = x, pattern = "::", replacement = .namespace_separator)
}

.get_template <- function() {
  getOption("reporter.template", .default_template)
}

.set_template <- function(t) {
  options("reporter.template" = t)
}

.find <- function(f) {
  UseMethod(".find", f)
}

.find.character <- function(f) {

  if(length(f) == 3) {
    return(
      paste0(
        f[2],"::",f[3]
        )
      )
  }

  s <- stringr::str_match(string = f, pattern = "([a-zA-Z0-9.]+)::([a-zA-Z0-9.]+)")

  if(any(is.na(s))) {
    s <- stringr::str_match(string = f, pattern = "([a-zA-Z0-9.]+)")

    e <- findFunction(s[1,2])

    stopifnot(length(e) != 0)

    e <- e[[1]]

    return(
      paste0(
        stringr::str_split_fixed(string = attributes(e)$name, pattern = ":", n = 2)[2],
        "::",
        s[1,2]
        )
      )

  } else {
    return(
      paste0(
        s[1,2], "::", s[1,3]
      )
    )
  }
}

#' @export
report <- function(x, fun, options = NULL, template = .get_template()) {

  if("function" %in% class(fun)) {
    f <- .find(f = as.character(match.call()$fun))
  } else if("character" %in% class(fun)) {
    f <- .find(f = fun)
  } else {
    stop("Unsupported function class: ", class(fun))
  }

  x <- structure(x, reporter = list(options = options))

  e <- extract(x, fun = f, options)

  render(e, options, template)
}

#' @export
extract <- function(x, fun, options = NULL) {

  if("function" %in% class(fun)) {
    f <- .find(f = as.character(match.call()$fun))
  } else if("character" %in% class(fun)) {
    f <- .find(f = fun)
  } else {
    stop("Unsupported function class: ", class(fun))
  }

  f <- .as_valid_function_path(f)

  ff <- getFunction(paste0("extract", ".", f))

  ff(x, options)
}

#' @export
render <- function(x, options = NULL, template = .get_template()) {

  stopifnot("reporter.Extract" %in% class(x))

  stopifnot(!is.null(attributes(x)$reporter))

  stopifnot(!is.null(attributes(x)$reporter$extract.type))

  f <- getFunction(paste0("render", ".", template))

  f(x, options)

}

