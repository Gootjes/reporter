

apa6_p <- function(x) {
  ifelse(x < .001, "_p_ < .001",
         ifelse(x < .01, fill_in("_p_ = `r round(x, 3)`", list(x=x)), fill_in("_p_ = `r round(x, 3)`", list(x=x)))
         )

}

render.apa6 <- function(x, options) {

  stopifnot("reporter.Extract" %in% class(x))

  stopifnot(!is.null(attributes(x)$reporter))

  stopifnot(!is.null(attributes(x)$reporter$extract.type))

  t <- attributes(x)$reporter$extract.type

  class(x) <- t

  UseMethod("render.apa6", x)

}

render.apa6.LinearModel <- function(x, options) {

  if(is.null(options)) {
    options <- list(ci = .95)
  }

  list(
    overall_model = list(
      f = fill_in("_F_(`r fstatistic$numdf`, `r fstatistic$dendf`) = `r round(fstatistic$value, 2)`", x),
      p = fill_in("`r apa6_p(pf(fstatistic$value, fstatistic$numdf, fstatistic$dendf, lower.tail=FALSE))`", x),
      Rsquared = fill_in("_R_^2^ = `r round(rsquared, 2)`", x),
      Rsquaredadjusted = fill_in("_R_^2^adj = `r round(rsquared_adjusted, 2)`", x)
    ),
    estimates = Map(function(e){
      list(
        Estimate = fill_in("_B_ = `r round(B, 2)`", e),
        SE = fill_in("_SE_ = `r round(SE, 2)`", e),
        t = fill_in("_t_(`r dendf`) = `r round(t, 2)`", c(e, list(dendf = x$fstatistic$dendf))),
        p = fill_in("`r apa6_p(p)`", e),
        ci = fill_in("`r options$ci*100`% CI [`r round(lb, 2)`; `r round(ub, 2)`]", c(e, list(options = options)))
        )
    }, x$coefficients)

  )

}
