
#' @export
extract.stats..lm <- function(x, options) {

  if(is.null(options)) {
    options <- list(ci = .95)
  }

  s <- stats::summary.lm(x)

  l <- list(

    rsquared = s$r.squared,
    rsquared_adjusted = s$adj.r.squared,
    fstatistic = to_list(s$fstatistic),
    .x = x

  )

  co <- list(
    coefficients = to_list(cbind(s$coefficients, confint(x, level = options$ci)), new_colnames = c("B","SE","t","p", "lb", "ub"))
    )

  reporter_attr <- c(attributes(x)$reporter,
                     list(
                       extract.type = c("stats..lm", "LinearModel")
                          )
                     )

  structure(c(l, co), class = c("reporter.Extract"), reporter = reporter_attr)

}
