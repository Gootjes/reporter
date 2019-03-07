context("Finding functions")

test_that("A function can be found based on a function or character", {
  expect_equal((function(fun){

    if("function" %in% class(fun)) {
      f <- .find(f = as.character(match.call()$fun))
    } else if("character" %in% class(fun)) {
      f <- .find(f = fun)
    } else {
      stop("Unsupported function class: ", class(fun))
    }

    f
    })(fun = lm), "stats::lm")
})
