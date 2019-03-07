context("stats..lm: extraction")

m <- stats::lm(mpg ~ cyl * gear, data = mtcars)

test_that("The function exists and a reporter.Extract object is returned", {
  expect_equal(class(extract(x = m, fun = lm, options = list(ci = .95))), "reporter.Extract")
})

test_that("The first estimate is rendered properly", {
  expect_equal(report(x = m, fun = lm, options = list(ci = .95))$estimates[[1]]$Estimate, "_B_ = 17.16")
})

test_that("The ci of an estimate is rendered properly", {
  expect_equal(report(x = m, fun = lm)$estimates[[1]]$ci, "95% CI [-12.69; 47.02]")
})
