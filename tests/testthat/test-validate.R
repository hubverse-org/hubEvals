test_that("get_transform_label returns bare function name", {
  expr <- rlang::expr(sqrt)
  expect_equal(get_transform_label(expr, NULL), "sqrt")
})


test_that("get_transform_label strips namespace from namespaced function", {
  expr <- rlang::expr(scoringutils::log_shift)
  expect_equal(get_transform_label(expr, NULL), "log_shift")
})


test_that("get_transform_label returns user-provided label when given", {
  expr <- rlang::expr(sqrt)
  expect_equal(get_transform_label(expr, "my_label"), "my_label")

  # Also works with anonymous function expression when label provided
  expr_anon <- rlang::expr(function(x) log(x + 1))
  expect_equal(get_transform_label(expr_anon, "log1p"), "log1p")
})


test_that("get_transform_label errors for anonymous function without label", {
  expr <- rlang::expr(function(x) log(x + 1))
  expect_error(
    get_transform_label(expr, NULL),
    regexp = "transform_label.*required.*anonymous transform function"
  )
})


test_that("validate_relative_metrics rejects bias as a relative metric", {
  expect_error(
    validate_relative_metrics(
      relative_metrics = "bias",
      metrics = c("wis", "bias"),
      by = "model_id"
    ),
    regexp = "bias.*not supported for relative skill scores"
  )

  # Mixed with a valid relative metric: still errors
  expect_error(
    validate_relative_metrics(
      relative_metrics = c("wis", "bias"),
      metrics = c("wis", "bias"),
      by = "model_id"
    ),
    regexp = "bias.*not supported for relative skill scores"
  )
})
