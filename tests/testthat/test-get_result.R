skip_on_ci()

cfg <- cfg_tmp_project()
sic_run_steady(cfg, scenario = 1)

test_that("get_result returns a matrix with correct colnames", {
  result <- get_result(cfg, 1, filters = c("bf=4", "var='Z'"), tidy = FALSE)
  expect_true(is.matrix(result))
  expect_type(result, "double")
  expect_equal(colnames(result), c("t", sprintf("bf:4|sn:%d|var:Z", 1:4)))
})

test_that("get_result with tidy return a tidy result", {
  result <- get_result(cfg, 1, filters = c("bf=4", "var='Z'"))
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("bf", "sn", "var", "t", "value"))
})
