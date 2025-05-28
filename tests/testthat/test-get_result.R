skip_on_ci()

cfg <- cfg_tmp_project()
sic_run_unsteady(cfg, iniParams = c(1, 0, 0, 1, 1))

test_that("get_result returns a matrix with correct colnames", {
  result <- get_result(cfg, 1, filters = c("bf=4", "var='Z'"))
  expect_true(is.matrix(result))
  expect_type(result, "double")
  expect_equal(colnames(result), c("t", sprintf("bf:4|sn:%d|var:Z", 1:4)))
})

test_that("get_result with tidy return a tidy result", {
  result <- get_result(
    cfg,
    1,
    filters = c("bf=4", "var='Z'"),
    fun_format = tidy_result
  )
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("bf", "sn", "var", "t", "value"))
})

test_that("get_result with tidy return a tidy result", {
  result <- get_result(
    cfg,
    1,
    1,
    filters = c("bf=4", "var='Z'"),
    fun_format = compact_tidy_result
  )
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("bf", "sn", "var", "values"))
  expect_equal(attr(result, "t"), seq(0, 86400, by = 60))
  expect_type(result$bf, "integer")
})

test_that("flow in first section must be equal to injected flow", {
  t <- seq(0, 86400, by = 600)
  dfTest <- data.frame(t = t, v = 100 * sin((seq_along(t) - 1) * pi / 16) + 200)
  input <- SicInput(
    dfTest,
    locations = SicLocation(list(Nd = 1, Pr = 1, Car = "Q"))
  )

  sic_run_unsteady(
    cfg,
    iniParams = c(1, 0, 0, 1, 1),
    sicInputs = input,
    params = list(DT = "600")
  )
  result <- get_result(
    cfg,
    1,
    1,
    filters = c("bf=1", "sn=1", "var='Q'"),
    fun_format = compact_tidy_result
  )
  expect_equal(result$values[1][[1]], dfTest$v)
})
