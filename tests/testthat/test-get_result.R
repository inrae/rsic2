skip_on_ci()

cfg <- cfg_tmp_project()
sic_run_fortran("fluvia", list(SCE = 1), cfg = cfg)

test_that("get_result returns a matrix with correct colnames", {
  result <- get_result(cfg, 1, filters = c("bf=4", "var='Z'"))
  expect_true(is.matrix(result))
  expect_type(result, "double")
  expect_equal(colnames(result), sprintf("bf:4|sn:%d|var:Z", 1:4))
})
