skip_on_ci()

cfg <- cfg_tmp_project()

test_that("fluvia with scenario = 1 should create a binary result file", {
  sic_run_steady(cfg, scenario = 1)
  expect_true(file.exists(gsub("\\.xml", "_1_0.res", cfg$project$path)))
  expect_true(file.exists(gsub("\\.xml", "_1_0.rci", cfg$project$path)))
})

test_that("talweg should update xml project", {
  mtime_before <- file.mtime(cfg$project$path)
  sic_run_mesh(cfg)
  expect_gt(file.mtime(cfg$project$path), mtime_before)
})

test_that("'One call' unsteady flow simulation works", {
  cfg <- cfg_tmp_project()
  sic_run_unsteady(cfg, iniParams = c(1, 0, 0, 1, 1))
  expect_true(file.exists(gsub("\\.xml", "_1_1.res", cfg$project$path)))
})
