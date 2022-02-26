skip_on_ci()

cfg <- loadLocalConfig()

test_that("fluvia on SCE=1 should create a binary result file", {
  sic_run_fortran("fluvia", list(SCE = 1), cfg = cfg)
  expect_true(file.exists(gsub("\\.xml", "_1_0.res", cfg$project$path)))
  expect_true(file.exists(gsub("\\.xml", "_1_0.rci", cfg$project$path)))
})