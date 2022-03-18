skip_on_ci()

test_that("set_initial_conditions works", {
  cfg <- cfg_tmp_project()
  sic_run_fortran("fluvia", list(SCE = 1), cfg = cfg)
  set_initial_conditions(c(1, 0, 0, 1, 1), cfg = cfg)
  sic_run_fortran("sirene", list(SCE = 1, VAR = 1), cfg = cfg)
  expect_true(file.exists(gsub("\\.xml", "_1_1.res", cfg$project$path)))
})
