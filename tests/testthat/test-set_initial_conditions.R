skip_on_ci()

test_that("set_initial_conditions works", {
  cfg <- cfg_tmp_project()
  sic_run_steady(cfg, scenario = 1)
  set_initial_conditions(c(1, 0, 0, 1, 1), cfg = cfg)
  sic_run_unsteady(cfg, scenario = 1, variant = 1)
  expect_true(file.exists(gsub("\\.xml", "_1_1.res", cfg$project$path)))
})
