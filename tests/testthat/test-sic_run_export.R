skip_on_ci()

cfg <- cfg_tmp_project()

test_that("RunExport on Fluvia run works", {
  sic_run_steady(cfg, scenario = 1)
  m <- sic_run_export(scenario = 1, params = list(t = 0), cfg = cfg)
  expect_type(m, "double")
  expect_equal(colnames(m)[1:3], c("Bief", "Section", "Abscisse"))
})
