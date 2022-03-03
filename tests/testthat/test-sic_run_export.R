skip_on_ci()

cfg <- loadLocalConfig()

test_that("RunExport on Fluvia run works", {
  sic_run_fortran("fluvia", list(SCE = 1), cfg = cfg)
  m <- sic_run_export(scenario = 1, params = list(t = 0), cfg = cfg)
  expect_type(m, "double")
  expect_equal(colnames(m)[1:3], c("Bief", "Section", "Abscisse"))
})
