test_that("incomplete config throw an error", {
  expect_error(loadConfig())
})

sic_path <- tempdir(check = TRUE)
xml_path <- tempfile(fileext = ".xml")
writeLines("<XML>Fake project</XML>", xml_path)

test_that("Wrong SIC path throw an error", {
  expect_error(loadConfig(file.path(sic_path, "fake"), xml_path))
})

test_that("Wrong XML path throw an error", {
  expect_error(loadConfig(sic_path, file.path(xml_path, "fake.xml")))
})

test_that("SIC and XML paths injected by parameters should be provided in cfg", {
  cfg <- loadConfig(sic_path, xml_path)
  expect_equal(cfg$sic$path, sic_path)
  expect_equal(cfg$project$path, xml_path)
})

test_that("SIC and XML paths injected by user yml file should be provided in cfg", {
  tmpCfgPath <- tempfile(fileext = ".yml")
  ymlCfg <- list(default = list(
    sic = list(path = sic_path),
    project = list(path = xml_path)
  ))
  yaml::write_yaml(ymlCfg, tmpCfgPath)
  cfg <- loadConfig(userFile = tmpCfgPath)
  expect_equal(cfg$sic$path, sic_path)
  expect_equal(cfg$project$path, xml_path)
})
