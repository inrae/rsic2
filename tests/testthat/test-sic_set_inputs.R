locations <- SicLocations(list(bf = 1, sn = 1, car = "Z"),
                          list(Nd = 1, Pr = 1, car = "Q"))
input <- SicInput(5, locations = locations)
dfTest <- data.frame(t = seq(0, by = 600, length.out = 11),
                     v = sin(0:10))
input2 <- SicInput(dfTest, locations = SicLocation(list(Nd = 1, Car = "Z")))

test_that("SicLocation shoud return errors with incoherent parameters", {
  expect_error(SicLocation(list(bf = 1, sn = 2)),
               regexp = "Each location should have at least an item 'CAR'")
  expect_error(SicLocation(list(bf = 1, nd = 2, car = "Q")),
               regexp = "These items can't be together in a location", fixed = TRUE)
})

test_that("SicLocation should works", {
  expect_s3_class(SicLocation(list(bf = 1, sn = 1, car = "Q")), "SicLocation")
  expect_equal(unclass(SicLocation(list(bf = 1, sn = 1, car = "Q"))),
               "BF=1\tSN=1\tCAR=Q")
})

test_that("SicLocations should work", {
  expect_s3_class(SicLocations(list(list(bf = 1, sn = 1, car = "Q"))), "SicLocations")
  expect_equal(SicLocations(list(list(bf = 1, sn = 1, car = "Q")))[1],
               "BF=1\tSN=1\tCAR=Q")
})

test_that("SiCinput should work with fixed value", {
  input <- SicInput(5, locations = locations)
  expect_s3_class(input, "SicInput")
  expect_equal(input$locations, locations)
  expect_equal(input$data, 5)

})

test_that("SiCinput should work with time in seconds", {
  input <- SicInput(dfTest$t, dfTest$v, locations = locations)
  expect_equal(input$data, dfTest)
})

test_that("SiCinput should work with time in POSIXt", {
  input <- SicInput(seq(as.POSIXct("2020-01-05 00:00:00", tz = "UTC"),
                        by = 600, length.out = 11),
                    dfTest$v,
                    locations = locations)
  expect_equal(input$data, dfTest)
})

test_that("SicInput should work with data.frame and matrix", {
  input <- SicInput(dfTest, locations = locations)
  expect_equal(input$data, dfTest)
  input <- SicInput(as.matrix(dfTest), locations = locations)
  expect_equal(input$data, dfTest)
})


test_that("merge.SicInputs should work", {
  inputs <- merge(input)
  expect_length(inputs, 1)
  inputs <- merge(input, input2)
  expect_length(inputs, 2)
  expect_equal(unclass(inputs[[2]]$locations), "ND=1\tCAR=Z")
})

skip_on_ci()
cfg <- cfg_tmp_project()

test_that("sic_write_par should return errors with wrong parameters", {
  expect_error(sic_write_par("toto", 1, input), regexp = "loadConfig")
  expect_error(sic_write_par(cfg, "toto", input), regexp = "is.numeric")
  expect_error(sic_write_par(cfg, 1, list()), regexp = "sicInputs")
  expect_error(sic_write_par(cfg, c(1, 2), input), regexp = "length")
})

test_that("sic_write_par should works", {
  sic_write_par(cfg, 1, merge(input, input2))
  file <- sic_get_par_filename(cfg, 1)
  expect_true(file.exists(file))
  s <- readLines(file)
  expect_equal(gsub("(\t)+$", "", s[2]), "L1\tBF=1\tSN=1\tCAR=Z")
  expect_equal(gsub("(\t)+$", "",s[4]), "X1\t5")
})
