# Minor bed generation
profT <- list(
  B = 2,
  S = (6 - 2) / 2 / 2,
  ZF = 100,
  ZB = 100 + 2
)
min_reach <- create_uniform_reach_txt(
  abscissas = seq(0, 10000, 500),
  upstream_bed_elevation = 10 + 2000 * 0.002,
  slope = 0.002,
  section_type = "T",
  profile = profT
)

test_that("extract_reach works", {
  sel_reach <- extract_reach(min_reach, c(2000, 4000))
  expect_s3_class(sel_reach, "ReachTxt")
  expect_s3_class(sel_reach[[1]], "SectionTxt")
  expect_length(sel_reach, 5)
})

test_that("split_reach works", {
  reaches <- split_reach(min_reach, seq(0, 10000, 2000))
  expect_length(reaches, 5)
})

test_that("Major bed section should have correct lengths", {
  # Major bed generation
  data("floodam_ead_dem")
  dem <- terra::rast(floodam_ead_dem)
  node_coords <- matrix(c(102550, 102550, 110000, 100000), ncol = 2)
  space_step = 25
  section_width = 5000
  maj_reach <- dem_to_reach_txt(
    dem,
    node_coords,
    space_step,
    section_width,
    major_bed = TRUE
  )
  reaches <- split_reach(maj_reach, seq(0, 10000, 5000))
  sn <- reaches[[1]][[length(reaches[[1]])]]
  s_cfg <- strsplit(sn[1], "$", fixed = TRUE)[[1]]
  expect_equal(s_cfg[3], " 0 ")
})
