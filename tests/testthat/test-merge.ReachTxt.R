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

# Major bed generation
data("floodam_ead_dem")
dem <- terra::rast(floodam_ead_dem)
node_coords <- matrix(c(102550, 102550, 110000, 100000), ncol = 2)
space_step = 500
section_width = 5000
maj_reach <- dem_to_reach_txt(
  dem,
  node_coords,
  space_step,
  section_width,
  major_bed = TRUE
)

test_that("merge.ReachTxt should work", {
  reach <- merge(min_reach, maj_reach)
  expect_s3_class(reach, "ReachTxt")
  expect_length(reach, length(min_reach) + length(maj_reach))
})
