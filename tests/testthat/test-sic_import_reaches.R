skip_on_ci()

cfg <- cfg_tmp_project()

# Minor bed generation
profT <- matrix(c(2, 6, 0, 2), ncol = 2)
min_reach <- create_uniform_reach_txt(abscissas = seq(0, 10000, 100),
                                      upstream_bed_elevation = 8 + 10000 * 0.002,
                                      slope = 0.002,
                                      section_type = "L",
                                      profile = profT)

# Major bed generation
data("floodam_ead_dem")
dem <- terra::rast(floodam_ead_dem)
node_coords <- matrix(c(102550, 102550, 110000, 100000), ncol = 2)
space_step = 100
section_width = 5000
maj_reach <- dem_to_reach_txt(dem, node_coords, space_step, section_width, major_bed = TRUE)

# Merge minor and major beds and split into 2 reaches
reach <- merge_reaches(min_reach, maj_reach)
reaches <- split_reach(reach, seq(0, 10000, 5000))

test_that("Geometry Import works", {
  sic_import_reaches(reaches, cfg = cfg)
})
