data("floodam_ead_dem")
dem <- terra::rast(floodam_ead_dem)

node_coords <- matrix(c(102550, 102550, 110000, 100000), ncol = 2)

extent <- terra::ext(dem)
maj_section_width <- as.numeric(extent$xmax - extent$xmin)

nb_sections <- ceiling(10000 / 25) + 1
section_centers <- get_section_centers(node_coords, nb_sections)

test_that("dem_to_reach works",{
  reach <- dem_to_reach(dem, node_coords, section_centers, section_width = maj_section_width)
})
