## code to prepare `floodam-ead_DEM`

# Prior to run this script, install all suggested packages with `remotes::install_deps(dependencies = TRUE)`

# Function exported from floodam-ead 2022-02-03
#' @title Generate floodplain DEM including streams
#'
#' @param min_elevation numeric, floodplain minimum elevation
#' @param D numeric, lateral floodplain elevation range (across floodplain)
#' @param rad numeric, x and y extent for a cell in pixels
#' @param n_y numeric, number of cells along y axis
#' @param r numeric, raster resolution in m
#' @param slope numeric, upstream-downstream slope
#' @param epsg, integer, EPSG Geodetic Parameter. Default value corresponding
#'      Chaco area in Argentina (Def :UTM Zone 19S)
#' @param lower_left numeric, lower left, i.e. floodplain origin in metric
#'      geodesic system
#' @param s_depth numeric, stream depth in m
#' @param s_up_width numeric, upper riverbed width in m (assuming symetric and
#'      trapezoidal crosssection)
#' @param s_down_width numeric, lower riverbed width in m (assuming symetric
#'      and trapezoidal crosssection)
#' @param ridge_ratio numeric, width ratio for ridge position
#' @param lambda numeric,  sigmoid exponent controling bank slope
#' @param bank_width numeric, sigmoid width in pixels
#' @param random_microtopo logical, addition of random microtopo
#' @param seed integer, seed for generation of random microtopo
#' @param microtopo_sill numeric, square of macrotopo / microtopo ratio
#'      (default 0.05)
#' @param microtopo_range numeric, microtopo spatial range in pixels
#'
#' @return Raster object
#'
#' @export
#'
#' @examples
#' DEM <- generate_DEM()
#'
#' @encoding UTF-8
#' @author Jean-Stéphane BAILLY, \email{bailly@agroparistech.fr}
#' @author Frédéric Grelot, \email{frederic.grelot@inrae.fr}
generate_DEM = function(
  min_elevation = 10,
  D = 20,
  rad = 100,
  n_y = 4,
  r = 25,
  slope = 0.002,
  epsg = 32719,
  lower_left = c(100000, 100000),
  s_depth = 2,
  s_up_width = 100,
  s_down_width = 75,
  ridge_ratio = 1 / 3,
  lambda = 1,
  bank_width = rad / 5,
  random_microtopo = "FALSE",
  seed = 123,
  microtopo_sill = (0.02)^2,
  microtopo_range = rad / 10
) {
  # Cell design
  x <- matrix(1:rad, nrow = rad, ncol = rad, byrow = TRUE)
  y <- matrix(rad:1, nrow = rad, ncol = rad)
  # ridge shoulder location along x axis (in pixels)
  ridge_x <- (1 + sin(seq(pi / 2, 5 * pi / 2, length.out = rad))) / 2
  ridge_x <- round(1 + (ridge_x * ((ridge_ratio * rad) - 1)))
  sigm <- function(xi, lambda, bank_width) {
    1 / (1 + exp(lambda * (xi[-length(xi)] - xi[length(xi)]) / bank_width))
  }
  prov <- cbind(x, ridge_x)
  z0 <- t(apply(prov, 1, sigm, lambda = lambda, bank_width = bank_width))

  # Cell replication
  z0 <- cbind(z0, z0[, ncol(z0):1])
  z0 <- do.call(rbind, mget(paste0("z", rep(0, n_y))))

  # Adding local microtopo from spatial random realization
  if (random_microtopo == "TRUE") {
    set.seed(seed)
    mt <- as.matrix(
      RandomFields::RFsimulate(
        x = 1:(rad * n_y),
        y = 1:(rad * 2),
        model = RandomFields::RMgauss(
          var = microtopo_sill,
          scale = microtopo_range
        ),
        grid = TRUE
      )
    )
    z0 <- z0 + mt
  }

  # Elevation scaling
  z0 <- (z0 * D) + min_elevation

  # Stream concatenation
  s <- seq(0, (s_up_width / 2), by = r)
  zs <- rep(0, length(s))
  zs[s <= s_down_width / 2] <- -s_depth
  zs[s > s_down_width / 2] <-
    -s_depth +
    2 *
      s_depth *
      (s[s > s_down_width / 2] - s_down_width / 2) /
      (s_up_width - s_down_width)
  zs <- c(rev(zs[-1]), zs)
  zs <- as.matrix(do.call(rbind, mget(rep("zs", nrow(z0)))))
  zs <- zs + min_elevation
  z0 <- cbind(
    z0[, 1:(ncol(z0) / 2)],
    zs,
    z0[, (1 + ncol(z0) / 2):ncol(z0)]
  )

  # Upstream-downstream slopping
  z0 <- z0 + (nrow(z0) + 1 - row(z0)) * r * slope

  # Shape result
  result <- terra::rast(
    z0,
    extent = terra::ext(
      lower_left[1],
      lower_left[1] + (r * (rad * 2 + ncol(zs))),
      lower_left[2],
      lower_left[2] + (r * rad * n_y)
    ),
    crs = sprintf("epsg:%s", epsg)
  )

  invisible(return(result))
}

floodam_ead_dem <- terra::wrap(generate_DEM())

usethis::use_data(floodam_ead_dem, overwrite = TRUE)
