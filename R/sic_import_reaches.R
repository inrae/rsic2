#' Import reach geometries into a SIC project
#'
#' @param reaches A [list] of ReachTxt objects (See [split_reach])
#' @param import_mode [character], importation mode of EdiSIC (See \url{https://sic.g-eau.fr/Import-sections-in-text-format?lang=en#import-modes-2})
#' @template param_cfg
#'
#' @return A [numeric] code returned by [shell]
#' @export
#'
#' @examples
#' # Minor bed generation
#' profT <- matrix(c(2, 6, 0, 2), ncol = 2)
#' min_reach <- create_uniform_reach_txt(abscissas = seq(0, 10000, 100),
#'                                       upstream_bed_elevation = 8 + 10000 * 0.002,
#'                                       slope = 0.002,
#'                                       section_type = "L",
#'                                       profile = profT)
#'
#' # Major bed generation
#' data("floodam_ead_dem")
#' dem <- terra::rast(floodam_ead_dem)
#' node_coords <- matrix(c(102550, 102550, 110000, 100000), ncol = 2)
#' space_step = 100
#' section_width = 5000
#' maj_reach <- dem_to_reach_txt(dem, node_coords, space_step, section_width, major_bed = TRUE)
#'
#' # Merge minor and major beds and split into 2 reaches
#' reach <- merge_reaches(min_reach, maj_reach)
#' reaches <- split_reach(reach, seq(0, 10000, 5000))
#'
#' \dontrun{
#' # Import with EdiSic
#' cfg <- cfg_tmp_project()
#' sic_import_reaches(reaches, cfg = cfg)
#' }
sic_import_reaches <- function(reaches, import_mode = "ImportXml_UPDATE", cfg = loadConfig()) {
  # Create reach files
  import_path <- dirname(cfg$project$path)
  reach_files <- sapply(seq_along(reaches), function(i) {
    file <- file.path(import_path, sprintf("reach_%04d.txt", i))
    write_reach_txt(file, reaches[[i]])
    return(shQuote(basename(file), type = "cmd"))
  })

  # Import files with EdiSic https://sic.g-eau.fr/Import-sections-in-text-format?lang=en
  project_name <- basename(tools::file_path_sans_ext(cfg$project$path))
  cmd_line <- shQuote(
    paste(
      file.path(cfg$sic$path, cfg$sic$edisic),
      shQuote(dirname(cfg$project$path), type = "cmd"),
      import_mode,
      shQuote(project_name, type = "cmd"),
      paste(reach_files, collapse = " ")
    ),
    type = "cmd2"
  )
  logger::log_debug(cmd_line)
  shell(
    cmd_line,
    wait = T,
    translate = T
  )
}

write_reach_txt <- function(file, reach) {
  s <- unlist(reach)
  writeLines(s, file)
}
