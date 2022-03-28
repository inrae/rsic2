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
#' reach <- merge(min_reach, maj_reach)
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
  ret <- shell(
    cmd_line,
    wait = T,
    translate = T
  )
  update_portion_abscissas(cfg)
  return(ret)
}

write_reach_txt <- function(file, reach) {
  s <- unlist(reach)
  writeLines(s, file)
}

#' Update abscissas of Strickler portions in a SIC model
#'
#' @description
#' As [sic_import_reaches] redefines abscissas in the reaches, and as the Stricklers
#' are defined with abscissas, geometry importation can lead to an inconsistent model.
#'
#' So the aim of this function is to reset Strickler definitions of the whole model
#' to a default value with one Strickler portion by reach with correct abscissas.
#'
#' @template param_cfg
#' @param stricklers 2-length [numeric], Strickler coefficient to apply for minor bed and medium bed respectively
#'
#' @return Use for side effect on the XML project file.
#'
#' @family sic_import_reaches
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cfg <- cfg_tmp_project()
#' update_portion_abscissas(cfg, KMin = 50)
#'
#' # Display first Strickler definitions encountered in the XML file
#' x <- read_xml(cfg$project$path)
#' xml_find_first(x, "//Stricklers")
#' }
update_portion_abscissas <- function(cfg,
                                     stricklers = cfg$project$stricklers) {
  x <- read_xml(cfg$project$path)
  x_biefs <- xml_find_all(x, "//Bief")
  lapply(x_biefs, function(x) {
    XD <- x %>% xml_find_first(".//Liste_Sections/SectionMin") %>% xml_attr("abscisse")
    XF <- x %>% xml_find_last(".//Liste_Sections/SectionMin") %>% xml_attr("abscisse")
    x_stricklers <- xml_find_all(x, ".//Stricklers")
    lapply(x_stricklers, function(x) {
      x_portion <- xml_find_first(x, "./Portion")
      xml_attr(x_portion, "XD") <- XD
      xml_attr(x_portion, "XF") <- XF
      xml_set_text(xml_child(x_portion, "KMin"), as.character(stricklers[1]))
      xml_set_text(xml_child(x_portion, "KMoy"), as.character(stricklers[2]))
      # Delete other defined portions in the reach
      other_portions <- xml_siblings(x_portion)
      lapply(other_portions, xml_remove)
    })
  })
  write_xml(x, cfg$project$path)
}

xml_find_last <- function(x, path) {
  x_all <- xml_find_all(x, path)
  return(x_all[length(x_all)])
}
