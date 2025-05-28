#' Copy an XML section from one reach to another
#'
#' @param source_reach [integer] the reach number of the source section
#' @param source_section [integer] the section number of the source reach
#' @param target_reach [integer] the reach number of the target section
#' @param target_section [integer] the section number of the target reach
#' @inheritParams get_xml_node_num_by_name
#' @param write_xml [logical] if `TRUE`, the XML file is saved after modification (Needs that `cfg` is provided)
#'
#' @returns Nothing. Use for side effect on the XML project file.
#' @export
#'
copy_xml_section <- function(
  source_reach,
  source_section,
  target_reach,
  target_section,
  xp = xml2::read_xml(cfg$project$path),
  write_xml = TRUE,
  cfg
) {
  # Get the source section
  source_section_node <- xp %>%
    xml2::xml_find_first(sprintf(
      "/Reseau/Liste_Biefs/Bief[@Num=%s]/Liste_Sections/SectionMin[@Num=%s]",
      as.character(source_reach),
      as.character(source_section)
    ))

  # Get the target section
  target_section_node <- xp %>%
    xml2::xml_find_first(sprintf(
      "/Reseau/Liste_Biefs/Bief[@Num=%s]/Liste_Sections/SectionMin[@Num=%s]",
      as.character(target_reach),
      as.character(target_section)
    ))

  xml2::xml_replace(target_section_node, source_section_node)

  # Save the modified XML file
  if (write_xml) xml2::write_xml(xp, cfg$project$path)
}
