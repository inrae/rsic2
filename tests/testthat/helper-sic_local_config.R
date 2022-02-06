# Set local configuration for SIC tests
loadLocalConfig <- function() {
  xml_path <- tempfile("sic_project", fileext = ".xml")
  file.copy(system.file("sic_project_test1.xml", package = "rsic2"),
            xml_path,
            overwrite = TRUE)
  loadConfig(xml_path = xml_path)
}
