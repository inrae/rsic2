#' file.path for Windows
#'
#' Same behavior as [file.path] but remove duplicated slashes and use backslashes as the path separator.
#'
#' @param ... Arguments sent to [file.path]
#'
file.pathwin <- function(...) {
  sPath = file.path(...)
  sPath = gsub("\\\\", "/", sPath) # Remplace les antislashes présents (bug avec la command system)
  sPath = gsub("//", "/", sPath) #Suppression des séparateurs en double
  sPath = gsub("/", "\\\\", sPath) #Remplacement des slashs par des anti-slashes
  return(sPath)
}
