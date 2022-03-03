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
