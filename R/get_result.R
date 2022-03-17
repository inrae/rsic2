#' Get a selection of variables from a simulation result
#'
#' @inheritParams sic_run_export
#' @param filters [character] conditions to select columns in result table, see details
#' @param m [matrix] of results produced by [read_bin_result_matrix]
#'
#' @return [matrix] of results with columns selected by `filters`.
#' @export
#' @import magrittr
#'
#' @examples
#' cfg <- cfg_tmp_project()
#' sic_run_fortran("fluvia", list(SCE = 1), cfg = cfg)
#' get_result(cfg, 1, filters = c("bf==4", "var=='Z'"))
get_result <- function(cfg,
                       scenario,
                       variant = 0,
                       filters = c(""),
                       m = read_bin_result_matrix(cfg, scenario, variant)) {

  df_col <- get_result_tree(cfg, scenario, variant)
  filters <- paste(filters, collapse = " AND ")
  if (filters != "") {
    df_col %<>% tidyquery::query(paste("SELECT * WHERE", filters))
  }
  m <- m[, df_col$col, drop = FALSE]
  column_names <- sapply(seq_len(nrow(df_col)),
                   function(i) {
                     df_col$col <- NULL
                     cols <- sapply(names(df_col),
                                    function(name) {
                                      if (df_col[i, name] > 0) {
                                        paste(name, df_col[i, name], sep = ":")
                                      } else {
                                        NULL
                                      }
                                    })
                     cols[sapply(cols, is.null)] <- NULL
                     paste(cols, collapse = "|")
                   })
  colnames(m) <- column_names
  return(m)
}


#' Read matrix of SIC simulation result file
#'
#' @inheritParams sic_run_export
#'
#' @return [matrix] with the simulation results
#' @export
#'
#' @examples
#' cfg <- cfg_tmp_project()
#' sic_run_fortran("fluvia", list(SCE = 1), cfg = cfg)
#' m <- read_bin_result_matrix(cfg, 1)
#' str(m)
read_bin_result_matrix <- function(cfg, scenario, variant = 0) {
  file <- paste0(
    paste(gsub("\\.xml", "", cfg$project$path),
          scenario, variant, sep = "_"),
    ".res"
  )
  con = file(file, "rb")
  # Skip header
  readBin(con, "raw", n = 4 * 5 + 4 + 8 + 4 + 4)
  dims <-
    readBin(con,
            "integer",
            n = 2,
            size = 4,
            endian = "little")
  # Skip (data type code?)
  readBin(con, "raw", n = 2)
  data <-
    readBin(con,
            "double",
            n = prod(dims),
            size = 4,
            endian = "little")
  readBin(con, "raw", n = 4) # @todo check end file content
  close(con)
  return(matrix(data, ncol = dims[2], byrow = TRUE))
}


#' Get correspondence between network object and columns in result binary file
#'
#' @inheritParams sic_run_export
#'
#' @return a [data.frame] with following columns:
#'
#' - "bf", "sn", "nd", "pr", "ouv": location of the result with number of respectively reach, section, node, offtake, and device.
#' - "var": the name of the calculated variable
#' - "col": the column number in the matrix produced by [read_bin_result_matrix]
#'
#' @warning
#' Up to now, this function only handle results at sections.
#'
#' @export
#' @import xml2
#' @import magrittr
#'
#' @examples
#' cfg <- cfg_tmp_project()
#' sic_run_fortran("fluvia", list(SCE = 1), cfg = cfg)
#' df <- get_result_tree(cfg, 1)
#' head(df)
get_result_tree <- function(cfg, scenario, variant = 0) {
  x <- read_xml(cfg$project$path)
  objs = c("Ouvrage", "Section", "Prise", "Noeud")
  names(objs) <- objs
  defcol <- get_DefCol(x, scenario, variant)

  df <- data.frame(bf = integer(),
                   sn = integer(),
                   nd = integer(),
                   pr = integer(),
                   ouv = integer(),
                   var = character(),
                   col = integer())

  # Sections
  xpath_biefs <- "/Reseau/Liste_Biefs/Bief"

  for (iBf in seq_along(xml_find_all(x, xpath_biefs))) {
    xpath_sections <- paste0(xpath_biefs, sprintf("[@Num=%d]/Liste_Sections/SectionMin", iBf))
    for (iSn in seq_along(xml_find_all(x, xpath_sections))) {
      xpath_res <- paste0(xpath_sections, "[@Num=%d]/Flu[@nScenario=%d]/ListeRes/Res[@nVar=%d]") %>%
        sprintf(iSn, scenario, variant)
      cols <- x %>% xml_find_first(xpath_res) %>%
        xml_attr("nCol") %>% strsplit(":") %>% "[["(1) %>% as.integer
      cols <- seq(from = cols[1], length.out = cols[2])

      df %<>% result_tree_add(list(bf = iBf, sn = iSn),
                              defcol$Section,
                              cols)
    }
  }
  return(df)
}

result_tree_add <- function(df, loc, defcol, cols) {
  loc <- modifyList(list(bf = 0, sn = 0, nd = 0, pr = 0, ouv = 0),
                           loc)
  return(rbind(df, data.frame(as.data.frame(loc), var = defcol, col = cols)))
}

get_DefCol <-
  function(x,
           scenario,
           variant,
           xpath = "/Reseau/Flu[@nScenario=%d]/ListeRes/Res[@nVar=%d]/ListeDefCol/DefCol[@Objet=\"%s\"]",
           objs = c("Ouvrage", "Section", "Prise", "Noeud")) {
    names(objs) <- objs
    lapply(objs, function(obj)
      get_text_xml_path(x, xpath, scenario, variant, obj))
  }

get_text_xml_path <- function(x, xpath, scenario, var, obj) {
  x %>% xml_find_first(xpath = sprintf(xpath, scenario, var, obj)) %>%
    xml_text %>% strsplit("\t") %>% "[["(1)
}
