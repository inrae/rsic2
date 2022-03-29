#' Tidy a result simulation
#'
#' @param res a *SicResult* [matrix] provided by [get_result]
#'
#' @return A [data.frame] with one line per saved simulation result time step :
#'
#' - one column per object type of the result (example: "bf" and "sn" for a section or "nd" and "pr" for an offtake)
#' - one column "var" for the definition of the result variable
#' - one column "t" for the simulation time of the result variable
#' - one columne "value" for the value of the result variable
#'
#' @export
#'
#' @examples
tidy_result <- function(res) {
  stopifnot(inherits(res, "SicResult"))
  res <- as.data.frame(res)
  df <- tidyr::gather(res, key = "key", value = "value", -"t")
  keys <- strsplit(df$key, "|", fixed = TRUE)
  l <- lapply(keys, function(x) {
    l <- lapply(strsplit(x, ":", fixed = TRUE),
                function(obj) {
                  df <- data.frame(x = obj[2])
                  names(df) <- obj[1]
                  return(df)
                })
    do.call(cbind, l)
  })
  df_obj <- do.call(rbind, l)
  df$key <- NULL
  cbind(df_obj, df)
}
