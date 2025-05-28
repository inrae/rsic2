#' Tidy a result simulation
#'
#' @param res a *SicResult* [matrix] provided by [get_result]
#'
#' @return `tidy_result` returns a [data.frame] with one line by variable and by saved simulation result time step :
#'
#' - one column per object type of the result (example: "bf" and "sn" for a section or "nd" and "pr" for an offtake)
#' - one column "var" for the definition of the result variable
#' - one column "t" for the simulation time of the result variable
#' - one columne "value" for the value of the result variable
#'
#' `compact_tidy_result` returns a [data.frame] with one line by variable :
#'
#' - one column per object type of the result (example: "bf" and "sn" for a section or "nd" and "pr" for an offtake)
#' - one column "var" for the definition of the result variable
#' - one column "values" containing a [data.frame]  with a column `value`
#'
#' The data.frame contains an attribute "t" with the time of the saved simulation time steps in seconds.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cfg <- cfg_tmp_project()
#' # Run unsteady flow simulation (flood over one day)
#' sic_run_unsteady(cfg, iniParams = c(1, 0, 0, 1, 1))
#' # Get water elevation in reach #2, section #2
#' m <- get_result(cfg, 1, 1,
#'                   filters = c("bf=2", "sn=2", "var='Z'"))
#' res <- tidy_result(m)
#' plot(res$t, res$value)
#' # Formatting can be called directly through argument
#' # `fun_format` of `get_result` function
#' res <- get_result(cfg, 1, 1,
#'                   filters = c("bf=2", "sn=2", "var='Z'"),
#'                   fun_format = compact_tidy_result))
#' # Plot result in first object against simulation time
#' plot(attr(res, "t"), res$values[1][[1]])
#' }
tidy_result <- function(res) {
  stopifnot(inherits(res, "SicResult"))
  res <- as.data.frame(res)
  df <- tidyr::gather(res, key = "key", value = "value", -"t")
  keys <- strsplit(df$key, "|", fixed = TRUE)
  l <- lapply(keys, function(x) {
    l <- lapply(strsplit(x, ":", fixed = TRUE), function(obj) {
      df <- data.frame(x = obj[2])
      if (obj[1] != "var") {
        if (obj[1] == "t") {
          df$x <- as.numeric(df$x)
        } else {
          df$x <- as.integer(df$x)
        }
      }
      names(df) <- obj[1]
      return(df)
    })
    do.call(cbind, l)
  })
  df_obj <- do.call(rbind, l)
  df$key <- NULL
  df <- cbind(df_obj, df)
  class(df) <- c("SicResultTidy", class(df))
  df
}

#' @rdname tidy_result
#' @export
compact_tidy_result <- function(res) {
  res1 <- res[1, , drop = FALSE]
  class(res1) <- class(res)
  df <- tidy_result(res1)
  cols <- seq_len(nrow(df))
  names(cols) <- colnames(res)[-1]
  l <- lapply(cols, function(i) {
    res[, 1 + i]
  })
  df$values <- l
  attr(df, "t") <- res[, 1]
  df$t <- NULL
  df$value <- NULL
  class(df) <- c("SicResultCompact", class(df))
  df
}
