#' Get PAR filename for a given scenario in a SIC project
#'
#' @inheritParams sic_run_export
#'
#' @return A [character] with the path of the PAR file
#' @noRd
#'
sic_get_par_filename <- function(cfg, scenario) {
  x <- read_xml(cfg$project$path)
  xPath <- sprintf("/Reseau/Flu[@nScenario=%d]/NomFicPar", scenario)
  file <- x %>% xml_find_first(xPath) %>% xml_text
  return(file.path(dirname(cfg$project$path), file))
}

#' Write a PAR file for SIC simulation
#'
#' Write inputs  in a PAR file respectively to \url{https://sic.g-eau.fr/Format-of-the-par-file}.
#'
#' @inheritParams sic_run_mesh
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the model configuration
#' cfg <- cfg_tmp_project()
#'
#' # How to impose an hydrograph at the upstream offtake of the model?
#' # Define the location of the upstream boundary condition to set
#' locations <- SicLocations(list(Nd = 1, Pr = 1, car = "Q"))
#' # Define its time series
#' sicInputs <- SicInput(data.frame(t = c(0, 3600, 7200), # time in seconds
#'                                 v = c(5, 20, 5)),     # flows
#'                       locations = locations)
#' # Write the parameters in the PAR file
#' sic_write_par(cfg, 1, sicInputs)
#' }
sic_write_par <- function(cfg, scenario, sicInputs) {
  if (is.null(attr(cfg, "config"))) {
    stop("`cfg` should be created with `loadConfig()`")
  }
  stopifnot(
    is.numeric(scenario),
    length(scenario) == 1,
    inherits(sicInputs, "SicInput") || inherits(sicInputs, "SicInputs")
  )
  if (inherits(sicInputs, "SicInput")) sicInputs <- merge(sicInputs)

  sParFileName = sic_get_par_filename(cfg, scenario)

  df = data.frame(
    C1 = "// PAR file for SIC",
    C2 = "automatically generated by rsic2",
    C3 = "",
    stringsAsFactors = FALSE
  )
  i = 0
  for (input in sicInputs) {
    i = i + 1
    # Localisation
    if (is.null(input$locations)) {
      dfLoc <- NULL
    } else {
      dfLoc <- data.frame(
        C1 = paste0("L", i),
        C2 = input$locations,
        C3 = "",
        stringsAsFactors = FALSE
      )
    }
    if (is.data.frame(input$data)) {
      dfHeader <- data.frame(
        C1 = paste0("X", i),
        C2 = "LOI",
        C3 = ifelse(input$interpolated, "R", "E")
      )
      dfData <- cbind(rep(paste0("X", i), nrow(input$data)), input$data)
      names(dfData) <- names(df)
      dfData <- rbind(dfHeader, dfData)
    } else {
      dfData <- data.frame(C1 = paste0("X", i), C2 = input$data, C3 = "")
    }
    df = rbind(df, dfLoc, dfData)
  }
  write.table(
    df,
    file = sParFileName,
    col.names = F,
    row.names = F,
    sep = "\t",
    quote = F
  )
}

#' Create a SIC input for a PAR file
#'
#' @param x either a fixed input, a [numeric] vector of time in seconds,
#'          a [POSIXt] vector of time, a [matrix] or a [data.frame] with 2 columns (time and value)
#' @param values a [numeric] vector used if `x` is a vector of [numeric] or [POSIXt]
#' @param start a [POSIXt] indicating the start time to use as time zero in the simulation
#' @param locations input locations created with [SicLocation] or [SicLocations]
#' @param interpolated Interpolation mode `TRUE` for "ramp" mode and `FALSE` for "step" mode
#' @param ... used for S3 method compatibility
#'
#' @return A *SicInput* object which is a list with the following items:
#'
#' - `locations`: a [SicLocations] object
#' - `data`: [numeric], the fixed value, or [data.frame], the time series to apply to the locations
#' - `interpolated`: [logical], interpolation mode
#'
#' @rdname SicInput
#' @family SicInput
#' @export
#'
#' @examples
#' # How to impose 5 m3/s in the upstream offtake of the model?
#' # Define location of the boundary condition to set
#' locations <- SicLocations(list(Nd = 1, Pr = 1, car = "Q"))
#' # Define its value
#' sicInputs <- SicInput(5, locations = locations)
#'
#' # How to impose an hydrograph at the upstream offtake of the model?
#' sicInputs <- SicInput(data.frame(t = c(0, 3600, 7200), # time in seconds
#'                                 v = c(5, 20, 5)),     # flows
#'                       locations = locations)
SicInput <- function(x, ...) {
  UseMethod("SicInput", x)
}

#' @rdname SicInput
#' @export
SicInput.numeric <- function(
  x,
  values = NULL,
  locations,
  interpolated = TRUE,
  ...
) {
  if (
    !(inherits(locations, "SicLocation") || inherits(locations, "SicLocations"))
  ) {
    stop("`locations` should be of class 'SicLocation' or 'SicLocations'")
  }
  if (is.null(values)) {
    if (length(x) != 1) stop("For a single value `x` should be of length 1")
    data <- x
  } else {
    if (length(x) != length(values))
      stop("Lenghts of `x` and `values`should be equal.")
    data <- data.frame(t = x, v = values)
  }
  sicInput <- list(
    locations = locations,
    data = data,
    interpolated = interpolated
  )
  class(sicInput) <- c("SicInput", class(sicInput))
  return(sicInput)
}

#' @rdname SicInput
#' @export
SicInput.POSIXt <- function(x, values, start = NULL, ...) {
  if (is.null(start)) start = x[1]
  if (!inherits(start, "POSIXt")) stop("`start` should be of class 'POSIXt'.")

  # Conversion in seconds
  x <- as.numeric(difftime(x, start, units = "secs"))

  if (any(x < 0))
    stop(
      "Negative time detected. Check the start time or the time series order."
    )

  SicInput(x, values, ...)
}

#' @rdname SicInput
#' @export
SicInput.data.frame <- function(x, ...) {
  SicInput(x[, 1], x[, 2], ...)
}

#' @rdname SicInput
#' @export
SicInput.matrix <- function(x, ...) {
  SicInput(x[, 1], x[, 2], ...)
}

#' Merge SicInput objects into a list
#'
#' @param x a [SicInput] object to merge
#' @param y a [SicInput] object to merge
#' @param ... other [SicInput] objects to merge
#'
#' @details The list of parameter is compliant with the `merge` S3 method
#' available in R, so this method can either be called by typing
#' `merge` or `merge.SicInput`.
#'
#' @return A `SicInputs` object which is a list of [SicInput]
#' @export
#'
#' @examples
#' # How to impose 5 m3/s in the upstream offtake of the model?
#' # Define location of the boundary condition to set
#' locations <- SicLocation(list(Nd = 1, Pr = 1, car = "Q"))
#' # Define its value
#' sicInputUpstream <- SicInput(5, locations = locations)
#'
#' # Opening a gate at 0.5 m
#' sicInputGate <- SicInput(
#'   0.5,
#'   locations = SicLocations(list(Bf = 3, Sn = 2, Ouv = 1, Car = "Ouverture"))
#' )
#'
#' # Merging all inputs
#' sicInputs <- merge(sicInputUpstream, sicInputGate)
#'
merge.SicInput <- function(x, y = NULL, ...) {
  sicInputs <- list(x, y, ...)
  sicInputs[sapply(sicInputs, is.null)] <- NULL
  class(sicInputs) <- c("SicInputs", class(sicInputs))
  return(sicInputs)
}

#' Set locations of a SIC model input
#'
#' Do the same as [SicLocation] for eventually several locations
#'
#' @param ... One or several [list] describing a location (See [SicLocation])
#'
#' @return a *SicLocations* object which is a [list] of [SicLocation].
#' @family SicInput
#' @export
#'
#' @examples
#' # Applying the same flow to offtakes located in nodes number 1 to 10
#' locations <- lapply(seq(10), function(i) { list(Nd = i, Pr = 1, Car = "Q")})
#' locations <- do.call(SicLocations, locations)
#' sicInputOfftakes <- SicInput(-0.5, locations = locations)
SicLocations <- function(...) {
  locations <- list(...)
  if (length(locations) == 1) locations <- locations[[1]]
  # Handle a single location in the parameters
  if (length(locations[[1]]) == 1) locations <- list(locations)
  if (!is.list(locations)) stop("`locations` must be a list")
  l <- sapply(locations, SicLocation)
  class(l) <- c("SicLocations", class(l))
  return(l)
}

#' Set a location of a SIC model input
#'
#' @param location a [list] containing the location keys (see details)
#'
#' @return a *SicLocation* object which is a [character] string in the same format as the locations described in the sic documentation of PAR files: \url{https://sic.g-eau.fr/Format-of-the-par-file}.
#'
#' @family SicInput
#' @export
#'
#' @inherit SicInput return examples
#'
SicLocation <- function(location) {
  names(location) <- toupper(names(location))
  # Checks
  availLoc <- c("BF", "ND", "PR", "ST", "OUV", "SN", "PBF", "CAR")
  availCar <- c(
    "Q",
    "Z",
    "KMin",
    "KMoy",
    "Inf",
    "CoteRadier",
    "Largeur",
    "Ouverture",
    "CoefQR",
    "SurverseHauteur",
    "CoefQSurverse",
    "TanAl",
    "CoefQT",
    "CoteAxe",
    "Rayon",
    "D",
    "JMax",
    "S1S2",
    "Decal",
    "Decrement",
    "CoteAmont"
  )
  if (!is.list(location)) stop("Each `location` must be a list")
  if (!"CAR" %in% names(location))
    stop("Each location should have at least an item 'CAR'")
  if (!any(names(location) %in% c("BF", "ND")))
    stop("a location should have at least an item 'BF' or 'ND'")
  dfIncompat <- expand.grid(c("BF", "SN", "PBF"), c("ND", "PR", "ST"))
  lapply(seq_len(nrow(dfIncompat)), function(i) {
    if (all(unlist(dfIncompat[i, ]) %in% names(location)))
      stop(
        "These items can't be together in a location: ",
        paste(paste0("'", unlist(dfIncompat[i, ]), "'"), collapse = ", ")
      )
  })
  if (any(names(location) %in% c("SN", "PBF")) & !"BF" %in% names(location))
    stop("Location with 'SN' or 'PBF' item should have an item 'BF'")
  # Create location string
  l <- lapply(names(location), function(objType) {
    if (!objType %in% availLoc) stop("Unknown location type: ", objType)
    if (objType == "CAR" && !location[[objType]] %in% availCar)
      stop(
        "Value '",
        location[[objType]],
        "' of item 'CAR' unsupported. It should be one of: ",
        paste(paste0("'", availCar, "'"), sep = ", ")
      )
    if (objType != "CAR" && !is.numeric(location[[objType]]))
      stop("Item '", objType, "' should be numeric.")
    return(paste(objType, location[[objType]], sep = "="))
  })
  s <- paste(unlist(l), collapse = "\t")
  class(s) <- c("SicLocation", class(s))
  return(s)
}
