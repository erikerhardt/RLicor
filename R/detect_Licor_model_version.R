#' Detect the Licor model version
#'
#' @param Licor_fn Licor filename to read
#' @param n_header_rows_min Number of lines to read, should be more than the number of header lines
#'
#' @return Licor_model includes \code{model} number or "Not Detected"
#' @export
#'
detect_Licor_model_version <-
  function(
    Licor_fn
  , n_header_rows_min = 100
  ) {

  ## DEBUG
  # path <- "C:/Dropbox/StatAcumen/consult/Authorship/2009_DavidHanson_Isotopes/R-package/RLicor/data-raw"
  # setwd(path)
  # Licor_fn <- "data_Licor_6400_6-1-3.txt"
  # Licor_fn <- "data_Licor_6400_6-1-4.txt"
  # Licor_fn <- "data_Licor_6800_.txt"

  # list can hold more options later, if helpful
  Licor_model <-
    list()

  # Read Licor file header
  message("Reading first ", n_header_rows_min, " lines of Licor file: ", Licor_fn)

  Licor_head <-
    base::readLines(
      Licor_fn
    , n = n_header_rows_min
    )

  # Detect Licor 6800
  # if (grepl("[Header]", Licor_head, fixed = TRUE)) {
  if (any(grepl("Chamber type\t6800", Licor_head, fixed = TRUE))) {
    Licor_model$model   <- "6800"
    return(Licor_model)
  }

  # Detect Licor 6400
  if (any(grepl("<li6400>", Licor_head, fixed = TRUE))) {
    Licor_model$model   <- "6400"
    return(Licor_model)
  }

  # Otherwise, not detected
  Licor_model$model   <- "Not Detected"
  return(Licor_model)
}

