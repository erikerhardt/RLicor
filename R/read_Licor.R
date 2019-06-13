#' Read Licor 6400 and 6800 files
#'
#' @param Licor_fn Licor file to read
#' @param sw_model Model of Licor or autodetect
#' @param n_header_rows_min Minimum number of rows to read in order to detect where header ends and data begins
#'
#' @return Licor is a list including the header and data
#' @export
#'
read_Licor <-
  function(
    Licor_fn
  , sw_model   = c("autodetect", 6800, 6400)[1]
  , n_header_rows_min = 100
  ) {

  ## DEBUG
  # path <- "C:/Dropbox/StatAcumen/consult/Authorship/2009_DavidHanson_Isotopes/package_testing/RLicor/data-raw"
  # setwd(path)
  # Licor_fn <- "data_Licor_6400_6-1-3.txt"
  # Licor_fn <- "data_Licor_6400_6-1-4.txt"
  # Licor_fn <- "data_Licor_6800_.txt"
  # dat_Licor <- read_Licor(Licor_fn)
  # str(dat_Licor)

  # list can hold more options later, if helpful
  Licor_model <-
    list()

  # Determine Licor model and version
  if (!any(sw_model %in% c("autodetect", 6800, 6400))) {
    warning("Licor sw_model argument not set correctly, defaulting to \"autodetect\".")
    sw_model <- "autodetect"
  }
  if (sw_model == 6800) {
    Licor_model$model <- 6800
  }
  if (sw_model == 6400) {
    Licor_model$model <- 6400
  }
  if (sw_model == "autodetect") {
    Licor_model <- detect_Licor_model_version(Licor_fn, n_header_rows_min)
  }

  if (Licor_model$model == "Not Detected") {
    stop("Licor model (6800 or 6400) could not be automatically detected from file header.")
  }


  # Read appropriate version

  if (Licor_model$model == 6800) {
    message("Licor 6800 model")
    Licor <- read_Licor_6800(Licor_fn)
  }

  if (Licor_model$model == 6400) {
    message("Licor 6400 model")
    Licor <- read_Licor_6400(Licor_fn)
  }

  return(Licor)
}

