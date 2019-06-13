read_Licor <-
  function(
    Licor_fn
  , sw_model   = c("autodetect", 6800, 6400)
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

  # Determine Licor model and version
  if (!(sw_model %in% c("autodetect", 6800, 6400))) {
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

    Licor <- read_Licor_6800(Licor_fn)

  }

  if (Licor_model$model == 6400) {

    Licor <- read_Licor_6400(Licor_fn)

  }

  return(Licor)
}

