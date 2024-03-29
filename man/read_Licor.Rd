% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_Licor.R
\name{read_Licor}
\alias{read_Licor}
\title{Read Licor 6400 and 6800 files}
\usage{
read_Licor(Licor_fn, sw_model = c("autodetect", 6800, 6400)[1],
  n_header_rows_min = 100)
}
\arguments{
\item{Licor_fn}{Licor file to read}

\item{sw_model}{Model of Licor or autodetect}

\item{n_header_rows_min}{Minimum number of rows to read in order to detect where header ends and data begins}
}
\value{
Licor is a list including the header and data
}
\description{
Read Licor 6400 and 6800 files
}
