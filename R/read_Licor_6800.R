#' read Licor 6800 file
#'
#' Read Licor file
#' Find the row where the column headers and data begin.
#' Separate any remarks into its own column.
#'
#' @param Licor_fn Licor filename to read
#'
#' @return Licor A list including a \code{header} list and \code{data} tibble
#' @importFrom magrittr %>%
#' @importFrom readr read_file
#' @importFrom stringr str_split str_replace_na
#' @importFrom purrr map flatten_chr safely
#' @importFrom dplyr bind_rows mutate select everything
#'
#' @export
#'
read_Licor_6800 <-
  function (Licor_fn) {

  # header and data, separated
  Licor <- list()

  # concepts from http://www.ericrscott.com/2018/01/17/li-cor-wrangling/

  # read data in as text to remove header and remark rows from column headers and data
  Licor_raw <- readr::read_file(Licor_fn)

  header_pattern  <- "[Header]"
  data_pattern    <- "[Data]"

  # splits into individual bouts
  Licor_bouts <- stringr::str_split(Licor_raw, stringr::fixed(header_pattern), simplify = TRUE)

  # splits further to separate headers from actual data
  Licor_header_data <- stringr::str_split(Licor_bouts, stringr::fixed(data_pattern), simplify = FALSE)

  # separate header and data, remove empty elements (first is always empty)
  Licor$header <-
    Licor_header_data %>%
    purrr::map(`[`, 1) %>% #equivalent to doing raw_split2[[i]][2] for every element "i"
    purrr::flatten_chr() #converts to a vector
  Licor$header <-
    Licor$header[!(Licor$header == "")]

  dat_temp <-
    Licor_header_data %>%
    purrr::map(`[`, 2) %>% # equivalent to doing Licor_header_data[[i]][2] for every element "i"
    purrr::flatten_chr() # converts to a vector
  dat_temp <-
    dat_temp[!is.na(dat_temp)]

  # Because the header has 3 lines
  #
  read_tsv_skip_before_after_header <- function(dat, skip_pre = 2, skip_post = 1) {
    # if header is on row 2 and data starts on row 4,
    # then skip_pre = 1 and skip_post = 1
    #source_names  <- names(readr::read_tsv(dat, skip = skip_pre - 1, n_max = 0))
    col_names     <- names(readr::read_tsv(dat, skip = skip_pre    , n_max = 0))
    #unit_names    <- names(readr::read_tsv(dat, skip = skip_pre + 1, n_max = 0))
    dat2 <- readr::read_tsv(dat, skip = skip_pre + 1 + skip_post, col_names = col_names)
    return(dat2)
  }

  dat_temp2 <-
    dat_temp %>%
    purrr::map(read_tsv_skip_before_after_header) %>%
    dplyr::bind_rows()

  # create a "safe" version of as.integer() that returns a list of a result and error
  # returns error for text remarks, returns value for integer observation numbers
  safe_as.int <-
    purrr::safely(as.integer)

  dat_temp3 <-
    dat_temp2 %>%
    dplyr::mutate(
      # create a comment column to indicate if an "obs" is actually a remark
      comment = is.na(safe_as.int(obs)$result)
      # copy those remarks to the remark column
    , remark = ifelse(comment == TRUE, obs, NA)
      # remove remarks from obs column
    , obs = ifelse(comment == FALSE, obs, NA)
    ) %>%
    # move the remark column the the begining
    dplyr::select(
      remark
    , dplyr::everything()
    ) %>%
    # remove the temporary comment column.
    dplyr::select(-comment) %>%
    # replace NA with the literal string "NA" so str_* functions from stringr can deal with it
    dplyr::mutate(
      remark = stringr::str_replace_na(remark)
    )

  Licor$data <-
    dat_temp3

  return(Licor)
}
