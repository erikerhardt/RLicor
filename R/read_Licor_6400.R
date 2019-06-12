#' read Licor file
#'
#' See \code{\link{val_TDL_Licor_variables}} for expected column names.
#'
#' Create \code{Licor} to hold data.
#'
#' Read Licor file
#'
#' Find the row where the column headers and data begin.
#'
#' Look for "Obs" as the first row (used to look for $STARTOFDATA$, but not in every version of Licor file).
#'
#' If any extra lines in Licor file, remove those lines and fix the Obs and HHMMSS columns.
#'
#' Date (day) of run, fix Thursday (Thr to Thu) representation.
#'
#' @param Licor_fn xxxPARAMxxx
#' @param Licor_TDL_time_offset_seconds xxxPARAMxxx
#' @param sw xxxPARAMxxx
#'
#' @return Licor xxxRETURNxxx
#' @importFrom utils read.delim
#'
read_Licor_6400 <-
function# read_Licor file
###
(Licor_fn
###
, Licor_TDL_time_offset_seconds
###
, sw
###
)
{
  ## See \code{\link{val_TDL_Licor_variables}} for expected column names.

  ## Create \code{Licor} to hold data.
  Licor <- list();  # create a list to return with data

  if (sw$use_Licor ) {

    ####################
    ## Read Licor file
    p_o <- paste("               Reading Licor file: ", Licor_fn, "\n"); write_out(p_o);
    ## Find the row where the column headers and data begin.
    Licor_head_nrows = 30;  # more rows than we need to check
    Licor_head <- utils::read.delim(Licor_fn, header=FALSE, sep="\n", nrows=Licor_head_nrows);
    ##details<<
    ## Look for "Obs" as the first row (used to look for $STARTOFDATA$, but not in every version of Licor file).
    #Licor_header_skip = seq(1,Licor_head_nrows)[(Licor_head == "$STARTOFDATA$")];
    for (i_nrows in 1:Licor_head_nrows){
      if (substr(Licor_head[i_nrows,],1,3) == "Obs"){
        Licor_header_skip = i_nrows - 1;
      }
    }
    # 11/22/2010 7:40PM changed
    Licor$data <- read.delim(Licor_fn, header=TRUE, sep="", skip=Licor_header_skip);     # any white space is delim

    ## If any extra lines in Licor file, remove those lines and fix the Obs and HHMMSS columns.
    fix_factor <- is.factor(Licor$data[,1]);
      if(fix_factor){
        p_o <- paste("            Note: Some junk lines in Licor file, removing those lines (may see NA warning)", "\n"); write_out(p_o);
        Licor$data2 <- utils::read.delim(Licor_fn, header=TRUE, sep="", as.is=TRUE, skip=Licor_header_skip);     # any white space is delim, as.is does not convert to factors
        na_data <- is.na(as.numeric(Licor$data2[,1]));
        Licor$data <- Licor$data[!na_data,]; # remove any lines that don't begin with a number -- such as lines: "Const=" -52 "Oxygen%" 2.0
        Licor$data[,"Obs"   ] <- as.numeric(Licor$data2[!na_data,"Obs"   ]); # fix the affected columns
        #Licor$data[,"HHMMSS"] <-            Licor$data2[!na_data,"HHMMSS"] ;
        Licor$data[,"FTime" ] <- as.numeric(Licor$data2[!na_data,"FTime" ]);
        Licor$data2 <- NULL; # remove after fixing
      }

    #Licor$data <- read.delim(Licor_fn, header=TRUE, sep="\t", skip=Licor_header_skip);  # only tabs is delim
    Licor$n <- dim(Licor$data)[1]; # number of observations

    # 7/15/2010 no longer adding column names -- processing a core set of variables and ignoring the rest
    ## check whether need to add columns
    #if ("VpdA" %in% colnames(Licor$data)) {
    #  Licor$sw_additional_col <- 1; # do nothing, the additional columns are in this file
    #} else {
    #  Licor$sw_additional_col <- 0;
    #      p_o <- paste("Licor file - Note: adding NA columns for VpdA .. xTemp2 columns not in this file\n"); wWw <- write_progress(p_o, time_start);
    #  temp_add <- matrix(NA, nrow=Licor$n, ncol=11);
    #  colnames(temp_add) <- c("VpdA","Ci_Ca","Ci_Pa","uc_20_mV","uc_21_mV","X.U.S.","Trans","CndCO2","Ref_mV","xTemp1","xTemp2");
    #  Licor$data <- cbind(Licor$data, temp_add);
    #}

    ## Date (day) of run, fix Thursday (Thr to Thu) representation.
    Licor_date_temp   <- scan(Licor_fn, what="character", skip=1, nlines=1);
      Licor_date_temp <- sub("Thr", "Thu", Licor_date_temp);  # Thursday has an alternate representation in Licor than R
    Licor_date_start  <- strptime(Licor_date_temp, "%a %b %d %Y %H:%M:%S"); #, tz=Sys.timezone());
    Licor_date        <- format(Licor_date_start, "%Y-%m-%d")

    # variables for each column
    Licor_HHMMSS      <- Licor$data[, "HHMMSS"];
    Licor_FTime       <- Licor$data[, "FTime"];

      #Licor_time_no_correction  <- strptime(paste(Licor_date, Licor_HHMMSS), "%Y-%m-%d %H:%M:%OS"); #, tz=Sys.timezone());
      Licor_time_start          <- strptime(paste(Licor_date, Licor_HHMMSS[1]), "%Y-%m-%d %H:%M:%OS"); #, tz=Sys.timezone());
      Licor_time_first_decimal  <- Licor_FTime[1];
      Licor_time_corrected      <- Licor_time_start + as.numeric(Licor_FTime) - as.numeric(Licor_time_first_decimal) + Licor_TDL_time_offset_seconds;
      #seconds_diff_Licor = as.numeric(difftime(Licor_time_corrected,Licor_time_no_correction, tz=Sys.timezone(), units="secs"));
      #seconds_diff_Licor = as.numeric(difftime(Licor_time_corrected[1:(Licor_n-1)],Licor_time_corrected[2:Licor_n], tz=Sys.timezone(), units="secs"));
         # > table(-seconds_diff_Licor)
         #   9.5   10 10.5   11
         #     1   36  136    2

      Licor$time <- Licor_time_corrected;
  } else {
    p_o <- paste("                Not using Licor file", "\n"); write_out(p_o);
    Licor$data <- list();

    Licor$n    <- NA;
    Licor$time <- NA;

  };

  return( Licor );
  ### Licor
}

