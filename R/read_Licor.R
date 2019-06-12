read_Licor <-
  function(
    Licor_fn
  ) {


  # Read Licor file header

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

  # Determine Licor model and version

  # Read appropriate version





  return(Licor)
}

