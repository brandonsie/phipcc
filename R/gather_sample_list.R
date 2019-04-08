#' Return a list of subsetinput data based on specified data types, file paths,
#' and sample names.
#'
#' @param data_types Character vector, for example "MLXP", "MLXP_promax", "MLXP_polycl"
#' @param input_files Paths to data files from which to extract relevant data. Structured as a list, with each element of the list containing a character vector of paths corresponding to data_type of the same index.
#' @param sample_names Sample names corresponding to relevant column names in data files.
#'
#' @return Named list. Name of each element is corresponding data_type.
#' And value of each element is table of corresponding data.
#'
#' @export


gather_sample_list <- function(data_types, input_files, sample_names){

  output_file <- list()

  for(i in 1:length(data_types)){
    # sub_input_files <- input_files[grep(data_types[i], input_files)]
    sub_input_files <- input_files[[i]]
    output_file[[i]] <- phipmake::gather_samples(sub_input_files, sample_names)
    names(output_file)[i] <- data_types[i]
  }

  return(output_file)
}

