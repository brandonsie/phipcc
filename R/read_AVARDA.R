#' read_AVARDA
#'
#'
#' @export

read_AVARDA <- function(
  AVARDA_paths, AVARDA_seropos_grep = "full_BH_p_value",
  AVARDA_breadth_grep = "full_filtered_evidence_number",
  remove_leading_x = FALSE
){

  # Identify paths to relevant files
  seropos_paths <- vector("character")
  breadth_paths <- vector("character")
  for(i in 1:length(AVARDA_paths)){

    AVARDA_files <- list.files(AVARDA_paths[i], full.names = TRUE)

    seropos_paths %<>% c(AVARDA_files[grep(AVARDA_seropos_grep, AVARDA_files)])
    breadth_paths %<>% c(AVARDA_files[grep(AVARDA_breadth_grep, AVARDA_files)])

  }

  # Read and Aggregate Seropos Data
  seropos_data <- list()
  for(i in 1:length(seropos_paths)){

    this_data <- data.table::fread(seropos_paths[i], data.table = FALSE)
    this_data <- replace(this_data, is.na(this_data), 1)
    this_data <- this_data[order(this_data$Virus),]

    seropos_data[[i]] <- this_data
  }
  seropos_data <- dplyr::bind_cols(seropos_data)

    # Read and Aggregate Breadth Data
  breadth_data <- list()
  for(i in 1:length(breadth_paths)){
    this_data <- data.table::fread(breadth_paths[i], data.table = FALSE)
    this_data <- replace(this_data, is.na(this_data), 0)
    this_data <- this_data[order(this_data$Virus),]

    breadth_data[[i]] <- this_data

  }
  breadth_data <- dplyr::bind_cols(breadth_data)


  # remove leading X in filenames if remove_leading_x == TRUE
  if(remove_leading_x == TRUE){
    names(seropos_data) <- gsub("^x", "", names(seropos_data), ignore.case = TRUE)
    names(breadth_data) <- gsub("^x", "", names(breadth_data), ignore.case = TRUE)
  }

  # Prepare Output List
  output_data <- list(seropos = seropos_data, breadth = breadth_data)
  return(output_data)

}

