#' subset data to only contain rows that match a template.
#'
#' @param data List of larger data frames to subset.
#' @param template list of smaller data frames to subset against.
#'
#' @export


subset_data <- function(data, template){

  output_data <- list()

  for(i in 1:length(data)){
    this_data <- data[[i]]
    this_temp <- template[[i]]

    output_data[[i]] <- this_data[match(this_temp[,1], this_data[,1]),]
    names(output_data)[i] <- names(data)[i]
  }


  return(output_data)


}