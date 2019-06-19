#' Call AnnotationGenerator on a list of data
#'
#' @param data Named list of data.
#' @param data_level Character vector of equal length to data, indicating whether each data type is "peptide" or "protein" -level.
#'
#' @export


annotate_cc_list <- function(data, data_level, ...
){

  output_data <- list()

  for(i in 1:length(data)){
    output_data[[i]] <- AnnotationGenerator(
      data[[i]], data_level[i], names(data)[i], ...)
  }

  names(output_data) <- names(data)
  return(output_data)

}