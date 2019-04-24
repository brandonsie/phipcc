#' Call AnnotationGenerator on a list of data
#'
#' @param data Named list of data.
#' @param data_level Character vector of equal length to data, indicating whether each data type is "peptide" or "protein" -level.
#' @param data_types Character vector describing types of data stored in data list. e.g. "zscore", "polyclonal".
#'
#' @export


annotate_cc_list <- function(data, data_level, data_types, ...
){

  output_data <- list()

  for(i in 1:length(data)){
    output_data[[i]] <- AnnotationGenerator(data[[i]], data_level[i], data_types[i], ...)
  }

  names(output_data) <- names(data)
  return(output_data)

}