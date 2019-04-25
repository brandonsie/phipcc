#' filter hits for data list.
#'
#' @param data_stats Input data list.
#' @param min_hits_enrichment Threshold value for data_stats$Case.Hits.
#' @param min_hits_rcp Threshold value for data_stats$RCP.Hits.Case
#' @param pval_thresh Threshold Value for Fisher.PValue.Min
#' @param case_over_control Logical describing whether or not to only keep hits
#' for which hit frequency in cases is higher than hit frequency in controls.
#' @param bind_rows Logical determining wiether dplyr::bind_rows is called at the end.
#'
#' @export


filter_hits_list <- function(
  data_stats, min_hits_enrichment, min_hits_rcp, pval_thresh,
  case_over_control = TRUE, bind_rows = FALSE
){

  output_data <- list()

  for(i in 1:length(data_stats)){
    output_data[[i]] <- filter_hits(
      data_stats[[i]], min_hits_enrichment, min_hits_rcp, pval_thresh, case_over_control)

    if(nrow(output_data[[i]]) > 0){
      output_data[[i]]$Data.Type <- names(data_stats)[i]
    }

    names(output_data)[i] <- names(data_stats)[i]


  }

  # #remove empty data types
  # rows_per_type <- sapply(output_data, nrow)
  # output_data <- output_data[!(rows_per_type==0)]


  if(bind_rows) output_data <- dplyr::bind_rows(output_data)


  return(output_data)
}