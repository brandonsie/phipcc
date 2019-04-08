#' filter hits for data list.
#'
#' @param data_stats Input data list.
#' @param min_hits_enrichment Threshold value for data_stats$Case.Hits.
#' @param min_hits_rcp Threshold value for data_stats$RCP.Hits.Case
#' @param pval_thresh Threshold Value for Fisher.PValue.Min
#'
#' @export


filter_hits_list <- function(
  data_stats, min_hits_enrichment, min_hits_rcp, pval_thresh
){

  output_data <- list()

  for(i in 1:length(data_stats)){
    output_data[[i]] <- filter_hits(
      data_stats[[i]], min_hits_enrichment, min_hits_rcp, pval_thresh)
  }

  return(output_data)
}