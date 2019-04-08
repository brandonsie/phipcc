#' placeholder
#'
#' @param data Input data frame.
#' @param min_hits_enrichment Threshold value for data_stats$Case.Hits.
#' @param min_hits_rcp Threshold value for data_stats$RCP.Hits.Case
#' @param pval_thresh Threshold Value for PValue.Min
#'
#' @export


filter_hits <- function(data, min_hits_enrichment, min_hits_rcp, pval_thresh){


  data <- data %>%
    subset(Case.Hits >= min_hits_enrichment) %>%
    subset(RCP.Hits.Case >= min_hits_rcp) %>%
    subset(PValue.Min <= pval_thresh)

  return(data)
}