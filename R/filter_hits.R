#' placeholder
#'
#' @param data Input data frame.
#' @param min_hits_enrichment Threshold value for data_stats$Case.Hits.
#' @param min_hits_rcp Threshold value for data_stats$RCP.Hits.Case
#' @param pval_thresh Threshold Value for PValue.Min
#' @param case_over_control Logical describing whether or not to only keep hits
#' for which hit frequency in cases is higher than hit frequency in controls.
#'
#' @export


filter_hits <- function(data, min_hits_enrichment, min_hits_rcp, pval_thresh,
                        case_over_control = TRUE){


  data <- data %>%
    subset(Case.Hits >= min_hits_enrichment) %>%
    subset(RCP.Hits.Case >= min_hits_rcp) %>%
    subset(PValue.Min <= pval_thresh)

  if(case_over_control){
    data <- data %>%
      subset(Case.Hit.Freq > Ctrl.Hit.Freq)
  }

  return(data)
}