#' calculate statistics for list of data
#'
#' @param case Case data list.
#' @param ctrl Control data list.
#' @param case_rcp Case RCP data list.
#' @param ctrl_rcp Control RCP data list.
#' @param enrichment_thresh Enrichment hit threshold.
#' @param rcp_thresh RCP hit threshold.
#' @param stat_test Statistical test. Currently supports "Fisher"
#' @param pval_correction Multiple hypothesis pvalue correction method. BH default.
#'
#' @export


compute_stats_list <- function(
  case, ctrl, case_rcp, ctrl_rcp,
  enrichment_thresh, rcp_thresh, stat_test = "Fisher", pval_correction = "BH"){

  # Check for input errors.
  if(length(case) != length(ctrl)){
    stop("Error: compute_stats_list: case and control length mismatch.")
  } else if((names(case) == names(ctrl)) %>% mean < 1){
    stop("Error: compute_stats_list: mismatch in case and control element names.")
  } else if((length(case) != length(case_rcp)) | (length(ctrl) != length(ctrl_rcp))){
    stop("Error: compute_stats_list: mismatch in data and rcp list length.")
  }

  # Continue
  output_data <- list()

  for(i in 1:length(case)){

    output_data[[i]] <- StatsGenerator(
      case[[i]], ctrl[[i]], case_rcp[[i]], ctrl_rcp[[i]],
      enrichment_thresh, rcp_thresh, stat_test, pval_correction)

    names(output_data)[i] <- names(case)[i]
  }

  return(output_data)
}
