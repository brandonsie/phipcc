#' prepare_AVARDA_clustergram_data
#'
#' @export


prepare_AVARDA_clustergram_data <- function(
  AVARDA_case_data, AVARDA_case_breadth_rcp, AVARDA_filtered, pval_thresh = 0.05, rcp_thresh = 0.95

){

  AVARDA_case_seropos <- AVARDA_case_data$seropos
  AVARDA_case_seropos[, -1] <- ifelse(AVARDA_case_seropos[,-1] <= pval_thresh, 1, 0)

  AVARDA_case_breadth_rcp_hits <- AVARDA_case_breadth_rcp
  AVARDA_case_breadth_rcp_hits[, -1] <- ifelse(AVARDA_case_breadth_rcp_hits[, -1] >= rcp_thresh, 1, 0)

  # take only statistically significant hits
  AVARDA_case_seropos_subset <- AVARDA_case_seropos[match(AVARDA_filtered$Virus[AVARDA_filtered$Seropos.Fisher.PVal < pval_thresh], AVARDA_case_seropos$Virus),]
  AVARDA_case_breadth_rcp_hits_subset <- AVARDA_case_breadth_rcp_hits[match(AVARDA_filtered$Virus[AVARDA_filtered$Breadth.Fisher.PVal < pval_thresh], AVARDA_case_breadth_rcp_hits$Virus),]




  # update rownames to be Virus|Seropos or Virus|Breadth
  AVARDA_case_seropos_subset$Virus <- paste(AVARDA_case_seropos_subset$Virus, "seropos", sep = "|")
  AVARDA_case_breadth_rcp_hits_subset$Virus <- paste(AVARDA_case_breadth_rcp_hits_subset$Virus, "breadth", sep = "|")

  # rbind
  AVARDA_clust_rbind <- dplyr::bind_rows(AVARDA_case_seropos_subset, AVARDA_case_breadth_rcp_hits_subset)
  colnames(AVARDA_clust_rbind)[1] <- "ID"

  return(AVARDA_clust_rbind)
}