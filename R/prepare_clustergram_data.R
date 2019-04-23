#' Process hit data for hierarchical clustering.
#'
#' @param case_rcp RCP data from RCPGenerator or compute_rcp_list.
#' @param hit_list Subset candidate data from filter_hits or filter_hits_list.
#' @param binarize_clustergram Logical controlling whether or not clustergram will be binarized above/below rcp_thresh.
#' @param rcp_thresh Threshold value to binarize against if binarize_clustergram == TRUE.
#' @param sample_field_delimiter Field delimiter in sample names (column names of case_rcp.)
#' @param sample_key_field Which field(s) to retrain from sample names when separated by sample_field delimiter.
#'
#'
#'
#' @export


prepare_clustergram_data <- function(
  case_rcp, hit_list, binarize_clustergram = TRUE, rcp_thresh = 0.95,
  sample_field_delimiter = ".", sample_key_field = 4){

  # Subset Case RCP data
  rcp_rbind <- dplyr::bind_rows(case_rcp, .id = "Data.Type")
  names(rcp_rbind)[2] <- "ID"

  rcp_subset <- rcp_rbind[rcp_rbind$ID %in% hit_list$id, ]
  rcp_subset <- rcp_subset[paste0(rcp_subset$Data.Type, rcp_subset$ID) %in%
                             paste0(hit_list$Data.Type, hit_list$id),]

  # first column Data.Type (zscore/polycl etc).
  # second column V1 pepID
  # subsequent coulumns are RCP info

  # Binarize above&below threshold if specified
  if(binarize_clustergram){
    rcp_subset[,-c(1:2)] <- ifelse(rcp_subset[,-c(1:2)] > rcp_thresh, 1, 0)
  }

  # Update rownames as Protein|Position
  for(i in 1:nrow(rcp_subset)){
    this_annot <- hit_list[hit_list$Data.Type == rcp_subset$Data.Type[i] &
                             hit_list$id == rcp_subset$ID[i], ]

    if(this_annot$Peptide != "NA"){
      rownames(rcp_subset)[i] <- paste(this_annot$Protein, this_annot$Peptide,
                                       sep = "|")
    } else{
      rownames(rcp_subset)[i] <- paste(this_annot$Protein, this_annot$Data.Type,
                                       sep = "|")
    }
  }
  # row match id and Data.Type
  # if peptide, take Protein|Tile
  # if protein (if Peptide is "NA"), take Protein|Data.Type
  # add fix for redundant if needed?
  # then remote rcp_subset cols 1 and 2
  # temporary fix duplicate names?

  # update colnames as sample_key_field after splitting by sample_field_delimiter
  colnames(rcp_subset) <- lapply(colnames(rcp_subset),function(x){
    strsplit(x, sample_field_delimiter, fixed = TRUE) %>% unlist %>%
      extract(sample_key_field)})


  # Remove old label rows and compute heatmap
  heatmap_data <- rcp_subset[, -c(1:2)] %>% as.matrix



  return(heatmap_data)

}