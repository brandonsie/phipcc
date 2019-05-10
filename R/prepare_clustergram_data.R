#' Process hit data for hierarchical clustering.
#'
#' @param case_rcp RCP data from RCPGenerator or compute_rcp_list.
#' @param hit_list Subset candidate data from filter_hits or filter_hits_list.
#' @param binarize_clustergram Logical controlling whether or not clustergram will be binarized above/below rcp_thresh.
#' @param rcp_thresh Threshold value to binarize against if binarize_clustergram == TRUE.
#' @param sample_field_delimiter Field delimiter in sample names (column names of case_rcp.)
#' @param sample_key_field Which field(s) to retrain from sample names when separated by sample_field delimiter.
#' @param AVARDA_RCP Optional AVARDA breadth case RCP data to add to clustergram.
#'
#'
#' @export


prepare_clustergram_data <- function(
  case_rcp, hit_list, binarize_clustergram = TRUE, rcp_thresh = 0.95,
  sample_field_delimiter = ".", sample_key_field = 4, AVARDA_RCP = NA){

  # Subset Case RCP data
  for(i in 1:length(case_rcp)){
    colnames(case_rcp[[i]])[1] <- "ID"
  }

  # if(!is.na(AVARDA_RCP)[1]){
  #   next_pos <- length(case_rcp) + 1
  #   case_rcp[[next_pos]] <- AVARDA_RCP
  #   names(case_rcp)[next_pos] <- "AVARDA_Breadth"
  # }

  rcp_rbind <- dplyr::bind_rows(case_rcp, .id = "Data.Type")
  # names(rcp_rbind)[2] <- "ID"

  rcp_subset <- rcp_rbind[rcp_rbind$ID %in% hit_list$id, ]
  rcp_subset <- rcp_subset[paste0(rcp_subset$Data.Type, rcp_subset$ID) %in%
                             paste0(hit_list$Data.Type, hit_list$id),]

  if(!is.na(AVARDA_RCP)[1]){
    AVARDA_RCP$Data.Type <- ""
    rcp_subset %<>% rbind(AVARDA_RCP)
  }



  # first column Data.Type (zscore/polycl etc).
  # second column V1 pepID
  # subsequent coulumns are RCP info

  # Binarize above&below threshold if specified
  if(binarize_clustergram){
    rcp_subset[,!(colnames(rcp_subset) %in% c("Data.Type", "ID"))] <-
      ifelse(rcp_subset[,!(colnames(rcp_subset) %in% c("Data.Type", "ID"))] >
               rcp_thresh, 1, 0)
  }

  # Update rownames as Protein|Position
  for(i in 1:nrow(rcp_subset)){
    this_annot <- hit_list[hit_list$Data.Type == rcp_subset$Data.Type[i] &
                             hit_list$id == rcp_subset$ID[i], ]

    #new way
    if(nrow(this_annot) == 0){
      this_rowname <- rcp_subset$ID[i]

    } else {
      this_rowname <- this_annot$ProteinPeptide

    }

    # if(this_annot$Peptide != "NA"){
    #   this_rowname <- paste(this_annot$Protein, this_annot$Peptide, sep = "|")
    # } else{
    #   this_rowname <- paste(this_annot$Protein, this_annot$Data.Type, sep = "|")
    # }

    # if(this_rowname %in% rownames(rcp_subset)){
    #   #Fix for redundant protein names
    #   base_rowname <- this_rowname
    #   k = 1
    #   while(this_rowname %in% rownames(rcp_subset)){
    #     this_rowname <- paste(base_rowname, k, sep = "|")
    #   }
    # }

    rownames(rcp_subset)[i] <- this_rowname

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
      extract(sample_key_field)}) %>% unlist


  # Remove old label rows and compute heatmap
  heatmap_data <- rcp_subset[,!(colnames(rcp_subset) %in% c("Data.Type", "ID"))] %>% as.matrix
  rownames(heatmap_data) <- rownames(rcp_subset)


  return(heatmap_data)

}
