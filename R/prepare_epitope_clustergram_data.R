#' Collapse hit clustergram based on cross-reactive peptide groups defined by epitopefindr.
#'
#' @param clustergram Sorted data from generate_clustergram
#' @param epitopeSummary Path to epitopeSummary output file from epitopefindr.
#'
#' @export


prepare_epitope_clustergram_data <- function(
  clustergram, epitopeSummary = "data/epitopefindr/epitopeSummary.csv"){
  # Load Input Data
  clust_ep <- clustergram
  groups <- data.table::fread(epitopeSummary, data.table = FALSE, header = TRUE)
  all_group_row_ind <- vector("numeric") #track of epitope rows to remove after loop

  # Loop through Epitope Groups (and Singletons at the end)
  for(i in 2:ncol(groups)){

    group_id <- colnames(groups)[i]
    group_peptides <- groups$id[groups[,i] != ""]

    clust_pepnames <- vector("character")
    for(j in 1:length(group_peptides)){
      fuzzy_matches <- agrep(group_peptides[j], rownames(clust_ep), value = TRUE)
      fuzzy_dist <- adist(group_peptides[j],fuzzy_matches)
      clust_pepnames %<>% c(fuzzy_matches[which(fuzzy_dist == min(fuzzy_dist))[1]])
    }


    if(group_id != "<NA>"){
      # Normal Group
      # merge data and take mean
      # group_id <- group_id %>% as.numeric

      group_row_ind <- c(1:nrow(clust_ep))[rownames(clust_ep) %in% clust_pepnames]
      all_group_row_ind %<>% c(group_row_ind)
      group_data <- clust_ep[group_row_ind, ]
      collapsed_data <- apply(group_data, 2, mean) %>% as.data.frame %>% t
      rownames(collapsed_data) <- paste0("Epitope_", group_id)

      clust_ep %<>% rbind(collapsed_data)

    } else{# Singletons (retain, don't group together)
    }

  }

  # after loops, remove all peptides that were members of a group
  clust_ep <- clust_ep[-(unique(all_group_row_ind)),]


  return(clust_ep)

}