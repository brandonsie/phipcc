#' candidate antigen generator script
#'
#' Further process annotated_data target by collapsing peptide data to protein data in a more reader-digestible format.
#'
#' @param data annotated_data target.
#'
#' @export


prepare_candidate_table <- function(data){

  # Extract relevant columns from input data

  data_subset <- data[, c("id", "RCP.Hit.Freq.Case", "RCP.Hit.Freq.Ctrl",
                          "Median.Hit.Score.Case", "Median.Hit.Score.Ctrl",
                          "PValue.Min", "Data.Type", "Protein", "Peptide",
                          "Description", "Flags")]
  data_subset <- data_subset[order(data_subset$PValue.Min),]

  # Collapse based on Protein column
  # Row_ID becomes <Protein> | <Peptide_1>, <Peptide_2> {na omitted}
  # Hit_Categories becomes comma-sep list of uniques from Data.Type
  # Row_PValue becomes minimum PValue.Min for that protein


  all_proteins <- unique(data_subset$Protein)
  output_data <- data.frame(matrix(nrow = length(all_proteins), ncol = 9))
  colnames(output_data) <- c("Protein", "Description",
                             "Annotations", "Data Types",
                             "Case Hit Frequency", "Ctrl Hit Frequency",
                             "Median Case Hit Score", "Median Ctrl Hit Score",
                             "P Value")

  for(i in 1:length(all_proteins)){

    this_protein <- all_proteins[i]
    this_data <- data_subset[data_subset$Protein == this_protein,]

    # Protein Column
    this_peptides <- paste(this_data$Peptide[!grepl("NA", this_data$Peptide)],
                           collapse = ", ")
    output_data$Protein[i] <- ifelse(
      this_peptides == "", this_protein,
      paste(this_protein, this_peptides, sep = " | "))

    # Description, Annotations, Data Types Columns
    output_data$Description[i] <- unique(this_data$Description)
    output_data$Annotations[i] <- paste(
      this_data$Flags[this_data$Flags != "NA"] %>% unique, collapse = ", "
    )
    output_data$`Data Types`[i] <- paste(unique(this_data$`Data.Type`),
                                         collapse = ", ")


    # P Value column. (add from which peptide/promax/polycl in paranthetical)
    min_pval <- min(this_data$PValue.Min)


    min_pval_info <- this_data[this_data$PValue.Min == min_pval,
                               c("Data.Type")]
    # # use below instead if want to append tile info
    # min_pval_info <- this_data[this_data$PValue.Min == min_pval,
    #                            c("Data.Type", "Peptide")]
    # min_pval_info <- ifelse(min_pval_info$Peptide == "NA",
    #                         min_pval_info$Data.Type,
    #                         paste(min_pval_info, collapse = " | "))
    output_data$`P Value`[i] <- paste0(
      formatC(min_pval,format="e",digits=1), " (", min_pval_info ,")")

    # Hit Frequency, Hit Score Columns. Take from the lowest PValue. round.
    min_pval_data <- this_data[this_data$PValue.Min == min_pval,
                               c("RCP.Hit.Freq.Case", "RCP.Hit.Freq.Ctrl",
                                 "Median.Hit.Score.Case", "Median.Hit.Score.Ctrl")]
    output_data$`Case Hit Frequency`[i] <- round(min_pval_data[1,1], 2)
    output_data$`Ctrl Hit Frequency`[i] <- round(min_pval_data[1,2], 2)
    output_data$`Median Case Hit Score`[i] <- round(min_pval_data[1,3], 1)
    output_data$`Median Ctrl Hit Score`[i] <- round(min_pval_data[1,4], 1)


  }

  #(!) do html separately so can save csv of normal table

  return(output_data)

}