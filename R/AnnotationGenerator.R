#' generate case control annotations for hit data based on library annotation file.
#'
#' @param data Input data frame.
#' @param data_level If data_level is "peptide", then AnnotationGenerator will operate based on the peptide_ parameters. If data_level is "protein", then Annotation_Generator will operate based on the protein_ parameters.
#' @param data_types Character vector describing types of data stored in data list. e.g. "zscore", "polyclonal".
#' @param annot Library annotation file with named columns.
#' @param peptide_col_id_match For data_level=="peptide", the name of a column in annot that corresponds to the first column of data.
#' @param protein_col_id_match For data_level=="protein", the name of a column in annot that corresponds to the first column of data.
#' @param peptide_cold_id_display For data_level=="peptide", the name of a column in annot that corresponds to identifiers to display in the markdown report corresponding to each peptide.
#' @param protein_col_id_display The name of a column in annot that corresponds to identifiers to display in the markdown report corresponding to each protein.
#' @param description_col_id The name of a column in annot that corresponds to identifiers to display in the markdown report corresponding to gene product or similar short protein descriptions to display in the markdown report.
#' @param flag_col_id Character string or vector of column  name(s) corresponding to annotation flags to indicate in the markdown report (e.g. of known autoantigens or surface proteins).
#' @param flag_type type "binary" to detect whether or not there is text in that cell. "literal" to display exact text from cell.
#' @param pep_aa Character string of column name in annot corresponding to peptide amino acid sequence in the markdown report. Needed for peitopefindr.
#'
#' @export


AnnotationGenerator <- function(
  data, data_level, data_types, annot, peptide_col_id_match, protein_col_id_match,
  peptide_col_id_display, protein_col_id_display, description_col_id, flag_col_id, flag_type,
  pep_aa = "pep_aa"
){

  # annot <- data.table::fread(annot_path, data.table = FALSE)

  if(data_level == "peptide"){
    data$Protein <- annot[match(data[,1], annot[,peptide_col_id_match]),
                          protein_col_id_display] %>% as.character
    data$Peptide <- annot[match(data[,1], annot[,peptide_col_id_match]),
                              peptide_col_id_display] %>% as.character


    data$ProteinPeptide <- paste(data$Protein, data$Peptide, sep = "|")

    flags <- data.frame(annot[match(data[,1], annot[,peptide_col_id_match]), flag_col_id])
    names(flags) <- flag_col_id


  } else if(data_level == "protein"){
    data$Protein <- annot[match(data[,1], annot[,protein_col_id_match]),
                          protein_col_id_display] %>% as.character
    data$Peptide <- "NA"

    data$ProteinPeptide <- paste(data$Protein, data_types, sep = "|")

    flags <- data.frame(annot[match(data[,1], annot[,protein_col_id_match]), flag_col_id])
    names(flags) <- flag_col_id


  } else{stop("Invalid data_level passed to AnnotationGenerator.")}

  # Deal with duplicate names

  duplicateProPep <- data$ProteinPeptide[duplicated(data$ProteinPeptide)]
  if(length(duplicateProPep) > 0){
    for(i in 1:length(duplicateProPep)){ #for each duplicate ID
      #count occurances
      dup_occurances <- length(with(data, ProteinPeptide[ProteinPeptide == duplicateProPep[i]]))

      #take rows with matching ID
      #rename sequentailly with suffix ".1", ".2", etc.
      data$ProteinPeptide[data$ProteinPeptide == duplicateProPep[[i]]] <-
        # paste0(with(data, ProteinPeptide[ProteinPeptide == duplicateProPep[i]]), 1:dup_occurances, sep=".")
        paste(duplicateProPep[i], 1:dup_occurances, sep=".")
    }
  }


  # Description

  data$Description <- annot[match(data$Protein, annot[,protein_col_id_display]),
                            description_col_id] %>% gsub("_", " ", .)

  # Prepare Flag annotations
  if(flag_type == "binary"){
    flags_simplified <- flags
    flags_simplified[] <- sapply(1:ncol(flags), function(x){
      temp <- flags[,x]
      temp[temp == "NA" | temp == 0 | temp == "0"] <- ""
      temp[temp != ""] <- names(flags)[x]
      return(temp)
    })
  } else{
    flags_simplified <- flags
  }




  flags_concat <- flags_key <- vector(mode = "character", length = nrow(data))

  for(i in 1:ncol(flags_simplified)){
    flags_concat %<>% paste(flags_simplified[,i], sep = ", ")
  }

  while(grepl(", , ", flags_concat) %>% sum > 0){
    flags_concat <- gsub(", , ", ", ", flags_concat)
  }

  flags_concat <- flags_concat %>% gsub("^, ", "", .) %>% gsub(", $", "", .)
  flags_concat[flags_concat == ""] <- "NA"
  flags_key <- sapply(strsplit(flags_concat, ", "), "[[", 1)

  data$Flags <- flags_concat
  data$Key.Flag <- flags_key


  if(data_level == "peptide"){
    data$pep_aa <- annot[match(data[,1], annot[, peptide_col_id_match]),
                         pep_aa]

  } else{
    data$pep_aa <- NA
  }



  # data <- cbind(data, flags_simplified)

  # add Protein | tile annotations
  #(!) handle promax polycl differentLY? (!) match to different col

  #add flags

  #(!)
  # flags_simplified #convert each cell to its colname if cell isn't empty
  # then generate Annotations (merged) and Annotation (highest priority positive)
  # make sure fully blanks get populated with NA in both S and N.


  # Collapse from peptide level to protein level

  # add Description column

  #(!) do once per protein instead?


  # Add HitCategoreis from $Data.Type

  return(data)
}