#' setup configurations for this case control study
#'
#' writes table (defualt name config.tsv) with specified values.
#' For input into define_plan_case_control()
#'
#' @param filename location to write configuration file. Default config.tsv in working direcotyr.
#' @param sep field separator for config.tsv. Tab default. Can be set to comma for csv, etc.
#' @param proj_id Study name to print in report from phipcc::render_from_template().
#' @param library PhIP peptide library identifier to print in report from phipcc::render_from_template().
#' @param ctrl_id Control cohort name to print in report from phipcc::render_from_template().
#' @param delimiter Separator, default comma, separating multi-field parameters below e.g. case_names, data_types.
#' @param case_names data column names corresponding to case samples
#' @param ctrl_names data column names corresponding to control samples
#' @param input_dirs directories that hold case and control data for this library
#' @param data_types grep terms to search for in input_dirs
#' @param data_names terms corresponding to data_types to display in R markdown report
#' @param data_level peptide or protein, corresponding to data_types. Affect annotation.
#' @param min_hits_rcpgenerator Minimum number of hits based on hit_thresh required to compute case_rcp for that peptide or protein.
#' @param hit_thresh Threshold for considering data hit.
#' @param rcp_thresh RCP threshold for statistics and clustergram binarization.
#' @param stat_test =Statistical test to perform (currently only supports Fisher).
#' @param pval_correction Pvalue correction method to be fed to p.adjust.
#' @param min_hits_enrichment Hit subsetting limit for minimum sample hits for inclusion of a peptide/protein
#' @param min_hits_rcp Hit subsetting limit for minimum sample RCP hits for inclusion of a peptide/protein.
#' @param min_freq_hits_rcp Hit subsetting limit for minimum frequency of sample RCP hits for inclusion of a peptide/protein.
#' @param pval_thresh Hit subsetting limit for maximum pvalue
#' @param annot_path path to library annotation file
#' @param peptide_col_id_match For data_level=="peptide", the name of a column in annot that corresponds to the first column of data.
#' @param protein_col_id_match For data_level=="protein", the name of a column in annot that corresponds to the first column of data.
#' @param peptide_col_id_display For data_level=="peptide", the name of a column in annot that corresponds to identifiers to display in the markdown report corresponding to each peptide.
#' @param protein_col_id_display The name of a column in annot that corresponds to identifiers to display in the markdown report corresponding to each protein.
#' @param description_col_id The name of a column in annot that corresponds to identifiers to display in the markdown report corresponding to gene product or similar short protein descriptions to display in the markdown report.
#' @param flag_col_id Character string or vector of column  name(s) corresponding to annotation flags to indicate in the markdown report (e.g. of known autoantigens or surface proteins).
#' @param flag_type "binary" to detect whether or not there is text in that cell. "literal" to display exact text from cell.
#' @param pep_aa Character string of column name in annot corresponding to peptide amino acid sequence in the markdown report. Needed for epitopefindr.
#' @param binarize_clustergram Logical controlling whether or not clustergram will be binarized above/below rcp_thresh.
#' @param sample_field_delimiter Field delimiter in sample names (column names of case_rcp.)
#' @param sample_key_field Which field(s) to retrain from sample names when separated by sample_field delimiter.
#' @param run_epitopefindr Logical whether or not to generate epitopefindr targets.
#' @param use_AVARDA Logical, whether or not to generate AVARDA targets.
#' @param AVARDA_paths Path to case and control AVARDA output files
#' @param AVARDA_seropos_grep Filename substring for AVARDA seropositivitity matrix.
#' @param AVARDA_breadth_grep Filename substring for AVARDA breadth matrix.
#' @param remove_leading_x Process AVARDA filenames to so that "X100.SampleName" is corrected to "100.SampleName", etc. (case insensitive)
#'
#' @export


config_case_control <- function(
  filename = "config.tsv",
  sep = "\t",
  proj_id,
  library,
  ctrl_id,
  delimiter = ",",
  case_names,
  ctrl_names,
  input_dirs,
  data_types,
  data_names,
  data_level,
  min_hits_rcpgenerator = 2,
  hit_thresh,
  rcp_thresh = 0.95,
  stat_test = "Fisher",
  pval_correction = "BH",
  min_hits_enrichment = 2,
  min_hits_rcp = 2,
  min_freq_hits_rcp = 0.1,
  pval_thresh = 0.05,
  annot_path,
  peptide_col_id_match,
  protein_col_id_match,
  peptide_col_id_display,
  protein_col_id_display,
  description_col_id,
  flag_col_id,
  flag_type,
  pep_aa,
  binarize_clustergram,
  sample_field_delimiter,
  sample_key_field,
  run_epitopefindr,
  use_AVARDA,
  AVARDA_paths,
  AVARDA_seropos_grep,
  AVARDA_breadth_grep,
  remove_leading_x
){

  config_template <- data.table::fread(system.file("example_config.tsv", package = "phipcc"))
  for(i in 1:nrow(config_template)){
    config_template$value[i] <- eval(as.name(config_template$param[i]))
  }

  data.table::fwrite(config_template, filename, sep = sep)



}