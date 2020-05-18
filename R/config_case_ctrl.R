#' setup configurations for this case control study
#'
#' writes table (defualt name config.tsv) with specified values.
#' For input into define_plan_case_control()
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