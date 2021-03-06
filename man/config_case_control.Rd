% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config_case_ctrl.R
\name{config_case_control}
\alias{config_case_control}
\title{setup configurations for this case control study}
\usage{
config_case_control(filename = "config.tsv", proj_id, library, ctrl_id,
  delimiter, case_names, ctrl_names, input_dirs, data_types, data_names,
  data_level, min_hits_rcpgenerator, hit_thresh, rcp_thresh, stat_test,
  pval_correction, min_hits_enrichment, min_hits_rcp, min_freq_hits_rcp,
  pval_thresh, annot_path, peptide_col_id_match, protein_col_id_match,
  peptide_col_id_display, protein_col_id_display, description_col_id,
  flag_col_id, flag_type, pep_aa, binarize_clustergram,
  sample_field_delimiter, sample_key_field, run_epitopefindr, use_AVARDA,
  AVARDA_paths, AVARDA_seropos_grep, AVARDA_breadth_grep, remove_leading_x)
}
\description{
writes table (defualt name config.tsv) with specified values.
For input into define_plan_case_control()
}
