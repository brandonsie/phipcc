#' Setup drake plan for case control report targetse.
#'
#' @param config_name Path to configuration spreadsheet.
#' @export


define_plan_case_control <- function(config_name = "config.tsv"){
  # --------------------------------------------------------------------------
  # Configuration Targets (try doing this outside of the plan)
  config <- data.table::fread(config_name, data.table = FALSE)

  param_split <- function(string, delim){
    stringr::str_split(string %>% unlist %>% as.character, delim) %>%
      unlist %>% as.character
  }

  # For Gather Data targets
  # these targets should be delimited by delimiter. write function to write/read easily
  delimiter  <- phipmake::getparam(config, "delimiter")
  case_names <- phipmake::getparam(config, "case_names") %>% param_split(delimiter)
  ctrl_names <- phipmake::getparam(config, "ctrl_names") %>% param_split(delimiter)
  input_dirs <- phipmake::getparam(config, "input_dirs") %>% param_split(delimiter)
  data_types <- phipmake::getparam(config, "data_types") %>% param_split(delimiter)
  hit_thresh <- phipmake::getparam(config, "hit_thresh") %>% param_split(delimiter) %>% as.numeric

  input_files <- lapply(data_types, function(x){
    # Specify actual file paths based on input_dirs and data_types
    all_files <- list.files(input_dirs, full.names = TRUE)
    all_files[grep(x, all_files, fixed = TRUE)]
  }) #list. each element is character vector of filepaths matching that datatype
    # (!) grep for data_types in input_dirs

  # For Statistics
  min_hits_rcpgenerator <- phipmake::getparam(config, "min_hits_rcpgenerator") %>% as.numeric
  enrichment_thresh <- phipmake::getparam(config, "enrichment_thresh") %>% as.numeric
  rcp_thresh <- phipmake::getparam(config, "hit_thresh") %>% as.numeric
  stat_test <- phipmake::getparam(config, "stat_test")
  pval_correction <- phipmake::getparam(config, "pval_correction")

  #(!) enrichment_thresh vs hit_thresh. needs to use list for promax/polycl. different data different threshold

  # For filtering hits & annotations
  min_hits_enrichment <- phipmake::getparam(config, "min_hits_enrichment") %>% as.numeric
  min_hits_rcp <- phipmake::getparam(config, "min_hits_rcp") %>% as.numeric
  min_freq_hits_rcp <- phipmake::getparam(config, "min_freq_hits_rcp") %>% as.numeric
  pval_thresh <- phipmake::getparam(config, "pval_thresh") %>% as.numeric
  annot_path <- phipmake::getparam(config, "annot_path")

  # for epitopefindr
  epf_params <- phipmake::getparam(config, "epf_params")


  # ============================================================================
  # Plan Start

  plan <- drake::drake_plan(

    # --------------------------------------------------------------------------
    # Gather data

    case_data = target(
      gather_sample_list(!!data_types, !!input_files, !!case_names)
      #gather_sample_list should return a phiplist
      # element NAMES are data_types
      # elements are data corresponding to datatype. from phipmake::gather_data
    ),
    ctrl_data = target(
      gather_sample_list(!!data_types, !!input_files, !!ctrl_names)
    ),

    # --------------------------------------------------------------------------
    # Compute statistics

    #(!) write compute_rcp_list. calls RCPGenerator for all. (try dplyr)
    case_rcp = target(
      compute_rcp_list(case_data, ctrl_data,
                       min_hits = !!min_hits_rcpgenerator,
                       hit_thresh = !!hit_thresh)),
    ctrl_rcp = target(compute_rcp_list(ctrl_data, "self")),

    #(!) write compute_stats_list. plotdata
    data_stats = target(
      compute_stats_list(case_data, ctrl_data, case_rcp, ctrl_rcp,
                         !!enrichment_thresh, !!rcp_thresh,
                         !!stat_test, !!pval_correction)
      #(!) add params config enrichment_thresh, rcp_thresh, test
      #(!) include multi pvalue correction here
    ),

    # --------------------------------------------------------------------------
    # Protein Candidate Generation


    # determine whether to use min_hits_rcp or min_freq_hits_rcp. use larger.
    num_case_samples = target(case_data[[1]] %>% nrow - 1),
    num_rcp_thresh = target({
      num_rcp_for_freq_thresh <- (num_case_samples * !!min_freq_hits_rcp) %>%
        ceiling
      num_rcp_thresh <- max(num_rcp_for_freq_thresh, !!min_hits_rcp)
    }),

    data_filtered = target(
      filter_hits_list(
        data_stats, !!min_hits_enrichment, num_rcp_thresh, !!pval_thresh)
    ),

    #(!) add check if any list has 0 hits after this. for each datatype.
    # if so then delete that element of data_filtered.


    # annotate_genes_list()
    #     # take input of (1) input field name (pep_id etc.), (2) output field names c(gene, product, pep_pos?) or c(uniprot_acc, product). (3) delimiter / spearators

    # annotate_flags_list()
    #     # c(autoantigen, cell surface protein, virus, etc.). (2) asis flag (if fasle don't copy exact text, just print category name) . virscan uniprot genus etc.

    # choose_pval_list()
    #     # take min/max/specified pval if multiple peptides

    # prepare_candidate_tables_list()
    # rename header prep kable etc.

    # (!) filter hits, annotate hits, format table
    # (!) return list of the possible tables. named.
    #(!) multipval is for collapsing to protein

    #(!) need to add flag to handle polyclonal and promax differently.


    # --------------------------------------------------------------------------
    # Exploratory Graphs

    # plot1_hitfreq()

    # plot2_hitscore()

    # plot3_pval()



    # --------------------------------------------------------------------------
    # Clustergram

    # heatmap_generate() # return static and interactive as list?


    # --------------------------------------------------------------------------
    # eptiopefindr
    #(!) include old output table?

    # run_epitopefindr()

    # --------------------------------------------------------------------------
    # Epitope Clustergram

    # epitope_heatmap_generate()

    # --------------------------------------------------------------------------

    #(!) these steps need to be tidyeval'ed for each input file type
    #(!) how does this work with input_files list?

    # write outputs including relevant paths
    #(!) add write_target with file_out for every data generating step.
    #  ^ (!) write to /data/ for tidiness

    blank_final_target = target()

  ) # end drake plan


  #(!) setup wrapper function to read in config.tsv and send values to plan as parameters? so can add defualt values? then define plan rewrites config with all used values?

  return(plan)
}