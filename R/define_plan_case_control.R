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

  # Gather Data targets
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
  rcp_thresh <- phipmake::getparam(config, "rcp_thresh") %>% as.numeric
  stat_test <- phipmake::getparam(config, "stat_test")
  pval_correction <- phipmake::getparam(config, "pval_correction")

  # For filtering hits
  min_hits_enrichment <- phipmake::getparam(config, "min_hits_enrichment") %>% as.numeric
  min_hits_rcp <- phipmake::getparam(config, "min_hits_rcp") %>% as.numeric
  min_freq_hits_rcp <- phipmake::getparam(config, "min_freq_hits_rcp") %>% as.numeric
  pval_thresh <- phipmake::getparam(config, "pval_thresh") %>% as.numeric

  # for annotations and plots
  data_level <- phipmake::getparam(config, "data_level") %>% param_split(delimiter)
  annot_path <- phipmake::getparam(config, "annot_path")
  peptide_col_id_match <- phipmake::getparam(config, "peptide_col_id_match")
  protein_col_id_match <- phipmake::getparam(config, "protein_col_id_match")
  peptide_col_id_display <- phipmake::getparam(config, "peptide_col_id_display")
  protein_col_id_display <- phipmake::getparam(config, "protein_col_id_display")
  description_col_id <- phipmake::getparam(config, "description_col_id")
  flag_col_id <- phipmake::getparam(config, "flag_col_id") %>% param_split(delimiter)
  flag_type <- phipmake::getparam(config, "flag_type") %>% param_split(delimiter)

  pep_aa <- phipmake::getparam(config, "pep_aa")

  # for clustergram
  binarize_clustergram <- phipmake::getparam(config, "binarize_clustergram") %>% as.logical
  sample_field_delimiter <- phipmake::getparam(config, "sample_field_delimiter")
  sample_key_field <- phipmake::getparam(config, "sample_key_field") %>% as.numeric

  # AVARDA
  use_AVARDA <- phipmake::getparam(config, "use_AVARDA") %>% as.logical
  AVARDA_paths <- phipmake::getparam(config, "AVARDA_paths") %>% param_split(delimiter)
  AVARDA_seropos_grep <- phipmake::getparam(config, "AVARDA_seropos_grep")
  AVARDA_breadth_grep <- phipmake::getparam(config, "AVARDA_breadth_grep")


  # for epitopefindr
  epf_params <- phipmake::getparam(config, "epf_params")

  # ----------------------------------------------------------------------------
  # Output File Names
  output.dir <- "data"
  sn.odir <- paste0(output.dir,"/")
  if(!dir.exists(output.dir)) dir.create(output.dir)

  output.ext <- "tsv"
  sn.ext <- paste0(".", output.ext)

  names.case.data <- paste0(sn.odir, data_types, "_Case",sn.ext)
  names.ctrl.data <- paste0(sn.odir, data_types, "_Ctrl",sn.ext)
  names.case.rcp <- paste0(sn.odir, data_types, "_Case_RCP",sn.ext)
  names.ctrl.rcp <- paste0(sn.odir, data_types, "_Ctrl_RCP",sn.ext)


  # ============================================================================
  # Plan Start


  plan <- drake::drake_plan(

    config = target(data.table::fread(file_in(!!config_name),
                                      data.table = FALSE)),
    # Gather data --------------------------------------------------------------

    case_data = target(
      gather_sample_list(!!data_types, !!input_files, !!case_names)
      #gather_sample_list should return a list
      # element NAMES are data_types
      # elements are data corresponding to datatype. from phipmake::gather_data
    ),
    ctrl_data = target(
      gather_sample_list(!!data_types, !!input_files, !!ctrl_names)
    ),
    #(!) need tidyeval for data.types OR bind_rows

    # write_case_data = target(
    #   phipmake::write_data(case_data, file_out(!!names.case.data))
    # ),
    # write_ctrl_data = target(
    #   phipmake::write_data(ctrl_data, file_out(!!names.ctrl.data))
    # ),

    # Compute statistics -------------------------------------------------------

    #(!) write compute_rcp_list. calls RCPGenerator for all. (try dplyr)
    case_rcp = target(
      compute_rcp_list(case_data, ctrl_data,
                       min_hits = !!min_hits_rcpgenerator,
                       hit_thresh = !!hit_thresh)),
    ctrl_rcp = target(compute_rcp_list(ctrl_data, "self")),

    # write_case_rcp = target(
    #   phipmake::write_data(case_rcp, file_out(!!names.case.rcp))
    # ),
    # write_ctrl_rcp = target(
    #   phipmake::write_data(ctrl_rcp, file_out(!!names.ctrl.rcp))
    # ),
    # write_case_rcp_data = phipmake::write_data(case_rcp_data, "data/case_rcp_data.tsv"),
    # write_ctrl_rcp_data = phipmake::write_data(ctrl_rcp_data, "data/ctrl_rcp_data.tsv"),

    # subset data based on any filteres applied previously to case_rcp
    case_data_subset = target(subset_data(case_data, case_rcp)),
    ctrl_data_subset = target(subset_data(ctrl_data, case_rcp)),
    ctrl_rcp_subset  = target(subset_data(ctrl_rcp,  case_rcp)),
    # write_case_data_subset = phipmake::write_data(case_data_subset, "data/case_data_subset.tsv"),
    # write_ctrl_data_subset = phipmake::write_data(ctrl_data_subset, "data/ctrl_data_subset.tsv"),
    # write_ctrl_rcp_subset  = phipmake::write_data(ctrl_rcp_subset,  "data/ctrl_rcp_subset.tsv"),

    #(!) write compute_stats_list. plotdata
    data_stats = target(
      compute_stats_list(
        case_data_subset, ctrl_data_subset, case_rcp, ctrl_rcp_subset,
        !!hit_thresh, !!rcp_thresh, !!stat_test, !!pval_correction)
    ),


    # Optional AVARDA module -------------------------------------------------------

    AVARDA_data = target(
      if(!!use_AVARDA){
        read_AVARDA(file_in(!!AVARDA_paths), !!AVARDA_seropos_grep, !!AVARDA_breadth_grep)
      } else NA
    ),

    AVARDA_case_data = target(
      if(!!use_AVARDA){
        subset_AVARDA(AVARDA_data, !!case_names)
      } else NA
    ),

    AVARDA_ctrl_data = target(
      if(!!use_AVARDA){
        subset_AVARDA(AVARDA_data, !!ctrl_names)
      } else NA
    ),

    #(!) move stats targets outside

    # AVARDA_case_breadth_rcp = target(
    #   if(!!use_AVARDA){
    #     phipmake::RCPGenerator(AVARDA_case_data$breadth, AVARDA_ctrl_data$breadth)
    #   } else NA
    # ),
    #
    # AVARDA_ctrl_breadth_rcp = target(
    #   if(!!use_AVARDA){
    #     phipmake::RCPGenerator(AVARDA_ctrl_data$breadth, "self")
    #   } else NA
    # ),

    AVARDA_stats = target(
      if(!!use_AVARDA){
        StatsGenerator_AVARDA(AVARDA_case_data, AVARDA_ctrl_data,
                              # AVARDA_case_breadth_rcp, AVARDA_ctrl_breadth_rcp,
                              seropos_pval = !!pval_thresh,
                              # rcp_thresh = !!rcp_thresh
                              )
      } else NA
    ),

    AVARDA_filtered = target(
      if(!!use_AVARDA){
        # Take seropos significant either up or down
        # take breadth significant only up in cases
        AVARDA_stats[(AVARDA_stats$Seropos.Fisher.PVal < !!pval_thresh) |
                     ((AVARDA_stats$Seropos.Breadth.Wilcox.PVal < !!pval_thresh)# &
                      #(AVARDA_stats$Breadth.RCP.Hits.Case.Freq >
                      #   AVARDA_stats$Breadth.RCP.Hits.Ctrl.Freq
                       #)
                      ),] %>% na.omit
      } else NA
    ),

    AVARDA_candidate_table = target(
      if(!!use_AVARDA){
        prepare_AVARDA_candidate_table(AVARDA_filtered)
      } else NA
    ),

    AVARDA_candidate_table_html = target(
      if(!!use_AVARDA){
        prepare_AVARDA_candidate_table_html(AVARDA_candidate_table)
      } else NA
    ),

    AVARDA_clustergram_data = target(
      if(!!use_AVARDA){
        prepare_AVARDA_clustergram_data(
          AVARDA_case_data, #AVARDA_case_breadth_rcp,
          AVARDA_filtered, !!pval_thresh# , #!!rcp_thresh
          )
      } else NA
    ),


    generic_AVARDA_target = target(
      if(!!use_AVARDA){

      } else NA
    ),


    # Protein Candidate Generation ---------------------------------------------

    num_case_samples = target(case_data[[1]] %>% ncol - 1),
    num_rcp_thresh = target({
      # determines whether to use min_hits_rcp or min_freq_hits_rcp. use larger.
      num_rcp_for_freq_thresh <-
        (num_case_samples * !!min_freq_hits_rcp) %>% ceiling
      num_rcp_thresh <- max(num_rcp_for_freq_thresh, !!min_hits_rcp)
    }),

    data_filtered = target(
      filter_hits_list(
        data_stats, !!min_hits_enrichment, num_rcp_thresh, !!pval_thresh)
      #(!) add check if any list has 0 hits after this. for each datatype.
      # if so then delete that element of data_filtered.
    ),


    filtered_data_types = target(names(data_filtered)),
    filtered_data_level = target({
      type_level_map <- data.frame(type = !!data_types, level = !!data_level);
      return(type_level_map$level[match(filtered_data_types, type_level_map$type)])
    }),
    annot = target(data.table::fread(!!annot_path, data.table = FALSE)),

    data_annotated = target(
      annotate_cc_list(data_filtered, filtered_data_level, annot,
        !!peptide_col_id_match, !!protein_col_id_match, !!peptide_col_id_display,
        !!protein_col_id_display, !!description_col_id, !!flag_col_id, !!flag_type
      )
    ),

    data_annotated_rbind = target(dplyr::bind_rows(data_annotated)),
    candidate_table = target(prepare_candidate_table(data_annotated_rbind)),
    candidate_table_html = target(prepare_candidate_table_html(candidate_table)),

    candidate_table_flagged = target(
      candidate_table[candidate_table$Annotations != "",]
    ),
    candidate_table_flagged_html = target(prepare_candidate_table_html(candidate_table_flagged)),

    # Exploratory Graphs -------------------------------------------------------

    plot1 = target(plot1_hitfreq(data_annotated_rbind, filtered_data_types)),
    plot2 = target(plot2_hitscore(data_annotated_rbind, filtered_data_types)),
      #(!) try geom_count instead of geom_jitter
      #(!) plot 2 used to have log scale but then lose values of 0.
    plot3 = target(plot3_pval(data_annotated_rbind, filtered_data_types)),


    # Clustergram --------------------------------------------------------------

    run_clustergram = target(ifelse(nrow(data_annotated_rbind) > 1, TRUE, FALSE)),

    clustergram_rawdata = target(
      if(run_clustergram){
        prepare_clustergram_data(
          case_rcp, data_annotated_rbind, !!binarize_clustergram, !!rcp_thresh,
          !!sample_field_delimiter, !!sample_key_field, AVARDA_clustergram_data)
      } else{NA}
    ),




    clustergram1 = target(
      if(run_clustergram){
        generate_clustergram(clustergram_rawdata, data_annotated_rbind) # (!) return list
        #first element is plot, second is sorted spreadsheet
      } else{NA}
    ),

    #(!) work on clustergram formatting. peptide/sample name sizes, etc.


    # eptiopefindr -------------------------------------------------------------
    #(!) include old output table?

    fasta_table = target(
      data.frame(ID = data_annotated_rbind$ProteinPeptide,
                 #ID = paste(data_annotated_rbind$Protein,
                 #                   data_annotated_rbind$Peptide, sep = "|"),
                 Seq = data_annotated_rbind$pep_aa) %>% na.omit
    ),

    run_epitopefindr = target(ifelse(nrow(fasta_table) > 0, TRUE, FALSE)),

    write_hits_fasta = target(
      epitopefindr::writeFastaAA(fasta_table, file_out("data/hits.fasta"))
    ),

    epitopefindr = target({
      if(run_epitopefindr){
        epitopefindr::epfind(
          data = file_in("data/hits.fasta"),
          output.dir = "data/epitopefindr/",
          make.png = FALSE)
        file_out("data/epitopefindr/epitopeSummary.csv")

      } else{NA}

    }),

    # Epitope Clustergram ------------------------------------------------------


    epitopeSummary = target(
      if(run_epitopefindr){
        data.table::fread(file_in("data/epitopefindr/epitopeSummary.csv"),
                          header = TRUE)

      } else{NA}
    ),

    epitope_clustergram_rawdata = target(
      if(run_epitopefindr){
        prepare_epitope_clustergram_data(
          clustergram1[[3]],
          file_in("data/epitopefindr/epitopeSummary.csv")
        )
      } else{NA}

    ),


    clustergram2 = target(
      if(run_epitopefindr){
        generate_clustergram(epitope_clustergram_rawdata, data_annotated_rbind, epitopeSummary)
      } else{NA}
    ),


    # --------------------------------------------------------------------------
    # Output

    graphs = target(
      list(plot1, plot2, plot3)
    ),

    write_output_targets = target(
      save(config, candidate_table_flagged_html, candidate_table_html,
           AVARDA_candidate_table_html, graphs, clustergram1, clustergram2,
           file = "data/outputs.RData")
    ),

    # --------------------------------------------------------------------------

    #(!) add write_target with file_out for every data generating step.
    #  ^ (!) write to /data/ for tidiness?
    # (!) need to add filename-generating section

    blank_final_target = target()

  ) # end drake plan


  #(!) setup wrapper function to read in config.tsv and send values to plan as parameters? so can add defualt values? then define plan rewrites config with all used values?

  return(plan)
}
