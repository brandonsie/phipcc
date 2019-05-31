#' Pass parameters to R markdown case control template and knit html output file.
#'
#' @param template Rmd output template file.
#' @param output_file Name of generated html report. If not provided, a default value is generated.
#' @param output_dir Directory in which to write report.
#' @param set_title Character string to be used for report title.  If not provided, a default value is generated.
#' @param import_rdata Logical whether or not to look for an .rdata file containing necessary targests at the path specified in rdata_path.
#' @param rdata_path If import_rdata == TRUE, then an file with the specified name will be passed to load().
#' @param config Configuration table used in phipcc:define_plan_case_control. After running drake::make on this plan, config can be retreived by calling drake::readd(config).  If not provided, a default value is generated.
#' @param candidate_table_flagged Kable html object of flagged/annotated candidate hits to display in report. Retrieved by calling drake::readd(candidate_table_flagged_html).  If not provided, a default value is generated.
#' @param candidate_table_full Kable html object of all candidate hits to display in report. Retrieved by calling drake::readd(candidate_table_html).  If not provided, a default value is generated.
#' @param AVARDA_candidate_table Kable html object of all AVARDA hits to display in report. Retrieved by calling drake::readd(AVARDA_candidate_table).  If not provided, a default value is generated.
#' @param graphs List of ggplot objects to be plotted in report. Retreived by calling list(drake::readd(plot1), drake::readd(plot2), drake::readd(plot3)).  If not provided, a default value is generated.
#' @param clustergram1 List of gplots::heatmap.2 object and sorted data. Retreived by calling drake::readd(clustergram1).  If not provided, a default value is generated.
#' @param motifs Directory path to epitopefindr motifs folder. Typically paste0(getwd(), "/data/epitopefindr/intermediate_files/msa/").  If not provided, a default value is generated.
#' @param clustergram2 List of gplots::heatma.2 object after collapsing epitopefindr groups. Retreived by calling drake::readd(clustergram2).  If not provided, a default value is generated.
#' @param env Environment from which to load drake objects.
#'
#' @seealso \code{\link{define_plan_case_control}}
#'
#' @export

render_from_template <- function(
  template = system.file("template_case_control.Rmd", package="phipcc"),
  output_file = NULL, output_dir = getwd(), set_title = NULL,
  import_rdata = FALSE, rdata_path = "data/outputs.RData",
  config = NULL,
  candidate_table_flagged = NULL, candidate_table_full = NULL,
  AVARDA_candidate_table = NULL,
  graphs = NULL,
  clustergram1 = NULL, motifs = NULL, clustergram2 = NULL
  ){


  if(import_rdata == TRUE){
    load(rdata_path)
    candidate_table_flagged <- candidate_table_flagged_html
    candidate_table_full <- candidate_table_html
    AVARDA_candidate_table <- AVARDA_candidate_table_html
  }

  # if(is.null(config)) drake::loadd(config)
  # if(is.null(candidate_table_flagged)) drake::loadd(candidate_table_flagged)
  # if(is.null(clustergram1)) drake::loadd(clustergram1, envir = env)
  # if(is.null(clustergram2)) drake::loadd(clustergram2, envir = env)

  #manually define defaults for `graphs` and `motifs`
  if(is.null(graphs)){
    graphs <- list(drake::readd(plot1), drake::readd(plot2), drake::readd(plot3))
  }

  if(is.null(motifs)) motifs <- paste0(getwd(), "/data/epitopefindr/intermediate_files/msa/")

  if(is.null(candidate_table_flagged)) candidate_table_flagged <- drake::readd(candidate_table_flagged_html)
  if(is.null(candidate_table_full)) candidate_table_full <- drake::readd(candidate_table_html)
  if(is.null(AVARDA_candidate_table)) AVARDA_candidate_table <- drake::readd(AVARDA_candidate_table_html)

  targets <- c("config", "candidate_table_flagged", "candidate_table_full",
               "graphs", "clustergram1", "motifs", "clustergram2", "AVARDA_candidate_table")

  for(i in 1:length(targets)){
    if(targets[i] %>% as.name %>% eval %>% is.null){drake::loadd(targets[i])}
  }

  proj_id <- phipmake::getparam(config, "proj_id")
  library <- phipmake::getparam(config, "library")
  ctrl_id <- phipmake::getparam(config, "ctrl_id")


  if(is.null(output_file)) output_file <-
    paste0(
      format(Sys.Date(),"%Y%m%d"), "_", proj_id, "_", ctrl_id, "_", library,
      "_CaseControl_Report.html")

  if(is.null(set_title)) set_title <-
    paste0(proj_id, " vs. ", ctrl_id, " PhIP-Seq Case-Control Report: ", library, " Library")



  # Define & Execute drakefile
  # plan <- define_plan_case_control()
  # drake::make(plan)

  # Read in drake outputs

  # Knit RMarkdown
  template <- template
  rmarkdown::render(
    template,
    output_file = output_file,
    output_dir = output_dir,
    params = list(
      set_title = set_title,
      config = config,
      candidate_table_flagged = candidate_table_flagged,
      candidate_table_full = candidate_table_full,
      AVARDA_candidate_table = AVARDA_candidate_table,
      graphs = graphs,
      cluster1 = clustergram1,
      motifs = motifs,
      cluster2 = clustergram2))

}
