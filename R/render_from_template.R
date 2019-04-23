#' Pass parameters to R markdown case control template and knit html output file.
#'
#' @param output_file Name of generated html report.
#' @param output_dir Directory in which to write report.
#' @param set_title Character string to be used for report title.
#' @param config Configuration table used in phipcc:define_plan_case_control. After running drake::make on this plan, config can be retreived by calling drake::readd(config).
#' @param candidate_table_flagged Kable html object of flagged/annotated candidate hits to display in report. Retrieved by calling drake::readd(candidate_table_flagged_html).
#' @param candidate_table_full Kable html object of all candidate hits to display in report. Retrieved by calling drake::readd(candidate_table_html).
#' @param graphs List of ggplot objects to be plotted in report. Retreived by calling list(drake::readd(plot1), drake::readd(plot2), drake::readd(plot3)).
#' @param cluster1 List of gplots::heatmap.2 object and sorted data. Retreived by calling drake::readd(clustergram1).
#' @param motifs Directory path to epitopefindr motifs folder. Typically paste0(getwd(), "/data/epitopefindr/intermediate_files/msa/").
#' @param cluster2 List of gplots::heatma.2 object after collapsing epitopefindr groups. Retreived by calling drake::readd(clustergram2)
#'
#' @seealso \code{\link{define_plan_case_control}}
#'
#' @export

render_from_template <- function(
  output_file = paste0(format(Sys.Date(),"%Y%m%d"),"_CaseControl_Report.html"),
  output_dir = getwd(),
  set_title,
  config, candidate_table_flagged, candidate_table_full, graphs, cluster1, motifs, cluster2
  ){



  # Define & Execute drakefile
  # plan <- define_plan_case_control()
  # drake::make(plan)

  # Read in drake outputs

  # Knit RMarkdown
  template <- system.file("template_case_control.Rmd", package="phipcc")
  rmarkdown::render(
    template,
    output_file = output_file,
    output_dir = output_dir,
    params = list(
      set_title = set_title,
      config = config,
      candidate_table_flagged = candidate_table_flagged,
      candidate_table_full = candidate_table_full,
      graphs = graphs,
      cluster1 = cluster1,
      motifs = motifs,
      cluster2 = cluster2))

}
