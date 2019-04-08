#' Pass parameters to R markdown case control template and knit html output file.
#'
#' @param output_file Name of generated html report.
#' @param output_dir Directory in which to write report.
#'
#' @export

render_from_template <- function(
  output_file = paste0(format(Sys.Date(),"%Y%m%d"),"_CaseControl_Report.html"),
  output_dir = getwd()
  ){



  # Define & Execute drakefile
  plan <- define_plan_case_control()
  drake::make(plan)

  # Read in drake outputs

  # Knit RMarkdown
  template <- system.file("template_case_control.Rmd", package="phipcc")
  rmarkdown::render(template, output_file = output_file, output_dir = output_dir,
                    params = list(config = NULL,
                                  candidates = NULL,
                                  graphs = NULL,
                                  clusters = NULL,
                                  motifs = NULL))

}
