#' prepare_AVARDA_candidate_table
#'
#' @export


prepare_AVARDA_candidate_table <- function(
  AVARDA_filtered
){

  AVARDA_subset <- AVARDA_filtered[, c(
    "Virus",
    "Case.Seropos.Freq", "Ctrl.Seropos.Freq",
    "Median.Seropos.PVal.Case", "Median.Seropos.PVal.Ctrl",
    "Seropos.Fisher.PVal",
    #"Breadth.RCP.Hits.Case.Freq", "Breadth.RCP.Hits.Ctrl.Freq",
    #"Median.Breadth.Hit.Case", "Median.Breadth.Hit.Ctrl",
    #"Breadth.Fisher.PVal",
    "Median.Seropos.Breadth.Case", "Median.Seropos.Breadth.Ctrl",
    "Seropos.Breadth.Wilcox.PVal",
    "PVal.Min")]

  AVARDA_subset <- AVARDA_subset[order(AVARDA_subset$PVal.Min), ]
  output_data <- AVARDA_subset[, -ncol(AVARDA_subset)]

  colnames(output_data) <- c(
    "Virus",
    "Case Seropos Frequency", "Ctrl Seropos Frequency",
    "Median Seropositive Case P Value", "Median Seropositive Ctrl P Value",
    "Seropos Fisher P Value",
    #"Case Breadth Hit Frequency", "Ctrl Breadth Hit Frequency",
    #"Median Hit Case Breadth", "Median Hit Ctrl Breadth",
    #"Breadth P Value"
    "Median Seropositive Case Breadth", "Median Seropositive Ctrl Breadth",
    "Seropos Breadth Wilcox P Value"
    )

  output_data$`Case Seropos Frequency` %<>% round(2)
  output_data$`Ctrl Seropos Frequency` %<>% round(2)
  # output_data$`Case Breadth Hit Frequency` %<>% round(2)
  # output_data$`Ctrl Breadth Hit Frequency` %<>% round(2)
  # output_data$`Median Hit Case Breadth` %<>% round(1)
  # output_data$`Median Hit Ctrl Breadth` %<>% round(1)
  output_data$`Median Seropositive Case Breadth` %>% round(1)
  output_data$`Median Seropositive Ctrl Breadth` %>% round(1)


  output_data$`Seropos Fisher P Value` %<>% formatC(format = "e", digits = 1)
  output_data$`Median Seropositive Case P Value` %<>% formatC(format = "e", digits = 1)
  output_data$`Median Seropositive Ctrl P Value` %<>% formatC(format = "e", digits = 1)
  # output_data$`Breadth P Value` %<>% formatC(format = "e", digits = 1)
  output_data$`Seropos Breadth Wilcox P Value` %<>% formatC(format = "e", digits = 1)

  return(output_data)

}



# AVARDA output table
#
#
#
# Virus (abbreviated strsplit name)
# Data Types (seropos, breadth)
# Hit Freq case/ctrl (seropos)
# Pvalue and parenthesis data type
#
# maybe keep seropos and breadth separate? but then sort rows on table so they are adjacent?



# AVARDA_ctrl_data$breadth[grep("Human_alphaherpesvirus_2", AVARDA_ctrl_data$breadth$Virus),] %>% extract(-1) %>% as.numeric



#' prepare_AVARDA_candidate_table_html
#'
#' @export

prepare_AVARDA_candidate_table_html <- function(data){

  # fix column names. used to be 2:5, 7:10
  names(data)[c(2:5, 7:8)] <- rep(c("Case", "Ctrl"), 3)
  options(knitr.table.format = "html")


  # frequency. previously also included col 7
  data[,c(2:3)] <- data[,c(2:3)] %>% dplyr::mutate_if(is.numeric, function(x){
    kableExtra::cell_spec(
      x, bold = TRUE,
      color = kableExtra::spec_color(x, end=0.9, scale_from = c(0, 1)),
      font_size = kableExtra::spec_font_size(x, begin = 12, end = 15))
  })

  # hit score. previously 9 and 10
  data[,7:8] <- data[,7:8] %>% dplyr::mutate_if(is.numeric, function(x){
    kableExtra::cell_spec(
      x, bold = TRUE,
      color = ifelse(x > 50, "#B4DE2CFF",
                   kableExtra::spec_color(x, end=0.9,
                                          scale_from = c(min(data[,7:8]),50))),
      font_size = ifelse(x > 50, 15,
                       kableExtra::spec_font_size(x, begin = 12, end = 15,
                                                  scale_from=c(min(data[,7:8]),50))))
  })



  return(data)
}