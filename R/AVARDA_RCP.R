#' calculate RCP for AVARDA data
#'
#' @export

AVARDA_RCPGenerator <- function(AVARDA_paths, AVARDA_grep, case_names, ctrl_names){
  AVARDA_filepaths <- list.files(AVARDA_paths, full.names = TRUE)
  AVARDA_filepaths <- AVARDA_filepaths[grep(AVARDA_grep, AVARDA_filepaths)]

  AVARDA_files <- list()
  for(i in 1:length(AVARDA_filepaths)){
    AVARDA_files[[i]] <- data.table::fread(AVARDA_filepaths[i], data.table = FALSE)
  }

  AVARDA_breadth <- lapply(AVARDA_files, function(x) x[,c("name","Filtered Evidence #")])

  AVARDA_breadth_sum <- lapply(AVARDA_breadth, function(x){
    x %>% dplyr::group_by(name) %>% dplyr::summarise(Breadth = sum(`Filtered Evidence #`))
  }) %>% dplyr::bind_rows()

  case_names_alnum <- case_names %>% gsub("[^[:alnum:]]", "", .)
  ctrl_names_alnum <- ctrl_names %>% gsub("[^[:alnum:]]", "", .)
  AVARDA_breadth_names_alnum <- AVARDA_breadth_sum$name %>% gsub("[^[:alnum:]]", "", .)

  case_AVARDA <- AVARDA_breadth_sum$Breadth[match(case_names_alnum, AVARDA_breadth_names_alnum)]
  ctrl_AVARDA <- AVARDA_breadth_sum$Breadth[match(ctrl_names_alnum, AVARDA_breadth_names_alnum)]

  case_AVARDA[case_AVARDA %>% is.na] <- 0
  ctrl_AVARDA[ctrl_AVARDA %>% is.na] <- 0

  case_AVARDA %<>% data.frame %>% t
  ctrl_AVARDA %<>% data.frame %>% t

  case_AVARDA %<>% cbind("AVARDA_Breadth", .)
  ctrl_AVARDA %<>% cbind("AVARDA_Breadth", .)


  case_AVARDA_rcp <- RCPGenerator(case_AVARDA, ctrl_AVARDA)
  # ctrl_AVARDA_rcp <- RCPGenerator(ctrl_AVARDA, ctrl_AVARDA)

  colnames(case_AVARDA_rcp) <- c("ID", case_names)
  # colnames(ctrl_AVARDA_rcp) <- c("id", ctrl_names)

  # return(list(case_AVARDA_rcp, ctrl_AVARDA_rcp))
  return(case_AVARDA_rcp)

}