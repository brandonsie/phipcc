#' subsetAVARDA
#'
#' @export

subset_AVARDA <- function(
  AVARDA_data, names
){

  output_data <- list()

  for(i in 1:length(AVARDA_data)){
    this_data <- AVARDA_data[[i]]
    this_data$Virus <- this_data$Virus %>% strsplit("__") %>% lapply(function(x) {tail(x, 1)}) %>% unlist

    this_data_names_alnum <- colnames(this_data) %>% gsub("[^[:alnum:]]", "", .)
    names_alnum <- names %>% gsub("[^[:alnum:]]", "", .)

    this_data <- this_data[, this_data_names_alnum %in% c("Virus", names_alnum)]
    this_data_names_alnum <- colnames(this_data) %>% gsub("[^[:alnum:]]", "", .)
    colnames(this_data)[-1] <- names[match(this_data_names_alnum[-1], names_alnum)]


    output_data[[i]] <- this_data

  }


  names(output_data) <- names(AVARDA_data)

  return(output_data)
}