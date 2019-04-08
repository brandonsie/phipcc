#' Calculate relative control percentile statistic for a list of data.
#'
#' Case and control should be lists of the same length. Each element of the list
#' should contain a data frame for which to compute rcp for/against. Corresponding
#' elements of the case and control lists should be data frames of equal number
#' of rows.
#'
#' @param case Case samples for which to compute RCP.
#' @param ctrl Control samples to compute RCP against.
#' @param min_hits Numeric, hreshold for number of cases scoring above hit_thresh in
#' original score required to be retained after filtering. By default, min_hits
#' is set to 0 and thus RCP is calculated for all peptides. Setting a nonzero
#' value for min_hits is only recommended if calculating RCP for all peptides
#' is prohibitively slow.
#' @param hit_thresh Vector of numeric, threshold value to apply when considering min_hits.
#' hit_thresh is only evaluated if min_hits > 0.
#'
#' @export


compute_rcp_list <- function(case, ctrl, min_hits = 0, hit_thresh = 0){

  # Check for input errors.
  if(class(ctrl)[1] == "character"){
    if(ctrl[1] != "self"){
      stop("Error: compute_rcp_list: unrecognized character input for ctrl.")
    }

  } else if(length(case) != length(ctrl)){
    stop("Error: compute_rcp_list: case and control length mismatch.")
  } else if((names(case) == names(ctrl)) %>% mean < 1){
    stop("Error: compute_rcp_list: mismatch in case and control element names.")
  }

  # Continue
  output_data <- list()
  if(length(hit_thresh) == 1) hit_thresh <- rep(hit_thresh, length(case))

  for(i in 1:length(case)){

    if(class(ctrl)[1] == "character"){
      if(ctrl[1] == "self"){
        output_data[[i]] <- RCPGenerator(case[[i]], "self", min_hits, hit_thresh[i])

      } else{
        stop("Error: invalid ctrl input to compute_rcp_list")
      }

    } else{
      output_data[[i]] <- RCPGenerator(case[[i]], ctrl[[i]], min_hits, hit_thresh[i])
    }

    names(output_data)[i] <- names(case)[i]
  }

  return(output_data)

}

