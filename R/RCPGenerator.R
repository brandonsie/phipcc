#' RCPGenerator calculates RCP, Relative Control Percentile, a case-control
#' statistic describing the percentile from 0.0 to 1.0 of one score against
#' that of a control population. For example, if a control population consisted
#' of three scores, 4.1, 5.0, 6.8, and 10.1, then a score of 5.0 would be
#' assigned an RCP of 0.25, because 5.0 is higher than 25% of the control
#' population. In this implementation, ties count as losses.
#'
#' @param case Data frame of cases.
#' @param ctrl Data frame of controls.
#' @param min_hits Numeric, hreshold for number of cases scoring above hit_thresh in
#' original score required to be retained after filtering. By default, min_hits
#' is set to 0 and thus RCP is calculated for all peptides. Setting a nonzero
#' value for min_hits is only recommended if calculating RCP for all peptides
#' is prohibitively slow.
#' @param hit_thresh Vector of numeric, threshold value to apply when considering min_hits.
#' hit_thresh is only evaluated if min_hits > 0.
#' @param verbose Logical to print status updates to console.



#(!) DON'T REQUIRE MIN ZHITS OR MIN RHITS. REPORT ALL DATA THEN SUBSET LATER
#(!) therefore also don't need hit thresh or RCP thresh.
# old params
# @param min.rhits Threshold for number of cases scoring above RCP.thresh
# in RCP required to be retained after filtering. If symm, min.rhits <- min.zhits
# @param RCP.thresh Theshold value to apply to min.rhits.


RCPGenerator <- function(case, ctrl, min_hits = 0, hit_thresh = 0,
                         verbose = FALSE){

  # ----------------------------------------------------------------------------
  # Configure settings

  print("RCPGenerator")
  print("dim case")
  print(dim(case))
  print("dim ctrl")
  print(dim(ctrl))
  print("min hits")
  print(min_hits)
  print("hit thresh")
  print(hit_thresh)

  #(!) only applies to old method
  # if ctrl = "self", then assign both to be the same
  self <- FALSE
  if(class(ctrl)[1] == "character") if(ctrl[1] == "self"){
    ctrl <- case
    self <- TRUE
  } else{
    if(nrow(case) != nrow(ctrl)){
      stop("Error: RCPGenerator: case control nrow mismatch.")
    }

  }

  if(min_hits > 0){
    print(dim(case))
    print(paste("Subsetting to ", min_hits, "above", hit_thresh))
    num_hits <- apply(case[,-1], 1, function(x){
      sum(x > hit_thresh)
    })

    case <- case[num_hits >= min_hits,]
    print(dim(case))
  }


  print("continue")

  # ----------------------------------------------------------------------------
  # RCP calculation

  pb <- txtProgressBar(min = 0, max = nrow(case), initial = 0, char = "=",
                       width = 20, style = 3)


  rcp.calc <- matrix(NA, nrow(case), ncol(case)) %>% data.frame
  rcp.calc[,1] <- case[,1]
  names(rcp.calc) <- names(case)

  if(self == FALSE){

    if(verbose){print(Sys.time())}
    for(i in 1:nrow(case)) {
      setTxtProgressBar(pb,i)
      quant <- stats::ecdf(as.numeric(ctrl[i,-1]))

      rcp.calc[i,-1] <- quant(as.numeric(case[i,-1]) - 1e-10)
    }

  } else{ #self vs. self runs faster
    i <- 1
    while(i <= nrow(case)){
      setTxtProgressBar(pb,i)

      #set upper bound to calculate i through j chunk incrementally
      inc <- 9999
      if(i+inc < nrow(case)){j <- i + inc} else{j <- nrow(case)}
      rcp.calc[i:j,-1] <- apply(case[i:j,-1],1, dplyr::percent_rank)
      i <- j + 1

    }
  }

  setTxtProgressBar(pb, nrow(case))
  close(pb)

  return(rcp.calc)

}
