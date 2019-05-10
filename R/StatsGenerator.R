#' performs a first round of
#' calculations on the data such as hit frequency and Fisher's test. These
#' calculations will be used in subsequent graphing.
#'
#' @param case Case data
#' @param ctrl Control data
#' @param case_rcp Case RCP data
#' @param ctrl_rcp Control RCP data
#' @param hit_thresh Enrichment hit threshold.
#' @param rcp_thresh RCP hit threshold.
#' @param stat_test Statistical test. Currently supports "Fisher"
#' @param pval_correction Multiple hypothesis pvalue correction method. BH default.
#'
#' @export
#
# output_data[[i]] <- StatsGenerator(
#   case[[i]], ctrl[[i]], case_rcp[[i]], ctrl_rcp[[i]],
#   hit_thresh, rcp_thresh, stat_test, pval_correction)


StatsGenerator <- function(
  case, ctrl, case_rcp, ctrl_rcp, hit_thresh = 10, rcp_thresh = 0.95,
  stat_test = "Fisher", pval_correction = "BH", note_range = 1) {

  data <- data.frame(id = case_rcp[,note_range])

  #Number of Cases (for RCP Hit Frequency and for Fisher)
  num.cases <- ncol(case)-note_range
  num.ctrls <- ncol(ctrl)-note_range

  #Raw HitS
  data$Case.Hits <-apply(case[,-note_range],1,
                         function(x) sum(as.numeric(x)>=hit_thresh))
  data$Ctrl.Hits <-apply(ctrl[,-note_range],1,
                         function(x) sum(as.numeric(x)>=hit_thresh))
  data$Case.Hit.Freq <- as.numeric(data$Case.Hits) / num.cases
  data$Ctrl.Hit.Freq <- as.numeric(data$Ctrl.Hits) / num.ctrls

  #RCP Hits
  # data$Mean.RCP <- apply(rcp[,-(note_range)],1,mean)
  data$RCP.Hits.Case <- apply(
    case_rcp[,-(note_range)],1,function(x) sum(as.numeric(x)>=rcp_thresh))
  data$RCP.Hits.Ctrl <- apply(
    ctrl_rcp[,-(note_range)],1, function(x) sum(as.numeric(x)>=rcp_thresh))
  data$RCP.Hit.Freq.Case <- data$RCP.Hits.Case / num.cases
  data$RCP.Hit.Freq.Ctrl <- data$RCP.Hits.Ctrl / num.ctrls


  #Median Enrichment Score of RCP Hits

  calcHitMedian <- function(input_data, input_rcp, rcp_thresh){

    this_data_hits <- input_data[as.numeric(input_rcp) >= rcp_thresh]

    if(length(this_data_hits) == 0){
      return(0)
    } else{
      return(median(as.numeric(this_data_hits)))
    }
  }

  for(i in 1:nrow(case)){
    data$Median.Hit.Score.Case[i] <- calcHitMedian(
      case[i, -note_range], case_rcp[i, -note_range], rcp_thresh)
    data$Median.Hit.Score.Ctrl[i] <- calcHitMedian(
      ctrl[i, -note_range], ctrl_rcp[i, -note_range], rcp_thresh)


    # data$Median.Hit.Score.Case[i] <-
    #   median(case[i,-note_range][case_rcp[i,-note_range]>rcp_thresh])
    # data$Median.Hit.Score.Ctrl[i] <-
    #   median(ctrl[i,-note_range][ctrl_rcp[i,-note_range]>rcp_thresh])
  }


  if(stat_test == "Fisher"){
    # Fisher Exact 1 (Raw)
    ft <- data.frame(t(sapply(1:nrow(case), function(x){
      fisher.test(cbind(c(data$Case.Hits[x],num.cases-data$Case.Hits[x]),
                        c(data$Ctrl.Hits[x],num.ctrls-data$Ctrl.Hits[x])))
    })))
    data$Fisher.PValue <- p.adjust(as.numeric(ft$p.value),method = pval_correction)

    # Fisher Exact 2 (RCP)
    ft2 <- data.frame(t(sapply(1:nrow(case), function(x){
      suppressWarnings(
        fisher.test(cbind(c(data$RCP.Hits.Case[x],
                            num.cases-data$RCP.Hits.Case[x]),
                          c(data$RCP.Hits.Ctrl[x],
                            num.ctrls-data$RCP.Hits.Ctrl[x]))))
    })))
    data$Fisher.PValue2 <- p.adjust(as.numeric(ft2$p.value),method = pval_correction)

    data$PValue.Min <-
      apply(data[,c("Fisher.PValue","Fisher.PValue2")],1,min)

  }


  return(data)
}