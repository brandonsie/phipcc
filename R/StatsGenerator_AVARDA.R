#' generate statistics from AVARDA output
#'
#'
#' @export



StatsGenerator_AVARDA <- function(
  AVARDA_case_data, AVARDA_ctrl_data,
  # AVARDA_case_breadth_rcp, AVARDA_ctrl_breadth_rcp,
  seropos_pval = 0.05, # rcp_thresh = 0.95,
  pval_correction = "BH"
){

  output_data <- data.frame(Virus = AVARDA_case_data$seropos$Virus)
  # output_data <- data.frame(Virus = AVARDA_case_data$seropos$Virus %>% strsplit("__") %>% lapply(function(x) {tail(x, 1)}) %>% unlist)


  num.cases <- ncol(AVARDA_case_data$seropos) - 1
  num.ctrls <- ncol(AVARDA_ctrl_data$seropos) - 1

  # Seropositives
  {
    # get hits below 0.05
    AVARDA_case_seropos <- AVARDA_case_data$seropos
    AVARDA_case_seropos[, -1] <- ifelse(AVARDA_case_seropos[,-1] <= seropos_pval, 1, 0)

    AVARDA_ctrl_seropos <- AVARDA_ctrl_data$seropos
    AVARDA_ctrl_seropos[, -1] <- ifelse(AVARDA_ctrl_seropos[,-1] <= seropos_pval, 1, 0)

    output_data$Case.Seropos <- apply(AVARDA_case_seropos[,-1], 1, sum)
    output_data$Ctrl.Seropos <- apply(AVARDA_ctrl_seropos[,-1], 1, sum)

    output_data$Case.Seropos.Freq <- output_data$Case.Seropos / num.cases
    output_data$Ctrl.Seropos.Freq <- output_data$Ctrl.Seropos / num.ctrls

    # then fishers exact
    ft <- data.frame(t(sapply(1:nrow(output_data), function(x){
      fisher.test(cbind(c(output_data$Case.Seropos[x], num.cases - output_data$Case.Seropos[x]),
                        c(output_data$Ctrl.Seropos[x], num.ctrls - output_data$Ctrl.Seropos[x])))
    })))
    output_data$Seropos.Fisher.PVal <- p.adjust(as.numeric(ft$p.value),method = pval_correction)

  }

  # Breadth
  {
    # New Way

    # Take breadth values for seropositives. cases and controls. (!) there may be 0 hits
    AVARDA_case_seropos_breadth <- AVARDA_case_data$breadth
    AVARDA_case_seropos_breadth[, -1][AVARDA_case_seropos[, -1] == 0] <- NA

    AVARDA_ctrl_seropos_breadth <- AVARDA_ctrl_data$breadth
    AVARDA_ctrl_seropos_breadth[, -1][AVARDA_ctrl_seropos[, -1] == 0] <- NA

    output_data$Median.Seropos.Breadth.Case <-
      apply(AVARDA_case_seropos_breadth[,-1], 1, function(x){
        x %>% na.omit %>% median
      })
    output_data$Median.Seropos.Breadth.Ctrl <-
      apply(AVARDA_ctrl_seropos_breadth[,-1], 1, function(x){
        x %>% na.omit %>% median
      })

    #wilcox two sided
    wt <- data.frame(t(sapply(1:nrow(output_data), function(x){
      this_case_base <- AVARDA_case_seropos_breadth[x, -1] %>% as.numeric %>% na.omit
      this_ctrl_base <- AVARDA_ctrl_seropos_breadth[x, -1] %>% as.numeric %>% na.omit


      if(length(this_case_base) >= 3 & length(this_ctrl_base) >= 3){

        # # Old way procuded ties
        # stats::wilcox.test(this_case, this_ctrl, alternative = "two.sided")$p.value

        # Add random value to each control. Repeat 100 times and take average.
        reps <- 100
        pvls <- vector("numeric", reps)
        pvls <- sapply(1:reps, function(x){
          # generate random decimals. rnorm divided by (largest absolute value * 2)
          rand_nums <- rnorm(length(this_case_base) + length(this_ctrl_base))
          max_rand <- rand_nums %>% abs %>% max
          rand_dec <- rand_nums / (max_rand * 2)

          this_case <- this_case_base + rand_dec[1:length(this_case_base)]
          this_ctrl <- this_ctrl_base + rand_dec[((length(this_case_base)+1):length(rand_dec))]
          stats::wilcox.test(this_case, this_ctrl, alternative = "two.sided")$p.value
        })

        return(mean(pvls))



      } else NA




    })))

    output_data$Seropos.Breadth.Wilcox.PVal <- p.adjust(as.numeric(wt), method = pval_correction)

    # Report median hit breadth

    # if hits in both, wilcox two tailed on seropos breadths

    # report this pvalue for breadth. (!) adjust html table


    # # Old Way
    #
    # # RCP
    # # AVARDA_case_breadth_rcp <- RCPGenerator(AVARDA_case_data$breadth, AVARDA_ctrl_data$breadth)
    # # AVARDA_ctrl_breadth_rcp <- RCPGenerator(AVARDA_ctrl_data$breadth, "self")
    #
    # AVARDA_case_breadth_rcp_hits <- AVARDA_case_breadth_rcp
    # AVARDA_case_breadth_rcp_hits[, -1] <- ifelse(AVARDA_case_breadth_rcp_hits[, -1] >= rcp_thresh, 1, 0)
    #
    # AVARDA_ctrl_breadth_rcp_hits <- AVARDA_ctrl_breadth_rcp
    # AVARDA_ctrl_breadth_rcp_hits[, -1] <- ifelse(AVARDA_ctrl_breadth_rcp_hits[, -1] >= rcp_thresh, 1, 0)
    #
    # output_data$Breadth.RCP.Hits.Case <- apply(AVARDA_case_breadth_rcp_hits[,-1], 1, sum)
    # output_data$Breadth.RCP.Hits.Ctrl <- apply(AVARDA_ctrl_breadth_rcp_hits[,-1], 1, sum)
    #
    # output_data$Breadth.RCP.Hits.Case.Freq <- output_data$Breadth.RCP.Hits.Case / num.cases
    # output_data$Breadth.RCP.Hits.Ctrl.Freq <- output_data$Breadth.RCP.Hits.Ctrl / num.ctrls
    #
    #
    # # fishers exact
    # ft2 <- data.frame(t(sapply(1:nrow(output_data), function(x){
    #   fisher.test(cbind(c(output_data$Breadth.RCP.Hits.Case[x], num.cases - output_data$Breadth.RCP.Hits.Case[x]),
    #                     c(output_data$Breadth.RCP.Hits.Ctrl[x], num.ctrls - output_data$Breadth.RCP.Hits.Ctrl[x])))
    # })))
    # output_data$Breadth.Fisher.PVal <- p.adjust(as.numeric(ft2$p.value),method = pval_correction)

  }

  # Median Hit Score
  {
    calcHitMedian <- function(input_scoredata, input_threshdata, thresh, na.value = 1){

      this_data_hits <- input_scoredata[as.numeric(input_threshdata) >= thresh]

      if(length(this_data_hits) == 0){
        return(na.value)
      } else{
        return(median(as.numeric(this_data_hits)))
      }
    }


    for(i in 1:nrow(output_data)){
      output_data$Median.Seropos.PVal.Case[i] <- calcHitMedian(
        AVARDA_case_data$seropos[i,-1], AVARDA_case_seropos[i, -1], 1, na.value = 1
      )
      output_data$Median.Seropos.PVal.Ctrl[i] <- calcHitMedian(
        AVARDA_ctrl_data$seropos[i,-1], AVARDA_ctrl_seropos[i, -1], 1, na.value = 1
      )

      # output_data$Median.Breadth.Hit.Case[i] <- calcHitMedian(
      #   AVARDA_case_data$breadth[i,-1], AVARDA_case_breadth_rcp_hits[i, -1], 1
      # )
      #
      # output_data$Median.Breadth.Hit.Ctrl[i] <- calcHitMedian(
      #   AVARDA_ctrl_data$breadth[i,-1], AVARDA_ctrl_breadth_rcp_hits[i, -1], 1
      # )
    }
  }


  output_data$PVal.Min <-
    apply(output_data[,c("Seropos.Fisher.PVal","Seropos.Breadth.Wilcox.PVal")],
          1, function(x){
            min(na.omit(x))
          })


  return(output_data)
}