#' Plot hit frequency vs median hit score of cases
#'
#' @param data Filtered hits data frame
#' @param data_types List of desired facets in order.
#'
#' @export

plot1_hitfreq <- function(data, data_types){

  library(ggplot2)
  data$Data.Type %<>% factor(levels = data_types)

  g <- ggplot(data,
              aes(x = Median.Hit.Score.Case, y = RCP.Hit.Freq.Case,
                  Protein = Protein, Description = Description, Annotations = Flags)) +
    geom_jitter(data = data[data$Flags != "NA", ],
                aes(color = Key.Flag), alpha = 0.5) +
    geom_jitter(data = data[data$Flags == "NA", ],
                aes(color = Key.Flag), alpha = 0.5) +
    ggtitle("Hit Frequency vs. Median Score of Case Hits (Jittered)") +
    xlab("Median Case Hit Score") + ylab("Case Hit Frequency") +
    facet_wrap(~Data.Type, scales = "free_x") + theme_bw() +
    theme(axis.text.x = element_text(angle = 0),
          axis.text.y = element_text(angle = 90),
          legend.title = element_blank())

  return(g)

}