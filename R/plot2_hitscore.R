#' second plot
#'
#' @param data Filtered hits data frame
#' @param data_types List of desired facets in order.
#'
#' @export


plot2_hitscore <- function(data, data_types){

  library(ggplot2)
  data$Data.Type %<>% factor(levels = data_types)

  g <- ggplot(data,
              aes(x = Median.Hit.Score.Ctrl, y = Median.Hit.Score.Case,
                  Protein = Protein, Description = Description, Annotation = Flags)) +
    geom_point(data = data[data$Flags != "NA", ],
               aes(color = Flags), alpha = 0.5) +
    geom_point(data = data[data$Flags == "NA", ],
               aes(color = Flags), alpha = 0.5) +
    facet_wrap(~Data.Type, scales = "free") + theme_bw() +
    #scale_x_log10() + scale_y_log10() +
    ggtitle("Median Score of Hits, Cases vs. Controls") +
    xlab("Median Control Hit Score") + ylab("Median Case Hit Score") +
    theme(axis.text.x = element_text(angle = 0),
          axis.text.y = element_text(angle = 90),
          legend.title = element_blank())


  return(g)
}
