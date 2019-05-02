#' third plot
#'
#' @param data Filtered hits data frame
#' @param data_types List of desired facets in order.
#'
#' @export


plot3_pval <- function(data, data_types){

  library(ggplot2)
  data$Data.Type %<>% factor(levels = data_types)

  g <- ggplot(data,
              aes(x = Median.Hit.Score.Case, y = -log10(PValue.Min),
                  Protein = Protein, Description = Description, Annotations = Flags)) +
    geom_jitter(data = data[data$Flags != "NA", ],
                aes(color = Key.Flag), alpha = 0.5) +
    geom_jitter(data = data[data$Flags == "NA", ],
                aes(color = Key.Flag), alpha = 0.5) +
    ggtitle("Minimum Fisher P-Value vs. Median Case Hit Score (Jittered)") +
    xlab("Median Case Hit Score") + ylab("-log10(p_BH)") +
    facet_wrap(~Data.Type, scales = "free_x") + theme_bw() +
    theme(axis.text.x = element_text(angle = 0),
          axis.text.y = element_text(angle = 90),
          legend.title = element_blank())

  return(g)
}