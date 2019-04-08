#' Plot hit frequency vs median hit score of cases
#'
#' @param data Filtered hits list.
#'
#' @export

plot1_hitfreq <- function(data, label_thresh = 0.12){

  for(i in 1:length(data)){
    #for each datatype (zscore, promax, etc.)

    this_plot_data <- as.data.frame(
      with(data[[i]],
           cbind(RCP.Hit.Freq.Case, Median.Hit.Score.Case, gene, product,
                 Annotation, Annotations, "ID" = names(data)[i])))

    if(i == 1){
      plot_data <- this_plot_data
    } else{
      plot_data %<>% rbind(this_plot_data)
    }

  }

  # plot

  g <- ggplot(all.plot,
              aes(x = Median.Hit.Score.Case, y = RC.Hit.Freq.Case,
                  gene = gene, product = product, Annotations = Annotations)) +
    geom_jitter(data = all.plot[is.na(all.plot$Annotation),],
                aes(color = Annotation), alpha = 0.5) +
    geom_jitter(data = all.plot[!is.na(all.plot$Annotation),],
                aes(color = Annotation), alpha = 0.5) +
    ggtitle("Hit Frequency vs. Median Score of Case Hits (Jittered)") +
    xlab("Median Case Hit Score") + ylab("Case Hit Frequency") +
    facet_grid(~ID) + theme_bw() +
    theme(axis.text.x = element_text(angle = 0),
          axis.text.y = element_text(angle = 90),
          legend.title = element_blank()) +
    geom_text_repel(data = subset(all.plot, RCP.Hit.Freq.Case > label_thresh),
                    aes(labelo = gene))

  gp <- ggplotly(g,tooltip=c("x","y","Annotations","gene","product"))


  return(list(g, gp))
}