#' generates clustergram of hits
#'
#' @param clustergram_rawdata output from prepare_clustergram_data or prepare_epitope_clustergram_data
#' @param ncolors Number of colors to show in clustergram
#'
#' @export


generate_clustergram <- function(clustergram_rawdata, ncolors = 2){
  my_palette <- colorRampPalette(c("white","black"))(n = ncolors)

  if(!is.null(dev.list())) dev.off()
  a <- gplots::heatmap.2(
    clustergram_rawdata,
    key = FALSE, keysize = 0.5, key.title = "RCP>=0.95",
    key.xlab = NA,
    key.ylab = NA,
    #labCol = FALSE, #remove col labels (temp?)
    #labRow = FALSE, #remove row labels (temp?)
    key.xtickfun = function() {
      breaks = pretty(parent.frame()$breaks)
      breaks = breaks[c(1,length(breaks))]
      list(at = parent.frame()$scale01(breaks),labels = breaks)
    },
    col = my_palette,
    margins = c(3, 10),
    trace = "both",
    tracecol = "grey",
    hline = 0,
    vline = 0,
    cexRow = 0.5,
    cexCol = 0.5,
    hclustfun = function(x) hclust(x, method = "ward.D2"),
    distfun = function(x) dist(x, method = "euclidean")^2
  )
  if(!is.null(dev.list())) dev.off()

  #store sorted heatmap
  clustergram_sorted <- clustergram_rawdata[rev(a$rowInd), a$colInd]

  b <- NA
  # library(heatmaply)
  # b <- heatmaply(
  #   clustergram_sorted,
  #   main = paste("Candidate Antigen Clustergram"),
  #   colors = my_palette,
  #   fontsize_row = 8,
  #   fontsize_col = 8,
  #   column_text_angle = 90,
  #   margins = c(65, 120),
  #   trace = "both",
  #   tracecol = "grey",
  #   Rowv = FALSE,
  #   Colv = FALSE,
  #   grid_gap = 1,
  #   hide_colorbar = TRUE) %>%
  #   layout(height = 800, width = 900)
  # if(!is.null(dev.list())) dev.off()


  return(list(a, b, clustergram_sorted))
}