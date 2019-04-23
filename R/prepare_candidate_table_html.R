#' Add kable html stying to candidate table.
#'
#' @param data output from prepare_candidate_table.
#'
#' @export

prepare_candidate_table_html <- function(data){

  names(data)[5:8] <- c("Case", "Ctrl", "Case", "Ctrl")
  options(knitr.table.format = "html")


  data[,4:5] <- data[,4:5] %>% dplyr::mutate_if(is.numeric, function(x){
    kableExtra::cell_spec(
      x, bold = TRUE,
      color = kableExtra::spec_color(x, end=0.9, scale_from = c(0, 1)),
      font_size = kableExtra::spec_font_size(x, begin = 12, end = 15))
  })


  data[,7:8] <- data[,7:8] %>% dplyr::mutate_if(is.numeric, function(x){
    kableExtra::cell_spec(
      x, bold=TRUE,
      color=ifelse(x>50, "#B4DE2CFF",
        kableExtra::spec_color(x, end=0.9,
                               scale_from = c(min(data[,7:8]),50))),
      font_size=ifelse(x > 50, 15,
        kableExtra::spec_font_size(x, begin = 12, end = 15,
                                   scale_from=c(min(data[,7:8]),50))))
  })

  #(!) add this to markdown report
  #(!) kable is knitr. specify other functions too
  # data %>%
  #   knitr::kable(format="html",escape=FALSE,caption="All Filtered Hits",row.names=FALSE) %>%
  #   kable_styling() %>%
  #   add_header_above(c(" " = 4, "Hit Freq" = 2,"Median Hit Score" = 2," " = 1)) %>%
  #   column_spec(1,border_left = "1px solid gray") %>%
  #   column_spec(ncol(data),border_right="1px solid gray") %>%
  #   column_spec(c(1:4,6,8),border_right="1px solid gray")  %>%
  #   scroll_box(height = "500px")

  return(data)

}


#(!) update to have better ID.,etc.
# goal columns:
# [ ] Protein (formerly gene): <gene> | <TileA>, <TileB>
# [x] Description (formerly product)
# [x] Annotations (Flags, in order)
# [ ] Hit categories (Data.Type)
# [~] Hit Frequency, Case, Ctrl (still need to format header)
# [~] Median Hit Score, Case, Ctrl (still need to format header)
# [ ] PValue (Pvalue.Min. collapse to smallest for that protein)

# (!) Collapse to protein. Gene|Tile,Tile names. Hit categories. Min Pvalue.
# (!) Kable HTML formatting. headers, conditional color/size of text, etc.

# (!!) Need to handle Promax/Polyclonal a little differently? if Peptide is NA.
