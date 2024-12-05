#' @title Represent nMDS with facets
#' @description
#' This function create a graphical output for a nMDS with facets
#' @param out_perma_nmds the output of the `perma_nmds()` function
#' @param mat_ech a dataframe or matrix, the matrix specifying the samples groups
#' with modalities in columns and samples in rows
#' @param var_facet a vector containing 2 variables to make a facet grid
#' @param var_col a group variable to color nMDS points
#'
#' @return Return an nMDS plot from the `ggplot` package
#'
#' @import ggplot2
#' @import vegan
#' @export
#'

nice_facet <- function(out_perma_nmds,
                       mat_ech,
                       var_facet,
                       var_col){

  scores_nmds <- data.frame(vegan::scores(out_perma_nmds[[1]])$sites)

  form <- as.formula(paste0(var_facet[1],"~",var_facet[2]))
  # print(head(cbind(scores_nmds,mat_ech)))
  # print(mat_ech$Year_class)
  plot <- cbind(scores_nmds,mat_ech) %>%
    ggplot2::ggplot(ggplot2::aes(x=NMDS1,y=NMDS2,color=get(var_col))) +
    ggplot2::geom_point() +
    ggplot2::facet_grid((form)) +
    ggplot2::labs(color=var_col)

  return(plot)
}
