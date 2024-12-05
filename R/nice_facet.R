#' @title Represent PERMANOVA with facets
#' @description
#' This function create a graphical output for a nMDS with a facet
#' @param out_perma_nmds Community matrix with samples in line
#' and individuals / species in line
#' @param mat_ech Matrix with metadata associated with samples
#' @param var_facet A vector containing 2 variables to make a facet
#' @param var_col The variable you want to color the samples with
#'
#' @return Return a nMDS plot
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
