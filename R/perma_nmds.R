#' @title Execute nMDS and PERMANOVA
#' @description
#' This function execute a PERMANOVA test to asses
#' the significance of the effect of variable on community
#' structure. It also performs an nMDS using the Bray-Curtis dissimilarity measure.
#' @param mat_com a dataframe or matrix, the community matrix with species
#'  in columns and samples in rows
#' @param mat_ech a dataframe or matrix, the matrix specifying the samples groups
#' with modalities in columns and samples in rows
#' @param var_test a vector containing variables to test
#' @param hellinger A boolean, if TRUE a Hellinger transformation is applied
#'
#' @return Return a list containing 2 elements, first an nMDS from the `vegan`
#' package, second a dataframe r2 and pvalue for each variable tested
#' @import vegan
#' @export

perma_nmds <- function(mat_com,
                       mat_ech,
                       var_test,
                       hellinger){
  if(hellinger == T){
    mat_com <- vegan::decostand(mat_com, method= "hellinger")
  }
  nmds <- vegan::metaMDS(mat_com,
                         distance= "bray",
                         autotransformation=F )
  form <- paste0("mat_com~",paste0(var_test,collapse="+"))
  perma <- vegan::adonis2(as.formula(form), data=mat_ech, by ="terms")

  output <- data.frame("effect"= rownames(perma),
                       "r2" = perma$R2,
                       "p_value" = perma$`Pr(>F)`[[1]])

  return(list(nmds,output))
}
