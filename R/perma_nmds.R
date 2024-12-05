#' @title Execute nMDS and PERMANOVA
#' @description
#' This function execute a PERMANOVA test to asses
#' the significance of the effect of variable on community
#' structure.
#' @param mat_com Community matrix with samples in line
#' and individuals / species in line
#' @param mat_ech Matrix with metadata associated with samples
#' @param var_test A vector containing variables to test description
#' @param hellinger A boolean, if TRUE a Hellinger transformation is applied
#'
#' @return Return a list containing 2 elements, first an nMDS second a dataframe
#' with the PERMANOVA results
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
