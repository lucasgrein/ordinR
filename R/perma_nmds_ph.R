#' Post_hoc test for PERMANOVA
#'
#' This function computes pairwise PERMANOVAs for each couple of groups
#'
#' @param mat_com a data frame or matrix, the community matrix with species in columns and samples in rows.
#' @param mat_ech a data frame or matrix, the matrix specifying the sample groups with modalities in columns and samples in rows.
#' @param var_test a character, the name of the modality to test.
#'
#' @return A data frame containing r2 and p values for each group couple
#'
#' @importFrom pairwiseAdonis pairwise.adonis2
#'
#' @export

perma_nmds_ph <- function(mat_com, mat_ech, var_test){

  var_non_test <- colnames(mat_ech)[colnames(mat_ech)!=var_test]

  form <- paste( "mat_com ~", var_test)
  form <- as.formula( paste( c(form, var_non_test), collapse=" + " ) )

  perma <- pairwiseAdonis::pairwise.adonis2(x=form, data=mat_ech, permutations=1000, method="bray")

  output <- data.frame("effect"=var_test, "level_vs_level"=c( names(perma)[-1] ),"r2"=NA, "p_value"=NA)

  for(i in 2:length(perma) ){
    output$r2[ output$level_vs_level==names(perma)[i] ] <- perma[[i]]$R2[1]
    output$p_value[ output$level_vs_level==names(perma)[i] ] <- perma[[i]]$`Pr(>F)`[1]
  }

  return(output)

}
