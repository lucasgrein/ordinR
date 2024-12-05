perma_nmds_ph <- function(mat_com, mat_ech, var_test){

  var_non_test <- colnames(mat_ech)[colnames(mat_ech)!=var_test]

  form <- paste( "mat_com ~", var_test)
  form <- as.formula( paste( c(form, var_non_test), collapse=" + " ) )

  perma <- pairwise.adonis2(x=form, data=mat_ech, permutations=1000, method="bray")

  output <- data.frame("effect"=var_test, "level_vs_level"=c( names(perma)[-1] ),"r2"=NA, "p_value"=NA)

  for(i in 2:length(perma) ){
    output$r2[ output$level_vs_level==names(perma)[i] ] <- perma[[i]]$R2[1]
    output$p_value[ output$level_vs_level==names(perma)[i] ] <- perma[[i]]$`Pr(>F)`[1]
  }

  return(output)

}
