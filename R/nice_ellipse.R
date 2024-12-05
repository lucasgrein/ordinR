nice_ellipse <- function(out_perma_nmds, mat_ech, var_test){

  nmds <- out_perma_nmds[[1]]
  score_nmds <- scores(nmds)$sites
  score_nmds <- as.data.frame(score_nmds)
  score_nmds <- cbind(score_nmds, mat_ech)

  plot.new()
  ord <- ordiellipse(ord=nmds,groups=score_nmds[, var_test],display="sites",kind="se",conf=0.95,label=TRUE)

  df_ell <- data.frame()
  for(g in levels(score_nmds[, var_test])){
    df_inter <- as.data.frame(with(score_nmds[score_nmds[, var_test]==g,],
                                   vegan:::veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,ord[[g]]$scale)))
    df_inter[, var_test] <- g
    df_ell <- rbind(df_ell, df_inter)
  }

  plot_phyt_site <- ggplot(data=score_nmds, aes(NMDS1, NMDS2)) +
    geom_point(aes(color=get(var_test)), alpha=0.5) +
    geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2, colour=get(var_test)), linewidth=1)

  dev.off()

  return(plot_phyt_site)

}
