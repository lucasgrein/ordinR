usethis::use_r("perma_nmds")
usethis::use_r("perma_nmds_ph")
usethis::use_r("nice_ellipse")
usethis::use_r("nice_facet")
usethis::use_r("nice_wrap")

usethis::use_package("ggplot2", type = "Depends")
usethis::use_package("magrittr", type = "Depends")
usethis::use_package("rlang", type = "Depends")
usethis::use_package("grDevices", type = "Depends")
usethis::use_package("graphics", type = "Depends")
usethis::use_package("stats", type = "Depends")
usethis::use_package("vegan", type = "Imports")
usethis::use_dev_package("pairwiseAdonis", type = "Imports",
                         remote = "pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

devtools::document()

devtools::check()

install_github("lucasgrein/ordinR")
