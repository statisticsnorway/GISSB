

library(devtools)
library(usethis)
library(roxygen2)
library(testthat)
library(tidyverse)

# Oppstart #
use_mit_license()
use_readme_rmd()
build_readme() # Kjøres etter .rmd-filen er ferdig utfylt


# Legger til hvilke pakker funksjonene er avhengige av #
# usethis::use_pipe()
# usethis::use_package("dplyr", type = "imports")
# usethis::use_package("httr", type = "imports")
# usethis::use_package("sf", type = "imports")
# usethis::use_package("igraph", type = "imports")
# usethis::use_package("tidygraph", type = "imports")
# usethis::use_package("cppRouting", type = "imports")
# usethis::use_package("nabor", type = "imports")
# usethis::use_package("reshape2", type = "imports")
# usethis::use_package("purrr", type = "imports")
# usethis::use_package("tibble", type = "imports")
# usethis::use_package("leaflet", type = "imports")
# usethis::use_package("RCurl", type = "imports")
# usethis::use_package("stringr", type = "imports")
# usethis::use_package("rlang", type = "imports")
# usethis::use_package("here", type = "imports")
# usethis::use_package("tidyselect", type = "imports")

# usethis::use_package("here", type = "imports")

# Fix for "no visible binding for global variable"

# Legger til automatiske tester for funksjonene #

devtools::load_all()

# Kjør bare en gang #

# usethis::use_r("address_to_coord")
# usethis::use_test("address_to_coord")
#
# usethis::use_r("coords_to_google")
# usethis::use_test("coords_to_google")
#
# usethis::use_r("vegnett_to_R")
# usethis::use_test("vegnett_to_R")
#
# usethis::use_r("coords_to_node")
# usethis::use_test("coords_to_node")
#
# usethis::use_r("shortest_path_igraph")
# usethis::use_test("shortest_path_igraph")
#
# usethis::use_r("shortest_path_cppRouting")
# usethis::use_test("shortest_path_cppRouting")
#
# usethis::use_r("path_leaflet")
# usethis::use_test("path_leaflet")

# rm(list = c("beregne_avstand_cpp"))
devtools::document()

devtools::check()

sessionInfo()
# > checking R files for syntax errors ... WARNING
# Warning in Sys.setlocale("LC_CTYPE", oLC_ct) :
#   OS reports request to set locale to "Norwegian BokmC%l_Norway.utf8" cannot be honored

# Kommer denne fra vignetten? Test uten!


# ?
# devtools::detach(package:GISSB, unload=TRUE)
# install.packages("C:/Users/rdn/Documents/Github/GISSB_1.1.tar.gz", repos = NULL, type="source")
# devtools::install_github("statisticsnorway/GISSB", ref = "main")
#
# library(devtools)
# install_github("statisticsnorway/GISSB")

# devtools::install_github

# Lager vignette
# usethis::use_vignette("GISSB_vignette")
usethis::use_pkgdown() # OBS: docs legges i .gitignore (må fjernes) - kjøres kun en gang?

pkgdown::build_site()

# usethis::use_pkgdown_github_pages() # OBS: docs legges i .gitignore (må fjernes før commit)
# pkgdown::build_site_github_pages()
#
# ?pkgdown::build_site_github_pages


# OBS: commit/push!
# GISSB_vignette: ,knit til .md før build_site?

# Lag pakke (?)
devtools::build()

# Installerer pakken lokalt #
devtools::install()



