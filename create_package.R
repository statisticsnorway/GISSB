
library(devtools)
library(usethis)
library(roxygen2)
library(testthat)

# Oppstart #
use_mit_license()
use_readme_rmd()
build_readme() # Kjøres etter .rmd-filen er ferdig utfylt


# Legger til hvilke pakker funksjonene er avhengige av #
usethis::use_package("dplyr", type = "imports")
usethis::use_package("httr", type = "imports")
usethis::use_package("sf", type = "imports")
usethis::use_package("igraph", type = "imports")
usethis::use_package("tidygraph", type = "imports")
usethis::use_package("cppRouting", type = "imports")
usethis::use_package("nabor", type = "imports")
usethis::use_package("reshape2", type = "imports")
usethis::use_package("purrr", type = "imports")
usethis::use_package("tibble", type = "imports")
usethis::use_package("leaflet", type = "imports")
usethis::use_package("RCurl", type = "imports")



# Legger til automatiske tester for funksjonene #

devtools::load_all()

usethis::use_r("adresse_api_koord")
usethis::use_test("adresse_api_koord")

usethis::use_r("koords_to_google")
usethis::use_test("koords_to_google")

usethis::use_r("vegnett_to_R")
usethis::use_test("vegnett_to_R")

usethis::use_r("node_koord")
usethis::use_test("node_koord")

usethis::use_r("beregne_avstand")
usethis::use_test("beregne_avstand")

usethis::use_r("beregne_avstand_cpp")
usethis::use_test("beregne_avstand_cpp")

usethis::use_r("path_leaflet")
usethis::use_test("path_leaflet")

# beregne_avstand


# rm(list = c("beregne_avstand_cpp"))
devtools::document()

devtools::check()

# Installerer pakken lokalt #
install()

# Lag pakke
devtools::build()

# devtools::install_github

# Lager vignette
usethis::use_pkgdown()
pkgdown::build_site()

# usethis::use_vignette("GISSB_vignette")
# pkgdown::build_site_github_pages()
usethis::use_pkgdown_github_pages()
