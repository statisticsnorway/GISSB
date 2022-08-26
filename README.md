
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GISSB - Network analysis in R

<!-- badges: start -->
<!-- badges: end -->

`GISSB` is a package that contains GIS functions in R for use in
Statistics Norway.

See the vignette for an introduction of how the functions can be used
for network analysis in R:
<https://statisticsnorway.github.io/GISSB/articles/GISSB_vignette.html>

## Installation

You can install the development version of GISSB like so:

``` r
devtools::install_github("statisticsnorway/GISSB")
```

## Requirements

Most of the functions in the GISSB package require that the Norwegian
road network [NVDB Ruteplan
nettverksdatasett](https://kartkatalog.geonorge.no/metadata/nvdb-ruteplan-nettverksdatasett/8d0f9066-34f9-4423-be12-8e8523089313)
has been downloaded locally. The format of the file should be “FGDB
10.0”.

After the file has been downloaded it has to be read into R using the sf
package:

``` r
vegnett <- sf::read_sf("vegnettRuteplan_FGDB_20210528.gdb")
```
