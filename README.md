gasper
======

[![Travis build
status](https://travis-ci.org/fabnavarro/gasper.svg?branch=master)](https://travis-ci.org/fabnavarro/gasper)

Graph signal processing in R.
![](README_files/figure-markdown_github/unnamed-chunk-1-1.png)

Download and Install
--------------------

Install the devtools package if you haven’t already.

``` r
install.packages("devtools")
```

To install the development package, type the following at the R command
line:

``` r
devtools::install_github("fabnavarro/gasper")
library(gasper)
```

To obtain the complete list of package functions, simply type

``` r
help(package = "gasper")
```

Getting Started
---------------

See the [package
vignette](http://fnavarro.perso.math.cnrs.fr/rpackage/gasper_vignette.pdf)
for more details. You could also build and see the vignette associated
with the package using the following lines of code

``` r
devtools::install_github("fabnavarro/gasper", build_vignettes = TRUE)
library(gasper)
```

Then, to view the vignette

``` r
vignette("gasper_vignette")
```

For an illustration of the features of the package, you can also refer
to the following repo
[SGWT-SURE](https://github.com/fabnavarro/SGWT-SURE) which provides an
effective generalization of the Stein Unbiased Risk Estimate (SURE) for
signal denoising/regression on graphs using Spectral Graph Wavelet
Transform.
