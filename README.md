# piamModelTests Tools

R package **piamModelTests**, version **0.31.5**

[![CRAN status](https://www.r-pkg.org/badges/version/piamModelTests)](https://cran.r-project.org/package=piamModelTests)  [![R build status](https://github.com/pik-piam/piamModelTests/workflows/check/badge.svg)](https://github.com/pik-piam/piamModelTests/actions) [![codecov](https://codecov.io/gh/pik-piam/piamModelTests/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/piamModelTests) [![r-universe](https://pik-piam.r-universe.dev/badges/piamModelTests)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

A collection of R tools provided by the Integrated Assessment Modeling Consortium (IAMC) for data analysis and diagnostics. 


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("piamModelTests")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("iamc") # Using and adding IAMC data checks
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **piamModelTests** in publications use:

Dietrich J, Auer C, Giannousakis A, Bertram C, Benke F, Humpenoeder F, Baumstark L (2023). _piamModelTests: piamModelTests Tools_. R package version 0.31.5.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {piamModelTests: piamModelTests Tools},
  author = {Jan Philipp Dietrich and Cornelia Auer and Anastasis Giannousakis and Christoph Bertram and Falk Benke and Florian Humpenoeder and Lavinia Baumstark},
  year = {2023},
  note = {R package version 0.31.5},
}
```
