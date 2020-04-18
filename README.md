
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmgarbage: automatic removal of garbage strings in OCR text

<!-- badges: start -->

<!-- badges: end -->

The goal of rmgarbage is to remove strings obtained from OCR engines
which are garbage. It contains functions that implement the methods
described by:

  - Taghva et al. 2001 “Automatic Removal of Garbage Strings in OCR
    Text: An implementation”
    <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.81.8901>  
  - Yang Cai (2008) “OCR Output Enhancement”
    <https://ladyissy.github.io/OCR/>

The code was inspired by Python code at
<https://github.com/foodoh/rmgarbage> and JavaScript code at
<https://github.com/Amoki/rmgarbage>.

## Installation

You can install rmgarbage from GitHub with:

``` r
remotes::install_github("benmarwick/rmgarbage")
```

## Example

This is a basic example which shows you how to solve the problem of
identifing bad OCR.

``` r
library(rmgarbage)
## basic example code
```

Here is an example of output on a good ocr:

``` r
good_ocr <- "This document was scanned perfectly"
good_ocr_split <- strsplit(good_ocr, " ")[[1]]
sapply(good_ocr_split, rmgarbage)
#>      This  document       was   scanned perfectly 
#>     FALSE     FALSE     FALSE     FALSE     FALSE
```

And here is an example of output on a bad ocr:

``` r
bad_ocr <- "This 3ccm@nt w&s scnnnnd not pe&;c1!y"
bad_ocr_ocr_split <- strsplit(bad_ocr, " ")[[1]]
sapply(bad_ocr_ocr_split, rmgarbage)
#>     This  3ccm@nt      w&s  scnnnnd      not pe&;c1!y 
#>    FALSE     TRUE     TRUE     TRUE    FALSE     TRUE
```

# Contributing
