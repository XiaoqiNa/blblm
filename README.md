
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blblm

<!-- badges: start -->

<!-- badges: end -->

The goal of blblm is to implement the linear regression with little bag
of bootstraps by using blblm(formula,data,parallel,m,B)

## Installation

You can install the released version of blblm from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("blblm")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("XiaoqiNa/blblm")
```

## Examples

This is a basic example which shows you how to solve a common problem
via `blblm`:

Fiting a linear regression model in the bag of little bootstraps via
`blblm`:

``` r
library(blblm)
fit <- blblm(formula=Sepal.Length~Petal.Length+Petal.Width,data=iris,m=10,B=100,parallel=TRUE)
```

Computting the coefficient of the blblm via `coef.blblm`:

``` r
coef.blblm(fit)
#>  (Intercept) Petal.Length  Petal.Width 
#>    4.1696409    0.5776257   -0.4187538
```

Getting the confidence interval at the level alpha=0.05 of blblm via
`confint.blblm`:

``` r
confint.blblm(fit,parm = NULL, level = 0.95)
#>                    2.5%      97.5%
#> Petal.Length  0.4545887  0.7071433
#> Petal.Width  -0.7034067 -0.1467767
```

Computing the predict model for the blblm via `predict.blblm`:

``` r
predict.blblm(fit,data.frame(Petal.Length=c(3.0,5.0),Petal.Width=c(0.1,1.1)),confidence=TRUE,level=0.95)
#>        fit      lwr      upr
#> 1 5.860643 5.634145 6.094243
#> 2 6.597140 6.408020 6.792847
```
