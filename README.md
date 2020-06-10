
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blblm

<!-- badges: start -->

<!-- badges: end -->

The goal of blblm is to implement both the linear regression and
logistic linear regression with little bag of bootstraps by using
blblm(formula,data,parallel,m,B)

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

## For linear regression Model:

This is a basic example which shows you how to solve a common problem
via `blblm`:

Fiting a linear regression model in the bag of little bootstraps via
`blblm`:

``` r
library(blblm)
fit <- blblm(formula=Sepal.Length~Petal.Length+Petal.Width,data=iris,m=10,B=100,parallel=TRUE)
summary(fit)
#>           Length Class   Mode
#> estimates 10     -none-  list
#> formula    3     formula call
```

Computting the coefficient of the blblm : `coef.blblm(fit)`

Getting the confidence interval at the level alpha=0.05 of blblm:
`confint.blblm(fit,parm = NULL, level = 0.95)`

Computing the predict model for the blblm:
`predict.blblm(fit,data.frame(Petal.Length=c(3.0,5.0),Petal.Width=c(0.1,1.1)),confidence=TRUE,level=0.95)`

## For Logistic linear regression Model:

Similarly, Here is another example which shows you how to solve a common
problem via `blbglm`:

Fiting a linear regression model in the bag of little bootstraps via
`blbglm`:

``` r
library(blblm)
fita <- blbglm(formula=Sepal.Length~Petal.Length+Petal.Width,data=iris,m=10,B=100,parallel=TRUE)
summary(fita)
#>           Length Class   Mode
#> estimates 10     -none-  list
#> formula    3     formula call
```

``` r
# coef.blbglm(fita)
# confint.blbglm(fita,parm = NULL, level = 0.95)
# predict.blbglm(fita,data.frame(Petal.Length=c(3.0,5.0),Petal.Width=c(0.1,1.1)),confidence=TRUE,level=0.95)
```
