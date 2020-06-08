
#' @title  The Bag of Little Bootstraps for Linear Regression Model
#' @details
#' This package implement the linear regression with little bag of bootstraps by using blblm(formula,data,parallel,m,B)
#' In this package, there are also corresponding functions from blblm, such as `coef.blblm`, `sigma.blblm`,`confint.blblm`and `predict.blblm`
#' @import purrr
#' @import stats
#' @import furrr
#' @import future
#' @import utils
#' @importFrom magrittr %>%
#'
#'
"_PACKAGE"


utils::globalVariables(c("."))


#' @param formula the linear regression formula
#'
#' @param data data frame
#' @param parallel whether users want to use parallelization to use more than one CPUs. If you choose to use parallization, run plan(multisession, worker=i) first, where i is the number of CPUs you are willing to use.
#' @param m numeric, split data into m parts of approximated equal sizes
#' @param B numeric, repeat the process B times
#'
#' @export
#' @examples
#' fit444 <- blblm(formula=Sepal.Length~Sepal.Width+Petal.Length,data=iris,m=10,B=100,parallel=TRUE)
#' confint.blblm(fit444,c("Sepal.Width,Petal.Length"),level = 0.95,parm=NULL)
#' predict.blblm(fit444, data.frame(Sepal.Width=c(3.0,3.9),Petal.Length=c(1.1,2.2)),confidence=TRUE,level=0.95)

blblm <- function(formula, data, parallel =TRUE, m = 10, B = 5000) {
  data_list <- split_data(data, m)

  if (parallel == TRUE){
    estimates <- future_map(data_list,
                            ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  } else {
    estimates <- map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  }

  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
#'
#' @param data data frame
#' @param m numeric, split data into m parts of approximated equal sizes
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
#'
#' @param formula the linear regression model
#' @param data data frame
#' @param n numeric, length of the data frame
#' @param B numeric, repeat the process B times
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#'
#' @param formula the linear regression model
#' @param data data frame
#' @param n numeric, length of the data frame
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs)
}


#' estimate the regression estimates based on given the number of repetitions
#'
#' @param formula the linear regression model
#' @param data data frame
#' @param freqs weight
lm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
#' @export
#' @param fit The linear regression model
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
#'
#' @export
#' @param fit The linear regression model
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' @name print.blblm
#' @title Print the blblm Formula
#' @param x the fit of the linear regression model of blblm
#'
#' @param ... Other arguments
#'
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' @name sigma.blblm
#' @title Computing the sigma of the blblm
#' @param object the fit of blblm
#'
#' @param confidence whether users want to use the confidence interval or not
#' @param level level of the confidence, Eg.0.95
#' @param ... Other arguments
#'
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' @name coef.blblm
#' @title Computing the coeficients of blblm
#' @param object the fit of blblm
#'
#' @param ... Other arguments
#'
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' @name confint.blblm
#' @title Computing the confidence interval of blblm
#' @param object the fit of blblm
#'
#' @param parm the users can choose whether to use parameters or not
#' @param level level of confidence, Eg.0.95
#' @param ... Other arguments
#'
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' @name predict.blblm
#' @title Computing the predictions of new data of blblm
#'
#' @param object the fit of blblm
#'
#' @param new_data data frame
#' @param confidence users can choose whether to have a confidence interval or not
#' @param level the level of confidence
#' @param ... Other arguments
#'
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}

