#' @title  The Bag of Little Bootstraps for Logistic Linear Regression Model
#'
#' @import purrr
#' @import stats
#' @import future
#' @import utils
#' @import furrr
#' @importFrom magrittr %>%


#' @param formula the generalized linear regression formula
#' @param data data frame
#' @param parallel whether users want to use parallelization to use more than one CPUs. If you choose to use parallization, run plan(multisession, worker=i) first, where i is the number of CPUs you are willing to use.
#' @param m numeric, split data into m parts of approximated equal sizes
#' @param B numeric, repeat the process B times
#'
#' @export
#'
#' @examples
#' fitaaaa <- blbglm(formula=Sepal.Length~Sepal.Width+Petal.Length,data=iris,m=10,B=100,parallel=TRUE)
#' summary(fitaaaa)
blbglm <- function(formula, data, parallel =TRUE, m = 10, B = 5000) {
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
split_data_glm <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
#'
#' @param formula the generalized linear regression model with Normal distribution
#' @param data data frame
#' @param n numeric, length of the data frame
#' @param B numeric, repeat the process B times
glm_each_subsample <- function(formula, data, n, B) {
  replicate(B, glm_each_boot(formula, data, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#'
#' @param formula the generalized linear regression model
#' @param data data frame
#' @param n numeric, length of the data frame
glm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, data, freqs)
}


#' estimate the regression estimates based on given the number of repetitions
#'
#' @param formula the generalized linear regression model with Normal distribution
#' @param data data frame
#' @param freqs weight
glm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- glm(formula,family = binomial(), data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
#'
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' @param x the fit of the logistic linear regression model of blbglm
#' @param ... Other arguments
#'
#' @export
#' @method print blbglm
print.blbglm <- function(x, ...) {
  cat("blbglm model:", capture.output(x$formula))
  cat("\n")
}


#' @param object the fit of blbglm
#'
#' @param confidence whether users want to use the confidence interval or not
#' @param level level of the confidence, Eg.0.95
#' @param ... Other arguments
#'
#' @export
#' @method sigma blbglm
sigma.blbglm <- function(object, confidence = FALSE, level, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(future_map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - level
    limits <- est %>%
      map_mean(~ quantile(future_map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}


#' @name coef.blbglm
#' @title Computing the coeficients of blbglm model
#' @param object the fit of blbglm
#'
#' @param ... Other arguments
#'
#' @export
#' @method coef blbglm
coef.blbglm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}



#' @name confint.blbglm
#' @title Computing the confidence interval of blbglm model
#' @param object the fit of blbglm
#'
#' @param parm the users can choose whether to use parameters or not
#' @param level level of confidence, Eg.0.95
#' @param ... Other arguments
#'
#' @export
#' @method confint blbglm
confint.blbglm <- function(object, parm = NULL, level, ...) {
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


#' @name predict.blbglm
#' @title Computing the predictions of new data of blbglm model
#' @param object the fit of blbglm
#' @param new_data data frame
#' @param confidence users can choose whether to have a confidence interval or not
#' @param level the level of confidence
#' @param ... Other arguments
#' @export
#' @method predict blbglm

predict.blbglm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
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
  (future_map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  future_map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  future_map(.x, .f, ...) %>% reduce(rbind)
}











