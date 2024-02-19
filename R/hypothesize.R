#' Hypothesis test structure
#'
#' @param stat test statistic
#' @param p.value p-value
#' @param dof degrees of freedom
#' @return hypothesis test
#' @export
#' @examples
#' # create a hypothesis test
#' test <- hypothesis_test(stat = 1.96, p.value = 0.05, dof = 1)
#' # print the test
#' test
#' # extract the p-value
#' pval(test)
#' # extract the degrees of freedom
#' dof(test)
#' # extract the test statistic
#' stat(test)
#' # check if the test is significant at the 5% level
#' is_significant_at(test, 0.05)
#' @importFrom stats pf pchisq qt qnorm
#' @export
hypothesis_test <- function(stat, p.value, dof, superclasses = NULL, ...) {
  res <- list(stat = stat, p.value = p.value, dof = dof, ...)
  class(res) <- unique(c(superclasses, "hypothesis_test"))
  res
}

#' Generic method for extracting the p-value from a hypothesis test
#' @param x a hypothesis test object
#' @param ... additional arguments to pass into the method
#' @return p-value
#' @export
pval <- function(x, ...) {
  UseMethod("pval")
}

#' p-value method for hypothesis tests
#'
#' @param x a hypothesis test
#' @param ... additional arguments
#' @return p-value
#' @export
pval.hypothesis_test <- function(x, ...) {
  x$p.value
}

#' Generic method for extracting the degrees of freedom from a hypothesis test
#' @param x a hypothesis test object
#' @param ... additional arguments to pass into the method
#' @return degrees of freedom
#' @export
dof <- function(x, ...) {
  UseMethod("dof")
}

#' Degrees of freedom method for hypothesis tests
#' @param x a hypothesis test
#' @param ... additional arguments
#' @return degrees of freedom
#' @export
dof.hypothesis_test <- function(x, ...) {
  x$dof
}

#' Generic method for extracting the test statistic from a hypothesis test
#' @param x a hypothesis test object
#' @param ... additional arguments to pass into the method
#' @return test statistic
#' @export
test_stat <- function(x, ...) {
  UseMethod("stat")
}

#' Test statistic method for hypothesis tests
#' @param x a hypothesis test
#' @param ... additional arguments
#' @return test statistic
#' @export
test_stat.hypothesis_test <- function(x, ...) {
  x$stat
}


#' Generic method for checking if a hypothesis test is significant at a given
#' significance level.
#' @param x a hypothesis test object
#' @param alpha significance level
#' @return logical indicating whether the test is significant at the given
#' significance level alpha (e.g., 0.05) or not.
#' @export
is_significant_at <- function(x, alpha, ...) {
  UseMethod("is_significant_at")
}

#' Significance test for the hypothesis_test class.
#' 
#' @param x a hypothesis test object
#' @param alpha significance level
#' @return logical indicating whether the test is significant at the given
#' significance level alpha (default is 0.05) or not.
#' @export
is_significant_at.hypothesis_test <- function(x, alpha, ...) {
  pval(x) < alpha
}

#' Likelihood ratio test. This function computes the likelihood ratio test
#' statistic and p-value.
#' 
#' Assumptions:
#' (1) The null model is nested within the alternative model.
#' (2) The likelihood values are computed from the same data set.
#' 
#' The degrees of freedom are conceptualized in the context of vector spaces.
#' Thus, by (1), the parameter space of the null model is a subspace of the
#' parameter space of the alternative model, both of which are vector spaces.
#' The degrees of freedom is the difference in the dimension of the null model
#' and the alternative model. Normally, this boils down to the difference in
#' the number of parameters between the two models, e.g., fixing some of the
#' components in the null model to some constant value.
#' 
#' The likelihood ratio test statistic is defined as:
#' 
#'   LRT = -2 (null_loglik - alt_loglik)
#' 
#' which, under the null hypothesis, is asymptotically chi-squared distributed
#' with degrees of freedom equal to the dof previously defined. Thus, we can
#' ask whether the observed likelihood ratio test statistic is significantly
#' different from the expected value under the null hypothesis. The p-value is
#' the probability of observing an LRT statistic as extreme as the one observed
#' under the null hypothesis.
#' 
#' @param null_loglik the likelihood value from a simpler likelihood model nested
#'             within the alternative model
#' @param alt_loglik the likelihood value from the more complicated model
#' @param dof degrees of freedom
#' @return likelihood ratio test
#' @examples
#' # create a likelihood ratio test
#' lrt <- lrt_from_loglik(null_loglik = -100, alt_loglik = -90, dof = 1)
#' # print the test
#' test_stat(lrt)
#' pval(lrt)
#' dof(lrt)
#' # check if the test is significant at the 5% level
#' is_significant_at(lrt, 0.05)
#' 
#' @importFrom stats pchisq
#' @export
lrt_from_loglik <- function(null_loglik, alt_loglik, dof) {
  stat <- -2 * null_loglik - alt_loglik
  p.value <- pchisq(stat, df = dof, lower.tail = FALSE)
  hypothesis_test(stat = stat, p.value = p.value, dof = dof,
                  superclasses = c("likelihood_ratio_test"))
}
