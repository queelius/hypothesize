#' @keywords internal
"_PACKAGE"

#' Create a Hypothesis Test Object
#'
#' Constructs a hypothesis test object that implements the `hypothesize` API.
#' This is the base constructor used by specific test functions like [lrt()],
#' [wald_test()], and [z_test()].
#'
#' @details
#' The `hypothesis_test` object is the fundamental data abstraction in this
#' package. It represents the result of a statistical hypothesis test and
#' provides a consistent interface for extracting results.
#'
#' This design follows the principle of **data abstraction**: the internal
#' representation (a list) is hidden behind accessor functions ([pval()],
#' [test_stat()], [dof()], [is_significant_at()]).
#'
#' @section Extending the Package:
#' To create a new type of hypothesis test:
#' \enumerate{
#'   \item Create a constructor function that computes the test statistic
#'     and p-value.
#'   \item Call `hypothesis_test()` with appropriate `superclasses`.
#'   \item The new test automatically inherits all generic methods.
#' }
#'
#' Example:
#' ```r
#' my_test <- function(data, null_value) {
#'   stat <- compute_statistic(data, null_value)
#'   p.value <- compute_pvalue(stat)
#'   hypothesis_test(
#'     stat = stat, p.value = p.value, dof = length(data) - 1,
#'     superclasses = "my_test",
#'     data = data, null_value = null_value
#'   )
#' }
#' ```
#'
#' @param stat Numeric. The test statistic.
#' @param p.value Numeric. The p-value (probability of observing a test
#'   statistic as extreme as `stat` under the null hypothesis).
#' @param dof Numeric. Degrees of freedom. Use `Inf` for tests based on
#'   the normal distribution.
#' @param superclasses Character vector. Additional S3 classes to prepend,
#'   creating a subclass of `hypothesis_test`.
#' @param ... Additional named arguments stored in the object for
#'   introspection (e.g., input data, null hypothesis value).
#'
#' @return An S3 object of class `hypothesis_test` (and any `superclasses`),
#'   which is a list containing at least `stat`, `p.value`, `dof`, plus
#'   any additional arguments passed via `...`.
#'
#' @examples
#' # Direct construction (usually use specific constructors instead)
#' test <- hypothesis_test(stat = 1.96, p.value = 0.05, dof = 1)
#' test
#'
#' # Extract components using the API
#' pval(test)
#' test_stat(test)
#' dof(test)
#' is_significant_at(test, 0.05)
#'
#' # Create a custom test type
#' custom <- hypothesis_test(
#'   stat = 2.5, p.value = 0.01, dof = 10,
#'   superclasses = "custom_test",
#'   method = "bootstrap", n_replicates = 1000
#' )
#' class(custom)  # c("custom_test", "hypothesis_test")
#' custom$method  # "bootstrap"
#'
#' @seealso [lrt()], [wald_test()], [z_test()] for specific test constructors;
#'   [pval()], [test_stat()], [dof()], [is_significant_at()] for accessors
#' @importFrom stats pchisq qnorm
#' @export
hypothesis_test <- function(stat, p.value, dof, superclasses = NULL, ...) {
  res <- list(stat = stat, p.value = p.value, dof = dof, ...)
  class(res) <- unique(c(superclasses, "hypothesis_test"))
  res
}

#' Print method for hypothesis tests
#' @param x a hypothesis test
#' @param ... additional arguments
#' @return Returns `x` invisibly.
#' @examples
#' w <- wald_test(estimate = 2.5, se = 0.8)
#' print(w)
#' @export
print.hypothesis_test <- function(x, ...) {
  cat(sprintf("Hypothesis test (%s)\n", class(x)[1]))
  cat("-----------------------------\n")
  cat(sprintf("Test statistic: %s\n", test_stat(x)))
  cat(sprintf("P-value: %s\n", pval(x)))
  cat(sprintf("Degrees of freedom: %s\n", dof(x)))
  cat(sprintf("Significant at 5%% level: %s\n", is_significant_at(x, 0.05)))
  invisible(x)
}

#' Extract the p-value from a hypothesis test
#' @param x a hypothesis test object
#' @param ... additional arguments to pass into the method
#' @return Numeric p-value.
#' @examples
#' w <- wald_test(estimate = 2.5, se = 0.8)
#' pval(w)
#' @export
pval <- function(x, ...) {
  UseMethod("pval")
}

#' @rdname pval
#' @export
pval.hypothesis_test <- function(x, ...) {
  x$p.value
}

#' Extract the degrees of freedom from a hypothesis test
#' @param x a hypothesis test object
#' @param ... additional arguments to pass into the method
#' @return Numeric degrees of freedom.
#' @examples
#' w <- wald_test(estimate = 2.5, se = 0.8)
#' dof(w)
#' @export
dof <- function(x, ...) {
  UseMethod("dof")
}

#' @rdname dof
#' @export
dof.hypothesis_test <- function(x, ...) {
  x$dof
}

#' Extract the test statistic from a hypothesis test
#' @param x a hypothesis test object
#' @param ... additional arguments to pass into the method
#' @return Numeric test statistic.
#' @examples
#' w <- wald_test(estimate = 2.5, se = 0.8)
#' test_stat(w)
#' @export
test_stat <- function(x, ...) {
  UseMethod("test_stat")
}

#' @rdname test_stat
#' @export
test_stat.hypothesis_test <- function(x, ...) {
  x$stat
}


#' Check if a hypothesis test is significant at a given level
#' @param x a hypothesis test object
#' @param alpha significance level
#' @param ... additional arguments passed to methods
#' @return Logical indicating whether the test is significant at level
#'   `alpha`.
#' @examples
#' w <- wald_test(estimate = 2.5, se = 0.8)
#' is_significant_at(w, 0.05)
#' @export
is_significant_at <- function(x, alpha, ...) {
  UseMethod("is_significant_at")
}

#' @rdname is_significant_at
#' @export
is_significant_at.hypothesis_test <- function(x, alpha, ...) {
  pval(x) < alpha
}

#' Likelihood Ratio Test
#'
#' Computes the likelihood ratio test (LRT) statistic and p-value for
#' comparing nested models.
#'
#' @details
#' The likelihood ratio test is a fundamental method for comparing nested
#' statistical models. Given a null model \eqn{M_0} (simpler, fewer parameters)
#' nested within an alternative model \eqn{M_1} (more complex), the LRT tests
#' whether the additional complexity of \eqn{M_1} is justified by the data.
#'
#' The test statistic is:
#'
#' \deqn{\Lambda = -2 \left( \ell_0 - \ell_1 \right) = -2 \log \frac{L_0}{L_1}}
#'
#' where \eqn{\ell_0} and \eqn{\ell_1} are the maximized log-likelihoods under
#' the null and alternative models, respectively.
#'
#' Under \eqn{H_0} and regularity conditions, \eqn{\Lambda} is asymptotically
#' chi-squared distributed with degrees of freedom equal to the difference in
#' the number of free parameters between models.
#'
#' @section Assumptions:
#' \enumerate{
#'   \item The null model must be nested within the alternative model
#'     (i.e., obtainable by constraining parameters of the alternative).
#'   \item Both likelihoods must be computed from the same dataset.
#'   \item Standard regularity conditions for asymptotic chi-squared
#'     distribution must hold (true parameter not on boundary, etc.).
#' }
#'
#' @section Relationship to Other Tests:
#' The LRT is one of the "holy trinity" of likelihood-based tests, alongside
#' the Wald test ([wald_test()]) and the score (Lagrange multiplier) test.
#' All three are asymptotically equivalent under \eqn{H_0}, but the LRT is
#' often preferred because it is invariant to reparameterization.
#'
#' @param null_loglik Numeric. The maximized log-likelihood under the null
#'   (simpler) model.
#' @param alt_loglik Numeric. The maximized log-likelihood under the
#'   alternative (more complex) model.
#' @param dof Positive integer. Degrees of freedom, typically the difference
#'   in the number of free parameters between models.
#'
#' @return A `hypothesis_test` object of subclass `likelihood_ratio_test`
#'   containing:
#' \describe{
#'   \item{stat}{The LRT statistic \eqn{\Lambda = -2(\ell_0 - \ell_1)}}
#'   \item{p.value}{P-value from chi-squared distribution with `dof` degrees
#'     of freedom}
#'   \item{dof}{The degrees of freedom}
#'   \item{null_loglik}{The input null model log-likelihood}
#'   \item{alt_loglik}{The input alternative model log-likelihood}
#' }
#'
#' @examples
#' # Comparing nested regression models
#' # Null model: y ~ x1 (log-likelihood = -150)
#' # Alt model:  y ~ x1 + x2 + x3 (log-likelihood = -140)
#' # Difference: 3 additional parameters
#'
#' test <- lrt(null_loglik = -150, alt_loglik = -140, dof = 3)
#' test
#'
#' # Is the more complex model significantly better?
#' is_significant_at(test, 0.05)
#'
#' # Extract the test statistic (should be 20)
#' test_stat(test)
#'
#' # Access stored inputs for inspection
#' test$null_loglik
#' test$alt_loglik
#'
#' @seealso [wald_test()] for testing individual parameters
#' @importFrom stats pchisq
#' @export
lrt <- function(null_loglik, alt_loglik, dof) {
  stat <- -2 * (null_loglik - alt_loglik)
  p.value <- pchisq(stat, df = dof, lower.tail = FALSE)
  hypothesis_test(
    stat = stat,
    p.value = p.value,
    dof = dof,
    superclasses = "likelihood_ratio_test",
    null_loglik = null_loglik,
    alt_loglik = alt_loglik
  )
}

#' One-Sample Z-Test
#'
#' Tests whether a population mean equals a hypothesized value when the
#' population standard deviation is known.
#'
#' @details
#' The z-test is one of the simplest and most fundamental hypothesis tests.
#' It tests \eqn{H_0: \mu = \mu_0} against various alternatives when the
#' population standard deviation \eqn{\sigma} is known.
#'
#' Given a sample \eqn{x_1, \ldots, x_n}, the test statistic is:
#'
#' \deqn{z = \frac{\bar{x} - \mu_0}{\sigma / \sqrt{n}}}
#'
#' Under \eqn{H_0}, this follows a standard normal distribution. The p-value
#' depends on the alternative hypothesis:
#' \itemize{
#'   \item Two-sided (\eqn{H_1: \mu \neq \mu_0}): \eqn{2 \cdot P(Z > |z|)}
#'   \item Less (\eqn{H_1: \mu < \mu_0}): \eqn{P(Z < z)}
#'   \item Greater (\eqn{H_1: \mu > \mu_0}): \eqn{P(Z > z)}
#' }
#'
#' @section When to Use:
#' The z-test requires knowing the population standard deviation, which is
#' rare in practice. When \eqn{\sigma} is unknown and estimated from data,
#' use a t-test instead. The z-test is primarily pedagogical, illustrating
#' the logic of hypothesis testing in its simplest form.
#'
#' @section Relationship to Wald Test:
#' The z-test is a special case of the Wald test ([wald_test()]) where the
#' parameter is a mean and the standard error is \eqn{\sigma/\sqrt{n}}.
#' The Wald test generalizes this to any asymptotically normal estimator.
#'
#' @param x Numeric vector. The sample data.
#' @param mu0 Numeric. The hypothesized population mean under \eqn{H_0}.
#'   Default is 0.
#' @param sigma Numeric. The known population standard deviation.
#' @param alternative Character. Type of alternative hypothesis:
#'   `"two.sided"` (default), `"less"`, or `"greater"`.
#'
#' @return A `hypothesis_test` object of subclass `z_test` containing:
#' \describe{
#'   \item{stat}{The z-statistic}
#'   \item{p.value}{The p-value for the specified alternative}
#'   \item{dof}{Degrees of freedom (Inf for normal distribution)}
#'   \item{alternative}{The alternative hypothesis used}
#'   \item{null_value}{The hypothesized mean \eqn{\mu_0}}
#'   \item{estimate}{The sample mean \eqn{\bar{x}}}
#'   \item{sigma}{The known population standard deviation}
#'   \item{n}{The sample size}
#' }
#'
#' @examples
#' # A light bulb manufacturer claims bulbs last 1000 hours on average.
#' # We test 50 bulbs and know from historical data that sigma = 100 hours.
#' lifetimes <- c(980, 1020, 950, 1010, 990, 1005, 970, 1030, 985, 995,
#'                1000, 1015, 960, 1025, 975, 1008, 992, 1012, 988, 1002,
#'                978, 1018, 965, 1022, 982, 1005, 995, 1010, 972, 1028,
#'                990, 1000, 985, 1015, 968, 1020, 980, 1008, 992, 1012,
#'                975, 1018, 962, 1025, 985, 1002, 988, 1010, 978, 1020)
#'
#' # Two-sided test: H0: mu = 1000 vs H1: mu != 1000
#' z_test(lifetimes, mu0 = 1000, sigma = 100)
#'
#' # One-sided test: are bulbs lasting less than claimed?
#' z_test(lifetimes, mu0 = 1000, sigma = 100, alternative = "less")
#'
#' @seealso [wald_test()] for the general case with estimated standard errors
#' @importFrom stats pnorm
#' @export
z_test <- function(x, mu0 = 0, sigma, alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  n <- length(x)
  xbar <- mean(x)
  se <- sigma / sqrt(n)
  z <- (xbar - mu0) / se

  p.value <- switch(alternative,
    "two.sided" = 2 * pnorm(-abs(z)),
    "less" = pnorm(z),
    "greater" = pnorm(z, lower.tail = FALSE)
  )

  hypothesis_test(
    stat = z,
    p.value = p.value,
    dof = Inf,
    superclasses = "z_test",
    alternative = alternative,
    null_value = mu0,
    estimate = xbar,
    sigma = sigma,
    n = n
  )
}

#' Wald Test
#'
#' Computes the Wald test statistic and p-value for testing whether a
#' parameter (or parameter vector) equals a hypothesized value.
#'
#' @details
#' The Wald test is a fundamental tool in statistical inference, used to test
#' the null hypothesis \eqn{H_0: \theta = \theta_0} against the alternative
#' \eqn{H_1: \theta \neq \theta_0}.
#'
#' **Univariate case** (when `se` is provided):
#' The test is based on the asymptotic normality of maximum likelihood
#' estimators. Under regularity conditions, if \eqn{\hat{\theta}} is the MLE
#' with standard error \eqn{SE(\hat{\theta})}, then:
#'
#' \deqn{z = \frac{\hat{\theta} - \theta_0}{SE(\hat{\theta})} \sim N(0, 1)}
#'
#' The Wald statistic is reported as \eqn{W = z^2}, which follows
#' a chi-squared distribution with 1 degree of freedom under \eqn{H_0}.
#' The z-score is stored in the returned object for reference.
#'
#' **Multivariate case** (when `vcov` is provided):
#' For a \eqn{k}-dimensional parameter vector \eqn{\hat{\theta}} with
#' variance-covariance matrix \eqn{\Sigma}, the Wald statistic is:
#'
#' \deqn{W = (\hat{\theta} - \theta_0)' \Sigma^{-1} (\hat{\theta} - \theta_0)
#'   \sim \chi^2(k)}
#'
#' The p-value is computed as \eqn{P(\chi^2_k \geq W)}.
#'
#' @section Relationship to Other Tests:
#' The Wald test is one of the "holy trinity" of likelihood-based tests,
#' alongside the likelihood ratio test ([lrt()]) and the score test.
#' For large samples, all three are asymptotically equivalent, but they
#' can differ substantially in finite samples.
#'
#' @param estimate Numeric. The estimated parameter value \eqn{\hat{\theta}}.
#'   A scalar for the univariate case or a vector for the multivariate case.
#' @param se Numeric. Standard error of the estimate for the univariate case.
#'   Mutually exclusive with `vcov`.
#' @param vcov Numeric matrix. Variance-covariance matrix for the multivariate
#'   case. Mutually exclusive with `se`.
#' @param null_value Numeric. The hypothesized value \eqn{\theta_0} under the
#'   null hypothesis. Default is 0. A scalar for the univariate case or a
#'   vector of the same length as `estimate` for the multivariate case.
#'
#' @return A `hypothesis_test` object of subclass `wald_test` containing:
#' \describe{
#'   \item{stat}{The Wald statistic \eqn{W}}
#'   \item{p.value}{Two-sided p-value from chi-squared distribution}
#'   \item{dof}{Degrees of freedom (1 for univariate, \eqn{k} for multivariate)}
#'   \item{z}{The z-score (univariate case only)}
#'   \item{estimate}{The input estimate}
#'   \item{se}{The input standard error (univariate case only)}
#'   \item{vcov}{The input variance-covariance matrix (multivariate case only)}
#'   \item{null_value}{The input null hypothesis value}
#' }
#'
#' @examples
#' # Univariate: test whether a regression coefficient differs from zero
#' w <- wald_test(estimate = 2.5, se = 0.8, null_value = 0)
#' w
#'
#' # Extract components
#' test_stat(w)        # Wald statistic (chi-squared)
#' w$z                 # z-score
#' pval(w)             # p-value
#' is_significant_at(w, 0.05)
#'
#' # Test against a non-zero null
#' wald_test(estimate = 2.5, se = 0.8, null_value = 2)
#'
#' # Multivariate: test two parameters jointly
#' est <- c(2.0, 3.0)
#' V <- matrix(c(1.0, 0.3, 0.3, 1.0), 2, 2)
#' w_mv <- wald_test(estimate = est, vcov = V, null_value = c(0, 0))
#' test_stat(w_mv)
#' dof(w_mv)           # 2
#' pval(w_mv)
#'
#' @seealso [lrt()] for likelihood ratio tests, [z_test()] for testing means
#' @importFrom stats pchisq pnorm
#' @export
wald_test <- function(estimate, se = NULL, vcov = NULL, null_value = 0) {
  if (!is.null(se) && !is.null(vcov))
    stop("Specify exactly one of 'se' (univariate) or 'vcov' (multivariate)")
  if (is.null(se) && is.null(vcov))
    stop("Specify exactly one of 'se' (univariate) or 'vcov' (multivariate)")

  if (!is.null(vcov)) {
    # Multivariate: W = (theta - theta0)' V^{-1} (theta - theta0) ~ chi-sq(k)
    diff <- estimate - null_value
    k <- length(estimate)
    stat <- as.numeric(t(diff) %*% solve(vcov) %*% diff)
    p.value <- pchisq(stat, df = k, lower.tail = FALSE)
    hypothesis_test(
      stat = stat,
      p.value = p.value,
      dof = k,
      superclasses = "wald_test",
      estimate = estimate,
      vcov = vcov,
      null_value = null_value
    )
  } else {
    # Univariate: W = z^2 ~ chi-sq(1)
    z <- (estimate - null_value) / se
    stat <- z^2
    p.value <- pchisq(stat, df = 1, lower.tail = FALSE)
    hypothesis_test(
      stat = stat,
      p.value = p.value,
      dof = 1,
      superclasses = "wald_test",
      z = z,
      estimate = estimate,
      se = se,
      null_value = null_value
    )
  }
}

#' Score Test (Lagrange Multiplier Test)
#'
#' Computes the score test statistic and p-value for testing whether a
#' parameter equals a hypothesized value, using the score function and
#' Fisher information evaluated at the null.
#'
#' @details
#' The score test is one of the "holy trinity" of likelihood-based tests,
#' alongside the Wald test ([wald_test()]) and the likelihood ratio test
#' ([lrt()]). All three are asymptotically equivalent under \eqn{H_0}, but
#' they differ in what they require:
#'
#' \itemize{
#'   \item **Wald test**: Needs the MLE and its standard error — requires
#'     fitting the alternative model.
#'   \item **LRT**: Needs maximized log-likelihoods under both models —
#'     requires fitting both.
#'   \item **Score test**: Needs only the score and information at
#'     \eqn{\theta_0} — requires fitting only the null model.
#' }
#'
#' This makes the score test computationally attractive when the null model
#' is simple but the alternative is expensive to fit.
#'
#' For the univariate case:
#' \deqn{S = \frac{U(\theta_0)^2}{I(\theta_0)} \sim \chi^2_1}
#'
#' For the multivariate case with \eqn{k} parameters:
#' \deqn{S = U(\theta_0)^\top I(\theta_0)^{-1} U(\theta_0) \sim \chi^2_k}
#'
#' The function detects scalar vs. vector input and dispatches accordingly.
#'
#' @param score Numeric scalar or vector. The score function
#'   \eqn{U(\theta_0) = \partial \ell / \partial \theta} evaluated at the
#'   null value.
#' @param fisher_info Numeric scalar or matrix. The Fisher information
#'   \eqn{I(\theta_0)} evaluated at the null value.
#' @param null_value Optional. The null hypothesis value, stored for
#'   reference but not used in computation.
#'
#' @return A `hypothesis_test` object of subclass `score_test` containing:
#' \describe{
#'   \item{stat}{The score statistic \eqn{S}}
#'   \item{p.value}{P-value from chi-squared distribution}
#'   \item{dof}{Degrees of freedom (1 for univariate, \eqn{k} for multivariate)}
#'   \item{score}{The input score value(s)}
#'   \item{fisher_info}{The input Fisher information}
#'   \item{null_value}{The input null hypothesis value (if provided)}
#' }
#'
#' @examples
#' # Univariate score test
#' score_test(score = 2, fisher_info = 2)
#'
#' # Compare the trinity on the same problem
#' score_test(score = 2, fisher_info = 2)
#' wald_test(estimate = 6, se = sqrt(6/10), null_value = 5)
#'
#' # Multivariate
#' score_test(score = c(1, 2), fisher_info = diag(c(1, 1)))
#'
#' @seealso [wald_test()], [lrt()] for the other members of the trinity
#' @importFrom stats pchisq
#' @export
score_test <- function(score, fisher_info, null_value = NULL) {
  if (is.matrix(fisher_info)) {
    k <- length(score)
    stat <- as.numeric(t(score) %*% solve(fisher_info) %*% score)
  } else {
    k <- 1
    stat <- score^2 / fisher_info
  }
  p.value <- pchisq(stat, df = k, lower.tail = FALSE)
  hypothesis_test(
    stat = stat,
    p.value = p.value,
    dof = k,
    superclasses = "score_test",
    score = score,
    fisher_info = fisher_info,
    null_value = null_value
  )
}

#' Combine Independent P-Values (Fisher's Method)
#'
#' Combines p-values from independent hypothesis tests into a single
#' omnibus test using Fisher's method.
#'
#' @details
#' Fisher's method is a meta-analytic technique for combining evidence from
#' multiple independent tests of the same hypothesis (or related hypotheses).
#' It demonstrates a key principle: **combining hypothesis tests yields a
#' hypothesis test** (the closure property).
#'
#' Given \eqn{k} independent p-values \eqn{p_1, \ldots, p_k}, Fisher's
#' statistic is:
#'
#' \deqn{X^2 = -2 \sum_{i=1}^{k} \log(p_i)}
#'
#' Under the global null hypothesis (all individual nulls are true), this
#' follows a chi-squared distribution with \eqn{2k} degrees of freedom.
#'
#' @section Why It Works:
#' If \eqn{p_i} is a valid p-value under \eqn{H_0}, then \eqn{p_i \sim U(0,1)}.
#' Therefore \eqn{-2\log(p_i) \sim \chi^2_2}. The sum of independent
#' chi-squared random variables is also chi-squared with summed degrees of
#' freedom, giving \eqn{X^2 \sim \chi^2_{2k}}.
#'
#' @section Interpretation:
#' A significant combined p-value indicates that **at least one** of the
#' individual null hypotheses is likely false, but does not identify which
#' one(s). Fisher's method is sensitive to any deviation from the global
#' null, making it powerful when effects exist but liberal when assumptions
#' are violated.
#'
#' @section Closure Property (SICP Principle):
#' This function exemplifies the closure property from SICP: the operation
#' of combining hypothesis tests produces another hypothesis test. The result
#' can be further combined, adjusted, or analyzed using the same generic
#' methods (`pval()`, `test_stat()`, `is_significant_at()`, etc.).
#'
#' @param ... `hypothesis_test` objects to combine, or numeric p-values.
#'   All tests must be independent.
#'
#' @return A `hypothesis_test` object of subclass `fisher_combined_test`
#'   containing:
#' \describe{
#'   \item{stat}{Fisher's chi-squared statistic \eqn{-2\sum\log(p_i)}}
#'   \item{p.value}{P-value from \eqn{\chi^2_{2k}} distribution}
#'   \item{dof}{Degrees of freedom (\eqn{2k})}
#'   \item{n_tests}{Number of tests combined}
#'   \item{component_pvals}{Vector of the individual p-values}
#' }
#'
#' @examples
#' # Scenario: Three independent studies test the same drug effect
#' # Study 1: p = 0.08 (trend, not significant)
#' # Study 2: p = 0.12 (not significant)
#' # Study 3: p = 0.04 (significant at 0.05)
#'
#' # Combine using raw p-values
#' combined <- fisher_combine(0.08, 0.12, 0.04)
#' combined
#' is_significant_at(combined, 0.05)  # Stronger evidence together
#'
#' # Or combine hypothesis_test objects directly
#' t1 <- wald_test(estimate = 1.5, se = 0.9)
#' t2 <- wald_test(estimate = 0.8, se = 0.5)
#' t3 <- z_test(rnorm(30, mean = 0.3), mu0 = 0, sigma = 1)
#'
#' fisher_combine(t1, t2, t3)
#'
#' # The result is itself a hypothesis_test, so it composes
#' # (though combining non-independent tests is invalid)
#'
#' @seealso [adjust_pval()] for multiple testing correction (different goal)
#' @importFrom stats pchisq
#' @export
fisher_combine <- function(...) {
  inputs <- list(...)

  # Extract p-values from hypothesis_test objects or use raw numerics
  pvals <- vapply(inputs, function(x) {
    if (inherits(x, "hypothesis_test")) {
      pval(x)
    } else if (is.numeric(x) && length(x) == 1) {
      x
    } else {
      stop("Arguments must be hypothesis_test objects or numeric p-values")
    }
  }, numeric(1))

  # Validate p-values
  if (any(pvals <= 0 | pvals > 1)) {
    stop("P-values must be in (0, 1]")
  }

  k <- length(pvals)
  stat <- -2 * sum(log(pvals))
  df <- 2 * k
  p.value <- pchisq(stat, df = df, lower.tail = FALSE)

  hypothesis_test(
    stat = stat,
    p.value = p.value,
    dof = df,
    superclasses = "fisher_combined_test",
    n_tests = k,
    component_pvals = pvals
  )
}

#' Confidence Interval from Hypothesis Test (Duality)
#'
#' Extracts a confidence interval from a hypothesis test object, exploiting
#' the fundamental duality between hypothesis tests and confidence intervals.
#'
#' @details
#' Hypothesis tests and confidence intervals are two views of the same
#' underlying inference. For a test of \eqn{H_0: \theta = \theta_0} at level
#' \eqn{\alpha}, the \eqn{(1-\alpha)} confidence interval contains exactly
#' those values of \eqn{\theta_0} that would **not** be rejected.
#'
#' This duality means:
#' \itemize{
#'   \item A 95% CI contains all values where the two-sided test has p > 0.05
#'   \item The CI boundary is where p = 0.05 exactly
#'   \item Inverting a test "inverts" it into a confidence set
#' }
#'
#' @section Available Methods:
#' Confidence intervals are currently implemented for:
#' \itemize{
#'   \item `wald_test`: Uses \eqn{\hat{\theta} \pm z_{\alpha/2} \cdot SE}
#'   \item `z_test`: Uses \eqn{\bar{x} \pm z_{\alpha/2} \cdot \sigma/\sqrt{n}}
#' }
#'
#' Tests without stored estimates (like `lrt` or `fisher_combined_test`)
#' cannot produce confidence intervals directly.
#'
#' @param object A `hypothesis_test` object.
#' @param parm Ignored (for compatibility with generic).
#' @param level Numeric. Confidence level (default 0.95).
#' @param ... Additional arguments (ignored).
#'
#' @return A named numeric vector with elements `lower` and `upper`.
#'
#' @examples
#' # Wald test stores estimate and SE, so CI is available
#' w <- wald_test(estimate = 2.5, se = 0.8)
#' confint(w)              # 95% CI
#' confint(w, level = 0.99) # 99% CI
#'
#' # The duality: 2.5 is in the CI, and testing H0: theta = 2.5
#' # would give p = 1 (not rejected)
#' wald_test(estimate = 2.5, se = 0.8, null_value = 2.5)
#'
#' # z-test also supports confint
#' z <- z_test(rnorm(50, mean = 10, sd = 2), mu0 = 9, sigma = 2)
#' confint(z)
#'
#' @importFrom stats qnorm
#' @export
confint.hypothesis_test <- function(object, parm = NULL, level = 0.95, ...) {
  stop("confint() not implemented for class '", class(object)[1],
       "'. Requires stored estimate and standard error.")
}

#' @rdname confint.hypothesis_test
#' @export
confint.wald_test <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(object$se)) {
    stop("confint() is not supported for multivariate Wald tests. ",
         "Use invert_test() for multivariate confidence regions.")
  }
  alpha <- 1 - level
  z_crit <- qnorm(1 - alpha / 2)
  estimate <- object$estimate
  se <- object$se

  c(lower = estimate - z_crit * se,
    upper = estimate + z_crit * se)
}

#' @rdname confint.hypothesis_test
#' @export
confint.z_test <- function(object, parm = NULL, level = 0.95, ...) {
  alpha <- 1 - level
  z_crit <- qnorm(1 - alpha / 2)
  estimate <- object$estimate
  se <- object$sigma / sqrt(object$n)

  c(lower = estimate - z_crit * se,
    upper = estimate + z_crit * se)
}

#' Adjust P-Value for Multiple Testing
#'
#' Applies a multiple testing correction to a hypothesis test or vector of
#' tests, returning adjusted test object(s).
#'
#' @details
#' When performing multiple hypothesis tests, the probability of at least one
#' false positive (Type I error) increases. Multiple testing corrections
#' adjust p-values to control error rates across the family of tests.
#'
#' This function demonstrates the **higher-order function** pattern: it takes
#' a hypothesis test as input and returns a transformed hypothesis test as
#' output. The adjusted test retains all original properties but with a
#' corrected p-value.
#'
#' @section Available Methods:
#' The `method` parameter accepts any method supported by [stats::p.adjust()]:
#' \describe{
#'   \item{`"bonferroni"`}{Multiplies p-values by \eqn{n}. Controls
#'     family-wise error rate (FWER). Conservative.
#'   }
#'   \item{`"holm"`}{Step-down Bonferroni. Controls FWER. Less conservative
#'     than Bonferroni while maintaining strong control.
#'   }
#'   \item{`"BH"` or `"fdr"`}{Benjamini-Hochberg procedure. Controls false
#'     discovery rate (FDR). More powerful for large-scale testing.
#'   }
#'   \item{`"hochberg"`}{Step-up procedure. Controls FWER under independence.}
#'   \item{`"hommel"`}{More powerful than Hochberg but computationally
#'     intensive.
#'   }
#'   \item{`"BY"`}{Benjamini-Yekutieli. Controls FDR under arbitrary
#'     dependence.
#'   }
#'   \item{`"none"`}{No adjustment (identity transformation).}
#' }
#'
#' @section Higher-Order Function Pattern:
#' This function exemplifies transforming hypothesis tests:
#' ```
#' adjust_pval : hypothesis_test -> hypothesis_test
#' ```
#' The output can be used with all standard generics (`pval()`, `test_stat()`,
#' `is_significant_at()`, etc.) and can be further composed.
#'
#' @param x A `hypothesis_test` object, or a list of such objects.
#' @param method Character. Adjustment method (see Details). Default is
#'   `"bonferroni"`.
#' @param n Integer. Total number of tests in the family. If `x` is a list,
#'   defaults to `length(x)`. For a single test, this must be specified.
#'
#' @return For a single test: a `hypothesis_test` object of subclass
#'   `adjusted_test` with the adjusted p-value. For a list of tests: a list
#'   of adjusted test objects.
#'
#' The returned object contains:
#' \describe{
#'   \item{stat}{Original test statistic (unchanged)}
#'   \item{p.value}{Adjusted p-value}
#'   \item{dof}{Original degrees of freedom (unchanged)}
#'   \item{adjustment_method}{The method used}
#'   \item{original_pval}{The unadjusted p-value}
#'   \item{n_tests}{Number of tests in the family}
#' }
#'
#' @examples
#' # Single test adjustment (must specify n)
#' w <- wald_test(estimate = 2.0, se = 0.8)
#' pval(w)  # Original p-value
#'
#' w_adj <- adjust_pval(w, method = "bonferroni", n = 10)
#' pval(w_adj)  # Adjusted (multiplied by 10, capped at 1)
#' w_adj$original_pval  # Can still access original
#'
#' # Adjusting multiple tests at once
#' tests <- list(
#'   wald_test(estimate = 2.5, se = 0.8),
#'   wald_test(estimate = 1.2, se = 0.5),
#'   wald_test(estimate = 0.8, se = 0.9)
#' )
#'
#' # BH (FDR) correction - n is inferred from list length
#' adjusted <- adjust_pval(tests, method = "BH")
#' vapply(adjusted, pval, numeric(1))  # Adjusted p-values
#'
#' # Compare methods
#' vapply(tests, pval, numeric(1))  # Original
#' vapply(adjust_pval(tests, method = "bonferroni"), pval, numeric(1))
#' vapply(adjust_pval(tests, method = "BH"), pval, numeric(1))
#'
#' @seealso [stats::p.adjust()] for the underlying adjustment,
#'   [fisher_combine()] for combining (not adjusting) p-values
#' @importFrom stats p.adjust
#' @export
adjust_pval <- function(x, method = "bonferroni", n = NULL) {
  # Handle list of tests

  if (is.list(x) && !inherits(x, "hypothesis_test")) {
    if (is.null(n)) n <- length(x)
    pvals <- vapply(x, pval, numeric(1))
    adjusted_pvals <- p.adjust(pvals, method = method, n = n)

    mapply(function(test, adj_p, orig_p) {
      hypothesis_test(
        stat = test_stat(test),
        p.value = adj_p,
        dof = dof(test),
        superclasses = c("adjusted_test", class(test)),
        adjustment_method = method,
        original_pval = orig_p,
        n_tests = n
      )
    }, x, adjusted_pvals, pvals, SIMPLIFY = FALSE)
  } else {
    # Single test
    if (is.null(n)) {
      stop("For a single test, 'n' (total number of tests) must be specified")
    }

    orig_p <- pval(x)
    adj_p <- p.adjust(orig_p, method = method, n = n)

    hypothesis_test(
      stat = test_stat(x),
      p.value = adj_p,
      dof = dof(x),
      superclasses = c("adjusted_test", class(x)),
      adjustment_method = method,
      original_pval = orig_p,
      n_tests = n
    )
  }
}

#' Complement a Hypothesis Test (NOT)
#'
#' Negates a hypothesis test by transforming its p-value: \eqn{p \to 1 - p}.
#' The complement test rejects when the original test fails to reject.
#'
#' @details
#' The complement is the NOT operation in the Boolean algebra of hypothesis
#' tests. Together with [intersection_test()] (AND) and [union_test()] (OR),
#' it forms a complete algebra where De Morgan's laws hold by construction.
#'
#' @section Connection to Equivalence Testing:
#' If the original test checks "is \eqn{\theta} different from
#' \eqn{\theta_0}?" (rejecting when the difference is large), the
#' complement checks "is \eqn{\theta} close to \eqn{\theta_0}?"
#' (rejecting when the difference is small). This connects to the
#' Two One-Sided Tests (TOST) procedure used in bioequivalence studies.
#'
#' @section Algebraic Properties:
#' \itemize{
#'   \item Double complement is identity:
#'     `complement_test(complement_test(t))` has the same p-value as `t`
#'   \item De Morgan's law:
#'     `union_test(a, b) = complement_test(intersection_test(complement_test(a), complement_test(b)))`
#' }
#'
#' @param test A `hypothesis_test` object.
#'
#' @return A `hypothesis_test` object with `"complemented_test"` prepended
#'   to the class vector. The original class hierarchy is preserved.
#' \describe{
#'   \item{original_pval}{The pre-complement p-value}
#'   \item{original_test}{The input test object}
#' }
#'
#' @examples
#' w <- wald_test(estimate = 3.0, se = 1.0)
#' pval(w)                  # small
#' pval(complement_test(w)) # large
#'
#' # Double complement recovers the original
#' pval(complement_test(complement_test(w))) == pval(w)
#'
#' @seealso [intersection_test()], [union_test()]
#' @export
complement_test <- function(test) {
  hypothesis_test(
    stat = test_stat(test),
    p.value = 1 - pval(test),
    dof = dof(test),
    superclasses = c("complemented_test", class(test)),
    original_pval = pval(test),
    original_test = test
  )
}

#' Intersection Test (AND)
#'
#' Combines hypothesis tests using the AND rule: rejects only when ALL
#' component tests reject.
#'
#' @details
#' The p-value is \eqn{\max(p_1, \ldots, p_k)} — the intersection rejects
#' at level \eqn{\alpha} if and only if every component p-value is below
#' \eqn{\alpha}.
#'
#' This is the intersection-union test (IUT; Berger, 1982). No multiplicity
#' correction is needed — the max is inherently conservative.
#'
#' @section Use Case --- Bioequivalence:
#' Bioequivalence requires showing a drug's effect is both "not too low"
#' AND "not too high". This is naturally an intersection test.
#'
#' @section Boolean Algebra:
#' Together with [complement_test()] (NOT) and [union_test()] (OR), this
#' forms a complete Boolean algebra. De Morgan's law holds by construction:
#' `union_test(a, b) = complement_test(intersection_test(complement_test(a), complement_test(b)))`
#'
#' @param ... `hypothesis_test` objects or numeric p-values.
#'
#' @return A `hypothesis_test` of subclass `intersection_test` with fields
#'   `n_tests` and `component_pvals`.
#'
#' @examples
#' # All must reject for intersection to reject
#' intersection_test(0.01, 0.03, 0.04)  # significant
#' intersection_test(0.01, 0.80)         # not significant
#'
#' @seealso [union_test()], [complement_test()], [fisher_combine()]
#' @export
intersection_test <- function(...) {
  inputs <- list(...)
  pvals <- vapply(inputs, function(x) {
    if (inherits(x, "hypothesis_test")) pval(x)
    else if (is.numeric(x) && length(x) == 1) x
    else stop("Arguments must be hypothesis_test objects or numeric p-values")
  }, numeric(1))
  k <- length(pvals)
  p.value <- max(pvals)
  hypothesis_test(
    stat = p.value,
    p.value = p.value,
    dof = k,
    superclasses = "intersection_test",
    n_tests = k,
    component_pvals = pvals
  )
}

#' Union Test (OR via De Morgan's Law)
#'
#' Combines hypothesis tests using the OR rule: rejects when ANY component
#' test rejects.
#'
#' @details
#' The union test implements the OR operation in the Boolean algebra of
#' hypothesis tests. It is defined via De Morgan's law:
#'
#' \deqn{\text{union}(t_1, \ldots, t_k) =
#'   \text{NOT}(\text{AND}(\text{NOT}(t_1), \ldots, \text{NOT}(t_k)))}
#'
#' This is not an approximation --- it is the definition. The implementation
#' is literally the De Morgan law applied to [complement_test()] and
#' [intersection_test()].
#'
#' The resulting p-value is \eqn{\min(p_1, \ldots, p_k)}.
#'
#' @section Multiplicity Warning:
#' The uncorrected \eqn{\min(p)} is anti-conservative when testing multiple
#' hypotheses. If you need to control the family-wise error rate, apply
#' [adjust_pval()] to the component tests before combining, or use
#' [fisher_combine()] which pools evidence differently.
#'
#' The raw union test is appropriate when you genuinely want to reject a
#' global null if any sub-hypothesis is false, without multiplicity
#' correction --- for example, in screening or exploratory analysis.
#'
#' @section Boolean Algebra:
#' Together with [intersection_test()] (AND) and [complement_test()] (NOT),
#' this forms a complete Boolean algebra over hypothesis tests:
#'
#' \itemize{
#'   \item AND: [intersection_test()] --- reject when all reject
#'   \item OR: `union_test()` --- reject when any rejects
#'   \item NOT: [complement_test()] --- reject when original fails to reject
#' }
#'
#' De Morgan's laws hold by construction:
#' \itemize{
#'   \item `union(a, b) = NOT(AND(NOT(a), NOT(b)))`
#'   \item `intersection(a, b) = NOT(OR(NOT(a), NOT(b)))`
#' }
#'
#' @param ... `hypothesis_test` objects or numeric p-values to combine.
#'
#' @return A `hypothesis_test` object of subclass `union_test` containing:
#' \describe{
#'   \item{stat}{The minimum p-value (used as the test statistic)}
#'   \item{p.value}{\eqn{\min(p_1, \ldots, p_k)}}
#'   \item{dof}{Number of component tests}
#'   \item{n_tests}{Number of tests combined}
#'   \item{component_pvals}{Vector of individual p-values}
#' }
#'
#' @examples
#' # Screen three biomarkers: reject if ANY is significant
#' t1 <- wald_test(estimate = 0.5, se = 0.3)
#' t2 <- wald_test(estimate = 2.1, se = 0.8)
#' t3 <- wald_test(estimate = 1.0, se = 0.4)
#' union_test(t1, t2, t3)
#'
#' # De Morgan's law in action
#' a <- wald_test(estimate = 2.0, se = 1.0)
#' b <- wald_test(estimate = 1.5, se = 0.8)
#' # These are equivalent:
#' pval(union_test(a, b))
#' pval(complement_test(intersection_test(complement_test(a), complement_test(b))))
#'
#' @seealso [intersection_test()] for AND, [complement_test()] for NOT,
#'   [fisher_combine()] for evidence pooling
#' @export
union_test <- function(...) {
  inputs <- list(...)

  # Wrap raw p-values as hypothesis_test objects for complement_test
  tests <- lapply(inputs, function(x) {
    if (inherits(x, "hypothesis_test")) x
    else if (is.numeric(x) && length(x) == 1)
      hypothesis_test(stat = NA_real_, p.value = x, dof = NA_real_)
    else stop("Arguments must be hypothesis_test objects or numeric p-values")
  })

  # De Morgan: OR(a, b, ...) = NOT(AND(NOT(a), NOT(b), ...))
  result <- complement_test(
    do.call(intersection_test, lapply(tests, complement_test))
  )

  # Extract component p-values for metadata
  pvals <- vapply(tests, pval, numeric(1))

  # Rewrap with union_test class and metadata
  hypothesis_test(
    stat = pval(result),
    p.value = pval(result),
    dof = length(pvals),
    superclasses = "union_test",
    n_tests = length(pvals),
    component_pvals = pvals
  )
}

#' Invert a Test into a Confidence Set (Test-Confidence Duality)
#'
#' Takes a test constructor function and returns the confidence set: the set
#' of null values that are not rejected at level \eqn{\alpha}.
#'
#' @details
#' Hypothesis tests and confidence sets are dual: a \eqn{(1-\alpha)}
#' confidence set contains exactly those parameter values \eqn{\theta_0}
#' for which the test of \eqn{H_0: \theta = \theta_0} would not reject at
#' level \eqn{\alpha}. This function makes that duality operational.
#'
#' `invert_test` is the most general confidence set constructor in the
#' package. Any test — including user-defined tests — can be inverted. The
#' specialized [confint()] methods for `wald_test` and `z_test` give exact
#' analytical intervals; `invert_test` gives numerical intervals for
#' arbitrary tests at the cost of a grid search.
#'
#' @section Higher-Order Function (SICP Principle):
#' This function takes a **function** as input (`test_fn`) and returns a
#' structured result. It demonstrates the power of the `hypothesis_test`
#' abstraction: because all tests implement the same interface (`pval()`),
#' `invert_test` can work with any test without knowing its internals.
#'
#' @param test_fn A function that takes a single numeric argument (the
#'   hypothesized null value) and returns a `hypothesis_test` object.
#' @param grid Numeric vector of candidate null values to test.
#' @param alpha Numeric. Significance level (default 0.05). The confidence
#'   level is \eqn{1 - \alpha}.
#'
#' @return An S3 object of class `confidence_set` containing:
#' \describe{
#'   \item{set}{Numeric vector of non-rejected null values}
#'   \item{alpha}{The significance level used}
#'   \item{level}{The confidence level (\eqn{1 - \alpha})}
#'   \item{test_fn}{The input test function}
#'   \item{grid}{The input grid}
#' }
#'
#' @examples
#' # Invert a Wald test to get a confidence interval
#' cs <- invert_test(
#'   test_fn = function(theta) wald_test(estimate = 2.5, se = 0.8, null_value = theta),
#'   grid = seq(0, 5, by = 0.01)
#' )
#' cs
#' lower(cs)
#' upper(cs)
#'
#' # Compare with the analytical confint (should agree up to grid resolution)
#' confint(wald_test(estimate = 2.5, se = 0.8))
#'
#' # Invert ANY user-defined test — no special support needed
#' my_test <- function(theta) {
#'   stat <- (5.0 - theta)^2 / 2
#'   hypothesis_test(stat = stat,
#'     p.value = pchisq(stat, df = 1, lower.tail = FALSE), dof = 1)
#' }
#' invert_test(my_test, grid = seq(0, 10, by = 0.01))
#'
#' @seealso [confint.wald_test()], [confint.z_test()] for analytical CIs
#' @export
invert_test <- function(test_fn, grid, alpha = 0.05) {
  pvals <- vapply(grid, function(theta) pval(test_fn(theta)), numeric(1))
  non_rejected <- grid[pvals >= alpha]
  structure(
    list(
      set = non_rejected,
      alpha = alpha,
      level = 1 - alpha,
      test_fn = test_fn,
      grid = grid
    ),
    class = "confidence_set"
  )
}

#' Print method for confidence sets
#' @param x a confidence_set object
#' @param ... additional arguments (ignored)
#' @return Returns `x` invisibly.
#' @examples
#' cs <- invert_test(
#'   function(theta) wald_test(estimate = 5, se = 1.2, null_value = theta),
#'   grid = seq(0, 10, by = 0.1)
#' )
#' print(cs)
#' @export
print.confidence_set <- function(x, ...) {
  cat(sprintf("Confidence set (%.0f%% level)\n", x$level * 100))
  cat("-----------------------------\n")
  if (length(x$set) == 0) {
    cat("Empty set (all null values rejected)\n")
  } else {
    cat(sprintf("Lower: %s\n", min(x$set)))
    cat(sprintf("Upper: %s\n", max(x$set)))
    cat(sprintf("Grid points in set: %s of %s\n", length(x$set), length(x$grid)))
  }
  invisible(x)
}

#' Extract the lower bound of a confidence set
#' @param x a confidence_set object
#' @param ... additional arguments (ignored)
#' @return Named numeric scalar with the lower bound.
#' @examples
#' cs <- invert_test(
#'   function(theta) wald_test(estimate = 5, se = 1.2, null_value = theta),
#'   grid = seq(0, 10, by = 0.1)
#' )
#' lower(cs)
#' @export
lower <- function(x, ...) UseMethod("lower")

#' @rdname lower
#' @export
lower.confidence_set <- function(x, ...) {
  if (length(x$set) == 0) return(c(lower = NA_real_))
  c(lower = min(x$set))
}

#' Extract the upper bound of a confidence set
#' @param x a confidence_set object
#' @param ... additional arguments (ignored)
#' @return Named numeric scalar with the upper bound.
#' @examples
#' cs <- invert_test(
#'   function(theta) wald_test(estimate = 5, se = 1.2, null_value = theta),
#'   grid = seq(0, 10, by = 0.1)
#' )
#' upper(cs)
#' @export
upper <- function(x, ...) UseMethod("upper")

#' @rdname upper
#' @export
upper.confidence_set <- function(x, ...) {
  if (length(x$set) == 0) return(c(upper = NA_real_))
  c(upper = max(x$set))
}
