# Boolean Algebra Extension Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add Boolean algebra over hypothesis tests (AND/OR/NOT), score test, test inversion, and multivariate Wald/score to hypothesize.

**Architecture:** All code goes in `R/hypothesize.R` (single-file design). All tests go in `tests/testthat/test-hypothesis-tests.R`. Each task adds one function via TDD: write failing tests, implement, verify, commit. Functions build on each other, so order matters.

**Tech Stack:** R, S3 classes, testthat v3, roxygen2, stats package (pchisq, qnorm, solve)

**Design doc:** `docs/plans/2026-02-26-boolean-algebra-design.md`

---

### Task 1: score_test (univariate)

**Files:**
- Modify: `R/hypothesize.R` (append after wald_test, ~line 441)
- Modify: `tests/testthat/test-hypothesis-tests.R` (append new section)

**Step 1: Write failing tests**

Append to `tests/testthat/test-hypothesis-tests.R`:

```r
# =============================================================================
# score_test
# =============================================================================

test_that("score_test computes correct univariate statistic and p-value", {
  # S = U(theta_0)^2 / I(theta_0) ~ chi-sq(1)
  s <- score_test(score = 2.0, fisher_info = 1.0)
  expect_s3_class(s, "score_test")
  expect_s3_class(s, "hypothesis_test")
  expect_equal(test_stat(s), 4.0)  # 2^2 / 1
  expect_equal(dof(s), 1)
  expect_equal(pval(s), pchisq(4.0, df = 1, lower.tail = FALSE))
})

test_that("score_test stores metadata", {
  s <- score_test(score = 1.5, fisher_info = 2.0, null_value = 3.0)
  expect_equal(s$score, 1.5)
  expect_equal(s$fisher_info, 2.0)
  expect_equal(s$null_value, 3.0)
})

test_that("score_test works with all accessors", {
  s <- score_test(score = 3.0, fisher_info = 4.0)
  expect_type(pval(s), "double")
  expect_type(test_stat(s), "double")
  expect_type(dof(s), "double")
  expect_type(is_significant_at(s, 0.05), "logical")
  expect_output(print(s), "score_test")
})

test_that("score_test is asymptotically equivalent to wald_test", {
  # For a normal model with known variance:
  # MLE = xbar, SE = sigma/sqrt(n), score at mu0 = n*(xbar-mu0)/sigma^2,
  # Fisher info = n/sigma^2
  # Both should give same statistic
  n <- 100
  sigma <- 2
  xbar <- 5.5
  mu0 <- 5.0
  se <- sigma / sqrt(n)
  score_val <- n * (xbar - mu0) / sigma^2
  info_val <- n / sigma^2

  w <- wald_test(estimate = xbar, se = se, null_value = mu0)
  s <- score_test(score = score_val, fisher_info = info_val)
  expect_equal(test_stat(s), test_stat(w), tolerance = 1e-10)
  expect_equal(pval(s), pval(w), tolerance = 1e-10)
})
```

**Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::test()' 2>&1 | tail -5`
Expected: FAIL — "could not find function 'score_test'"

**Step 3: Implement score_test**

Add to `R/hypothesize.R` after the `wald_test` function (after line 441):

```r
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
#'   \item **Wald test**: Needs the MLE \eqn{\hat{\theta}} and its standard
#'     error — requires fitting the full (alternative) model.
#'   \item **LRT**: Needs the maximized log-likelihoods under both models —
#'     requires fitting both models.
#'   \item **Score test**: Needs only the score and information at
#'     \eqn{\theta_0} — requires fitting only the null model.
#' }
#'
#' This makes the score test computationally attractive when the null model
#' is simple but the alternative is expensive to fit.
#'
#' The univariate test statistic is:
#' \deqn{S = \frac{U(\theta_0)^2}{I(\theta_0)} \sim \chi^2_1}
#'
#' For the multivariate case with \eqn{k} parameters:
#' \deqn{S = U(\theta_0)^\top I(\theta_0)^{-1} U(\theta_0) \sim \chi^2_k}
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
#'   \item{stat}{The score statistic}
#'   \item{p.value}{P-value from chi-squared distribution}
#'   \item{dof}{Degrees of freedom (1 for univariate, k for multivariate)}
#'   \item{score}{The input score function value}
#'   \item{fisher_info}{The input Fisher information}
#'   \item{null_value}{The input null hypothesis value (if provided)}
#' }
#'
#' @examples
#' # Univariate: test whether a Poisson rate equals 5
#' # Observed: 60 events in 10 units of time
#' # Score at lambda0=5: sum(x)/lambda0 - n = 60/5 - 10 = 2
#' # Fisher info at lambda0=5: n/lambda0 = 10/5 = 2
#' score_test(score = 2, fisher_info = 2)
#'
#' # Compare the trinity on the same problem
#' # (all three are asymptotically equivalent)
#' score_test(score = 2, fisher_info = 2)
#' wald_test(estimate = 6, se = sqrt(6/10), null_value = 5)
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
```

**Step 4: Regenerate docs and run tests**

Run: `Rscript -e 'devtools::document(); devtools::test()' 2>&1 | tail -5`
Expected: all tests PASS

**Step 5: Commit**

```bash
git add R/hypothesize.R NAMESPACE man/ tests/testthat/test-hypothesis-tests.R
git commit -m "feat: add score_test, completing the Wald/LRT/Score trinity"
```

---

### Task 2: complement_test (NOT)

**Files:**
- Modify: `R/hypothesize.R` (append after adjust_pval, end of file)
- Modify: `tests/testthat/test-hypothesis-tests.R`

**Step 1: Write failing tests**

```r
# =============================================================================
# complement_test (NOT)
# =============================================================================

test_that("complement_test inverts p-value", {
  w <- wald_test(estimate = 2.0, se = 1.0)
  c <- complement_test(w)
  expect_equal(pval(c), 1 - pval(w))
})

test_that("complement_test preserves class hierarchy", {
  w <- wald_test(estimate = 2.0, se = 1.0)
  c <- complement_test(w)
  expect_s3_class(c, "complemented_test")
  expect_s3_class(c, "wald_test")
  expect_s3_class(c, "hypothesis_test")
})

test_that("complement_test stores original p-value and test", {
  w <- wald_test(estimate = 2.0, se = 1.0)
  c <- complement_test(w)
  expect_equal(c$original_pval, pval(w))
  expect_identical(c$original_test, w)
})

test_that("double complement is identity", {
  w <- wald_test(estimate = 2.0, se = 1.0)
  cc <- complement_test(complement_test(w))
  expect_equal(pval(cc), pval(w), tolerance = 1e-15)
})

test_that("complement_test preserves test statistic and dof", {
  w <- wald_test(estimate = 2.0, se = 1.0)
  c <- complement_test(w)
  expect_equal(test_stat(c), test_stat(w))
  expect_equal(dof(c), dof(w))
})

test_that("complement_test works with all test types", {
  z <- z_test(rnorm(30, 1), mu0 = 0, sigma = 1)
  l <- lrt(null_loglik = -100, alt_loglik = -95, dof = 2)
  f <- fisher_combine(0.01, 0.05, 0.10)
  expect_s3_class(complement_test(z), "complemented_test")
  expect_s3_class(complement_test(l), "complemented_test")
  expect_s3_class(complement_test(f), "complemented_test")
})
```

**Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::test()' 2>&1 | tail -5`
Expected: FAIL — "could not find function 'complement_test'"

**Step 3: Implement complement_test**

Append to `R/hypothesize.R`:

```r
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
#' Two One-Sided Tests (TOST) procedure used in equivalence testing
#' and bioequivalence studies.
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
#'   to the class vector. The original class hierarchy is preserved, so a
#'   complemented Wald test is simultaneously a `complemented_test`, a
#'   `wald_test`, and a `hypothesis_test`. Additional fields:
#' \describe{
#'   \item{original_pval}{The pre-complement p-value}
#'   \item{original_test}{The input test object}
#' }
#'
#' @examples
#' # A significant Wald test
#' w <- wald_test(estimate = 3.0, se = 1.0)
#' pval(w)                  # small (rejects H0)
#' pval(complement_test(w)) # large (fails to reject complement)
#'
#' # Double complement recovers the original
#' pval(complement_test(complement_test(w))) == pval(w)
#'
#' @seealso [intersection_test()], [union_test()] for the AND/OR operations
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
```

**Step 4: Regenerate docs and run tests**

Run: `Rscript -e 'devtools::document(); devtools::test()' 2>&1 | tail -5`
Expected: all tests PASS

**Step 5: Commit**

```bash
git add R/hypothesize.R NAMESPACE man/ tests/testthat/test-hypothesis-tests.R
git commit -m "feat: add complement_test (NOT operation for Boolean algebra)"
```

---

### Task 3: intersection_test (AND)

**Files:**
- Modify: `R/hypothesize.R` (append after complement_test)
- Modify: `tests/testthat/test-hypothesis-tests.R`

**Step 1: Write failing tests**

```r
# =============================================================================
# intersection_test (AND)
# =============================================================================

test_that("intersection_test p-value is max of component p-values", {
  t1 <- wald_test(estimate = 2.0, se = 1.0)
  t2 <- wald_test(estimate = 3.0, se = 1.0)
  it <- intersection_test(t1, t2)
  expect_equal(pval(it), max(pval(t1), pval(t2)))
})

test_that("intersection_test works with raw p-values", {
  it <- intersection_test(0.01, 0.05, 0.10)
  expect_equal(pval(it), 0.10)
})

test_that("intersection_test works with mixed inputs", {
  w <- wald_test(estimate = 2.0, se = 1.0)
  it <- intersection_test(w, 0.03)
  expect_equal(pval(it), max(pval(w), 0.03))
})

test_that("intersection_test has correct class", {
  it <- intersection_test(0.01, 0.05)
  expect_s3_class(it, "intersection_test")
  expect_s3_class(it, "hypothesis_test")
})

test_that("intersection_test stores metadata", {
  it <- intersection_test(0.01, 0.05, 0.10)
  expect_equal(it$n_tests, 3)
  expect_equal(it$component_pvals, c(0.01, 0.05, 0.10))
})

test_that("intersection_test rejects only when ALL components reject", {
  # One non-significant -> intersection not significant
  it <- intersection_test(0.01, 0.80)
  expect_false(is_significant_at(it, 0.05))

  # All significant -> intersection significant
  it2 <- intersection_test(0.01, 0.02, 0.03)
  expect_true(is_significant_at(it2, 0.05))
})

test_that("intersection_test composes with fisher_combine", {
  it <- intersection_test(0.01, 0.02)
  w <- wald_test(estimate = 2.0, se = 1.0)
  combined <- fisher_combine(it, w)
  expect_s3_class(combined, "fisher_combined_test")
})
```

**Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::test()' 2>&1 | tail -5`
Expected: FAIL — "could not find function 'intersection_test'"

**Step 3: Implement intersection_test**

Append to `R/hypothesize.R`:

```r
#' Intersection Test (AND)
#'
#' Combines hypothesis tests using the AND rule: rejects only when ALL
#' component tests reject.
#'
#' @details
#' The intersection test implements the AND operation in the Boolean algebra
#' of hypothesis tests. It rejects the global null hypothesis only when every
#' individual test rejects — the most conservative combination rule.
#'
#' The p-value is simply \eqn{\max(p_1, \ldots, p_k)}, because the
#' intersection rejects at level \eqn{\alpha} if and only if every component
#' rejects at level \eqn{\alpha}, which happens if and only if every
#' component p-value is less than \eqn{\alpha}, which happens if and only
#' if the largest p-value is less than \eqn{\alpha}.
#'
#' This is the intersection-union test (IUT; Berger, 1982). No multiplicity
#' correction is needed — the max operation is inherently conservative.
#'
#' @section Use Case — Bioequivalence:
#' Bioequivalence testing requires showing that a drug's effect is both
#' "not too low" AND "not too high" compared to a reference. This is
#' naturally an intersection test: both one-sided tests must reject.
#'
#' @section Boolean Algebra:
#' Together with [complement_test()] (NOT) and [union_test()] (OR), this
#' function forms a complete Boolean algebra over hypothesis tests. De
#' Morgan's law holds by construction:
#'
#' `union_test(a, b) = complement_test(intersection_test(complement_test(a), complement_test(b)))`
#'
#' @param ... `hypothesis_test` objects or numeric p-values to combine.
#'
#' @return A `hypothesis_test` object of subclass `intersection_test`
#'   containing:
#' \describe{
#'   \item{stat}{The maximum p-value (used as the test statistic)}
#'   \item{p.value}{\eqn{\max(p_1, \ldots, p_k)}}
#'   \item{dof}{Number of component tests}
#'   \item{n_tests}{Number of tests combined}
#'   \item{component_pvals}{Vector of individual p-values}
#' }
#'
#' @examples
#' # Bioequivalence: drug effect must be in [0.8, 1.25] of reference
#' # Test 1: effect > 0.8 (lower bound)
#' # Test 2: effect < 1.25 (upper bound)
#' t_lower <- wald_test(estimate = 1.05, se = 0.08, null_value = 0.8)
#' t_upper <- wald_test(estimate = 1.05, se = 0.08, null_value = 1.25)
#' intersection_test(t_lower, t_upper)
#'
#' # With raw p-values
#' intersection_test(0.01, 0.03, 0.04)  # all < 0.05, so significant
#' intersection_test(0.01, 0.80)         # one large, so not significant
#'
#' @seealso [union_test()] for the OR operation, [complement_test()] for NOT,
#'   [fisher_combine()] for evidence pooling
#' @export
intersection_test <- function(...) {
  inputs <- list(...)
  pvals <- sapply(inputs, function(x) {
    if (inherits(x, "hypothesis_test")) pval(x)
    else if (is.numeric(x) && length(x) == 1) x
    else stop("Arguments must be hypothesis_test objects or numeric p-values")
  })
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
```

**Step 4: Regenerate docs and run tests**

Run: `Rscript -e 'devtools::document(); devtools::test()' 2>&1 | tail -5`
Expected: all tests PASS

**Step 5: Commit**

```bash
git add R/hypothesize.R NAMESPACE man/ tests/testthat/test-hypothesis-tests.R
git commit -m "feat: add intersection_test (AND combinator)"
```

---

### Task 4: union_test (OR via De Morgan)

**Files:**
- Modify: `R/hypothesize.R` (append after intersection_test)
- Modify: `tests/testthat/test-hypothesis-tests.R`

**Step 1: Write failing tests**

```r
# =============================================================================
# union_test (OR via De Morgan)
# =============================================================================

test_that("union_test p-value is min of component p-values", {
  t1 <- wald_test(estimate = 2.0, se = 1.0)
  t2 <- wald_test(estimate = 0.5, se = 1.0)
  ut <- union_test(t1, t2)
  expect_equal(pval(ut), min(pval(t1), pval(t2)))
})

test_that("union_test works with raw p-values", {
  ut <- union_test(0.80, 0.03, 0.50)
  expect_equal(pval(ut), 0.03)
})

test_that("union_test has correct class", {
  ut <- union_test(0.01, 0.05)
  expect_s3_class(ut, "union_test")
  expect_s3_class(ut, "hypothesis_test")
})

test_that("union_test stores metadata", {
  ut <- union_test(0.01, 0.05, 0.10)
  expect_equal(ut$n_tests, 3)
  expect_equal(ut$component_pvals, c(0.01, 0.05, 0.10))
})

test_that("union_test rejects when ANY component rejects", {
  ut <- union_test(0.80, 0.90, 0.01)
  expect_true(is_significant_at(ut, 0.05))

  ut2 <- union_test(0.80, 0.90, 0.60)
  expect_false(is_significant_at(ut2, 0.05))
})

test_that("De Morgan: union = NOT(AND(NOT(a), NOT(b)))", {
  p1 <- 0.03
  p2 <- 0.15
  p3 <- 0.07

  # Direct union
  ut <- union_test(p1, p2, p3)

  # Manual De Morgan construction
  tests <- list(
    hypothesis_test(stat = 0, p.value = p1, dof = 1),
    hypothesis_test(stat = 0, p.value = p2, dof = 1),
    hypothesis_test(stat = 0, p.value = p3, dof = 1)
  )
  dm <- complement_test(
    do.call(intersection_test, lapply(tests, complement_test))
  )

  expect_equal(pval(ut), pval(dm))
})

test_that("union_test composes with other operations", {
  ut <- union_test(0.01, 0.05)
  # Can be combined
  combined <- fisher_combine(ut, 0.03)
  expect_s3_class(combined, "fisher_combined_test")
  # Can be complemented
  ct <- complement_test(ut)
  expect_s3_class(ct, "complemented_test")
})
```

**Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::test()' 2>&1 | tail -5`
Expected: FAIL — "could not find function 'union_test'"

**Step 3: Implement union_test**

Append to `R/hypothesize.R`:

```r
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
#' This is not an approximation — it is the definition. The implementation
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
#' correction — for example, in screening or exploratory analysis.
#'
#' @section Boolean Algebra:
#' Together with [intersection_test()] (AND) and [complement_test()] (NOT),
#' this forms a complete Boolean algebra over hypothesis tests:
#'
#' \itemize{
#'   \item AND: [intersection_test()] — reject when all reject
#'   \item OR: `union_test()` — reject when any rejects
#'   \item NOT: [complement_test()] — reject when original fails to reject
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
  pvals <- sapply(tests, pval)

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
```

**Step 4: Regenerate docs and run tests**

Run: `Rscript -e 'devtools::document(); devtools::test()' 2>&1 | tail -5`
Expected: all tests PASS

**Step 5: Commit**

```bash
git add R/hypothesize.R NAMESPACE man/ tests/testthat/test-hypothesis-tests.R
git commit -m "feat: add union_test (OR via De Morgan's law)"
```

---

### Task 5: invert_test and confidence_set

**Files:**
- Modify: `R/hypothesize.R` (append after union_test)
- Modify: `tests/testthat/test-hypothesis-tests.R`

**Step 1: Write failing tests**

```r
# =============================================================================
# invert_test and confidence_set
# =============================================================================

test_that("invert_test returns a confidence_set", {
  cs <- invert_test(
    test_fn = function(theta) wald_test(estimate = 2.5, se = 0.8, null_value = theta),
    grid = seq(0, 5, by = 0.01)
  )
  expect_s3_class(cs, "confidence_set")
})

test_that("invert_test matches confint.wald_test", {
  est <- 2.5
  se <- 0.8
  cs <- invert_test(
    test_fn = function(theta) wald_test(estimate = est, se = se, null_value = theta),
    grid = seq(-1, 6, by = 0.001),
    alpha = 0.05
  )
  w <- wald_test(estimate = est, se = se)
  ci <- confint(w, level = 0.95)
  expect_equal(lower(cs), ci["lower"], tolerance = 0.005)
  expect_equal(upper(cs), ci["upper"], tolerance = 0.005)
})

test_that("invert_test matches confint.z_test", {
  set.seed(42)
  x <- rnorm(50, mean = 10, sd = 2)
  sigma <- 2
  cs <- invert_test(
    test_fn = function(mu) z_test(x, mu0 = mu, sigma = sigma),
    grid = seq(8, 12, by = 0.001),
    alpha = 0.05
  )
  z <- z_test(x, mu0 = 0, sigma = sigma)
  ci <- confint(z, level = 0.95)
  expect_equal(lower(cs), ci["lower"], tolerance = 0.005)
  expect_equal(upper(cs), ci["upper"], tolerance = 0.005)
})

test_that("invert_test works with user-defined test", {
  # Custom test: reject if |x - theta| > 2
  my_test <- function(theta) {
    x <- 5.0
    stat <- (x - theta)^2
    hypothesis_test(stat = stat, p.value = if (abs(x - theta) > 2) 0.01 else 0.5, dof = 1)
  }
  cs <- invert_test(test_fn = my_test, grid = seq(0, 10, by = 0.1), alpha = 0.05)
  expect_true(lower(cs) >= 2.9)
  expect_true(upper(cs) <= 7.1)
})

test_that("confidence_set stores metadata", {
  test_fn <- function(theta) wald_test(estimate = 1, se = 0.5, null_value = theta)
  cs <- invert_test(test_fn = test_fn, grid = seq(-2, 4, by = 0.1), alpha = 0.10)
  expect_equal(cs$alpha, 0.10)
  expect_equal(cs$level, 0.90)
  expect_true(length(cs$set) > 0)
})

test_that("lower and upper accessors work", {
  cs <- invert_test(
    test_fn = function(theta) wald_test(estimate = 5, se = 1, null_value = theta),
    grid = seq(0, 10, by = 0.01)
  )
  expect_true(lower(cs) < 5)
  expect_true(upper(cs) > 5)
  expect_true(lower(cs) < upper(cs))
})

test_that("print.confidence_set produces output", {
  cs <- invert_test(
    test_fn = function(theta) wald_test(estimate = 5, se = 1, null_value = theta),
    grid = seq(0, 10, by = 0.01)
  )
  expect_output(print(cs), "Confidence set")
})

test_that("invert_test returns empty set when all null values rejected", {
  # Very precise estimate far from grid
  cs <- invert_test(
    test_fn = function(theta) wald_test(estimate = 100, se = 0.01, null_value = theta),
    grid = seq(0, 5, by = 0.01)
  )
  expect_equal(length(cs$set), 0)
})
```

**Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::test()' 2>&1 | tail -5`
Expected: FAIL — "could not find function 'invert_test'"

**Step 3: Implement invert_test, confidence_set, lower, upper**

Append to `R/hypothesize.R`:

```r
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

#' @export
print.confidence_set <- function(x, ...) {
  cat(sprintf("Confidence set (%.0f%% level)\n", x$level * 100))
  cat("-----------------------------\n")
  if (length(x$set) == 0) {
    cat("Empty set (all null values rejected)\n")
  } else {
    cat("Lower: ", min(x$set), "\n")
    cat("Upper: ", max(x$set), "\n")
    cat("Grid points in set: ", length(x$set), "of", length(x$grid), "\n")
  }
  invisible(x)
}

#' Extract the lower bound of a confidence set
#' @param x a confidence_set object
#' @param ... additional arguments (ignored)
#' @return numeric lower bound
#' @export
lower <- function(x, ...) UseMethod("lower")

#' @export
lower.confidence_set <- function(x, ...) {
  if (length(x$set) == 0) return(NA_real_)
  min(x$set)
}

#' Extract the upper bound of a confidence set
#' @param x a confidence_set object
#' @param ... additional arguments (ignored)
#' @return numeric upper bound
#' @export
upper <- function(x, ...) UseMethod("upper")

#' @export
upper.confidence_set <- function(x, ...) {
  if (length(x$set) == 0) return(NA_real_)
  max(x$set)
}
```

**Step 4: Regenerate docs and run tests**

Run: `Rscript -e 'devtools::document(); devtools::test()' 2>&1 | tail -5`
Expected: all tests PASS

**Step 5: Commit**

```bash
git add R/hypothesize.R NAMESPACE man/ tests/testthat/test-hypothesis-tests.R
git commit -m "feat: add invert_test for test-confidence duality"
```

---

### Task 6: Multivariate wald_test

**Files:**
- Modify: `R/hypothesize.R` (modify existing wald_test at ~line 427)
- Modify: `tests/testthat/test-hypothesis-tests.R`

**Step 1: Write failing tests**

```r
# =============================================================================
# wald_test multivariate extension
# =============================================================================

test_that("multivariate wald_test computes correct statistic", {
  est <- c(2.0, 3.0)
  V <- matrix(c(1.0, 0.3, 0.3, 1.0), 2, 2)
  null <- c(0, 0)
  w <- wald_test(estimate = est, vcov = V, null_value = null)

  diff <- est - null
  expected_stat <- as.numeric(t(diff) %*% solve(V) %*% diff)
  expect_equal(test_stat(w), expected_stat)
  expect_equal(dof(w), 2)
  expect_equal(pval(w), pchisq(expected_stat, df = 2, lower.tail = FALSE))
})

test_that("multivariate wald_test with diagonal vcov matches sum of univariates", {
  est <- c(2.0, 3.0)
  se1 <- 0.8
  se2 <- 1.2
  V <- diag(c(se1^2, se2^2))

  w_multi <- wald_test(estimate = est, vcov = V)
  w1 <- wald_test(estimate = est[1], se = se1)
  w2 <- wald_test(estimate = est[2], se = se2)

  expect_equal(test_stat(w_multi), test_stat(w1) + test_stat(w2),
               tolerance = 1e-10)
})

test_that("univariate wald_test still works unchanged", {
  w <- wald_test(estimate = 2.5, se = 0.8)
  expect_equal(test_stat(w), (2.5 / 0.8)^2)
  expect_equal(dof(w), 1)
  expect_true(!is.null(w$z))
})

test_that("multivariate wald_test has correct class", {
  V <- diag(c(1, 1))
  w <- wald_test(estimate = c(1, 2), vcov = V)
  expect_s3_class(w, "wald_test")
  expect_s3_class(w, "hypothesis_test")
})

test_that("wald_test rejects se and vcov together", {
  expect_error(wald_test(estimate = 1, se = 0.5, vcov = matrix(1)),
               "exactly one")
})
```

**Step 2: Run tests to verify the new ones fail**

Run: `Rscript -e 'devtools::test()' 2>&1 | tail -5`
Expected: FAIL on multivariate tests (vcov not recognized)

**Step 3: Modify wald_test**

Replace the existing `wald_test` function body (keep the roxygen block, update it):

Update the roxygen `@param` section to add `vcov` and make `se` optional:

In `R/hypothesize.R`, replace the wald_test function (lines ~427-441) with:

```r
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
```

Also update the roxygen for wald_test: add `@param vcov` and change `@param se` to note it's for univariate. Add `@param null_value` default note. Update the `@details` section to mention multivariate. Add an example showing the multivariate case and the diagonal decomposition.

**Step 4: Regenerate docs and run tests**

Run: `Rscript -e 'devtools::document(); devtools::test()' 2>&1 | tail -5`
Expected: all tests PASS (including existing univariate tests)

**Step 5: Commit**

```bash
git add R/hypothesize.R NAMESPACE man/ tests/testthat/test-hypothesis-tests.R
git commit -m "feat: extend wald_test with multivariate vcov support"
```

---

### Task 7: Update DESCRIPTION version and run final checks

**Files:**
- Modify: `DESCRIPTION` (version 0.10.0 -> 0.11.0)
- Modify: `NEWS.md` (add v0.11.0 changelog)
- Modify: `.zenodo.json` (version field)
- Modify: `CITATION.cff` (version field)

**Step 1: Bump version**

In `DESCRIPTION`, change `Version: 0.10.0` to `Version: 0.11.0`.

**Step 2: Update NEWS.md**

Prepend v0.11.0 section to `NEWS.md`:

```markdown
# hypothesize 0.11.0

## New Features

* `score_test()`: Score (Lagrange multiplier) test, completing the
  Wald/LRT/Score trinity. Supports univariate and multivariate cases.
* `complement_test()`: NOT operation — negates a test (p → 1-p). Connects
  to equivalence testing.
* `intersection_test()`: AND combinator — rejects when all component tests
  reject. P-value = max(p_i).
* `union_test()`: OR combinator — rejects when any component test rejects.
  Implemented via De Morgan's law: NOT(AND(NOT(...))).
* `invert_test()`: Test-confidence duality — inverts any test constructor
  into a confidence set via grid search.
* `lower()` and `upper()`: Generic accessors for confidence set bounds.
* `wald_test()` now accepts a `vcov` matrix for multivariate testing.

## Boolean Algebra

The combination of `complement_test()`, `intersection_test()`, and
`union_test()` forms a Boolean algebra over hypothesis tests. De Morgan's
laws hold by construction: `union_test(a, b)` is literally defined as
`complement_test(intersection_test(complement_test(a), complement_test(b)))`.
```

**Step 3: Update .zenodo.json and CITATION.cff**

Change `"version": "0.10.0"` to `"version": "0.11.0"` in both files.

**Step 4: Run full check**

Run: `Rscript -e 'devtools::document(); devtools::check()' 2>&1 | tail -20`
Expected: 0 errors, 0 warnings

Run: `Rscript -e 'cov <- covr::package_coverage(); cat(sprintf("Coverage: %.2f%%\n", covr::percent_coverage(cov)))' 2>&1 | tail -3`
Expected: 95%+ coverage

**Step 5: Commit**

```bash
git add DESCRIPTION NEWS.md .zenodo.json CITATION.cff
git commit -m "v0.11.0: Boolean algebra extension"
```
