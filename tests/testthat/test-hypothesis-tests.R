# Tests for hypothesize package
# Focus on logic, not I/O

# =============================================================================
# hypothesis_test (base constructor)
# =============================================================================

test_that("hypothesis_test creates object with correct structure", {
  ht <- hypothesis_test(stat = 2.0, p.value = 0.05, dof = 1)

  expect_s3_class(ht, "hypothesis_test")
  expect_equal(test_stat(ht), 2.0)

  expect_equal(pval(ht), 0.05)
  expect_equal(dof(ht), 1)
})

test_that("hypothesis_test stores extra arguments", {
  ht <- hypothesis_test(
    stat = 1.5, p.value = 0.1, dof = 2,
    custom_field = "hello", another = 42
  )

  expect_equal(ht$custom_field, "hello")
  expect_equal(ht$another, 42)
})

test_that("hypothesis_test respects superclasses", {
  ht <- hypothesis_test(
    stat = 1.0, p.value = 0.3, dof = 5,
    superclasses = c("my_test", "another_class")
  )

  expect_s3_class(ht, "my_test")
  expect_s3_class(ht, "another_class")
  expect_s3_class(ht, "hypothesis_test")
  expect_equal(class(ht), c("my_test", "another_class", "hypothesis_test"))
})

test_that("is_significant_at works correctly", {
  sig <- hypothesis_test(stat = 3.0, p.value = 0.01, dof = 1)
  not_sig <- hypothesis_test(stat = 1.0, p.value = 0.10, dof = 1)

  expect_true(is_significant_at(sig, 0.05))
  expect_false(is_significant_at(not_sig, 0.05))
  expect_true(is_significant_at(not_sig, 0.15))
})

# =============================================================================
# lrt (Likelihood Ratio Test)
# =============================================================================

test_that("lrt computes correct test statistic", {
  # LRT stat = -2 * (null_loglik - alt_loglik)
  result <- lrt(null_loglik = -100, alt_loglik = -90, dof = 2)

  expect_equal(test_stat(result), 20)  # -2 * (-100 - (-90)) = -2 * (-10) = 20
  expect_equal(dof(result), 2)
})

test_that("lrt p-value follows chi-squared distribution", {
  result <- lrt(null_loglik = -100, alt_loglik = -90, dof = 2)

  # p-value should be P(chi-sq(2) > 20)
  expected_p <- pchisq(20, df = 2, lower.tail = FALSE)
  expect_equal(pval(result), expected_p)
})

test_that("lrt stores input values", {
  result <- lrt(null_loglik = -150, alt_loglik = -140, dof = 3)

  expect_equal(result$null_loglik, -150)
  expect_equal(result$alt_loglik, -140)
})

test_that("lrt has correct class", {
  result <- lrt(null_loglik = -100, alt_loglik = -95, dof = 1)

  expect_s3_class(result, "likelihood_ratio_test")
  expect_s3_class(result, "hypothesis_test")
})

# =============================================================================
# z_test
# =============================================================================

test_that("z_test computes correct z-statistic", {
  x <- c(10, 12, 11, 13, 9)
  result <- z_test(x, mu0 = 10, sigma = 2)

  # z = (mean(x) - mu0) / (sigma / sqrt(n))
  expected_z <- (mean(x) - 10) / (2 / sqrt(5))
  expect_equal(test_stat(result), expected_z)
})

test_that("z_test two-sided p-value is correct", {
  x <- rep(12, 25)  # mean = 12
  result <- z_test(x, mu0 = 10, sigma = 2, alternative = "two.sided")

  z <- (12 - 10) / (2 / sqrt(25))  # = 5
  expected_p <- 2 * pnorm(-abs(z))
  expect_equal(pval(result), expected_p)
})

test_that("z_test one-sided p-values are correct", {
  x <- rep(12, 25)  # mean = 12

  result_greater <- z_test(x, mu0 = 10, sigma = 2, alternative = "greater")
  result_less <- z_test(x, mu0 = 10, sigma = 2, alternative = "less")

  z <- (12 - 10) / (2 / sqrt(25))  # = 5

  expect_equal(pval(result_greater), pnorm(z, lower.tail = FALSE))
  expect_equal(pval(result_less), pnorm(z))
})

test_that("z_test stores metadata correctly", {
  x <- 1:10
  result <- z_test(x, mu0 = 3, sigma = 1.5, alternative = "less")

  expect_equal(result$null_value, 3)
  expect_equal(result$sigma, 1.5)
  expect_equal(result$n, 10)
  expect_equal(result$estimate, mean(x))
  expect_equal(result$alternative, "less")
})

test_that("z_test has correct class and dof", {
  result <- z_test(1:10, mu0 = 0, sigma = 1)

  expect_s3_class(result, "z_test")
  expect_equal(dof(result), Inf)
})

# =============================================================================
# wald_test
# =============================================================================

test_that("wald_test computes correct z-score and statistic", {
  result <- wald_test(estimate = 2.5, se = 0.5, null_value = 1.5)

  # z = (estimate - null_value) / se
  expected_z <- (2.5 - 1.5) / 0.5  # = 2
  expect_equal(result$z, expected_z)
  expect_equal(test_stat(result), expected_z^2)  # Wald stat is z^2
})

test_that("wald_test p-value uses chi-squared(1) distribution", {
  result <- wald_test(estimate = 3, se = 1, null_value = 0)

  # z = 3, stat = 9
  expected_p <- pchisq(9, df = 1, lower.tail = FALSE)
  expect_equal(pval(result), expected_p)
})

test_that("wald_test stores inputs correctly", {
  result <- wald_test(estimate = 1.5, se = 0.3, null_value = 1.0)

  expect_equal(result$estimate, 1.5)
  expect_equal(result$se, 0.3)
  expect_equal(result$null_value, 1.0)
})

test_that("wald_test defaults null_value to 0", {
  result <- wald_test(estimate = 2, se = 1)

  expect_equal(result$null_value, 0)
  expect_equal(result$z, 2)  # (2 - 0) / 1
})

test_that("wald_test has correct class and dof", {
  result <- wald_test(estimate = 1, se = 0.5)

  expect_s3_class(result, "wald_test")
  expect_equal(dof(result), 1)
})

# =============================================================================
# fisher_combine
# =============================================================================

test_that("fisher_combine computes correct statistic", {
  p1 <- 0.05
  p2 <- 0.10
  p3 <- 0.20

  result <- fisher_combine(p1, p2, p3)

  expected_stat <- -2 * (log(p1) + log(p2) + log(p3))
  expect_equal(test_stat(result), expected_stat)
})

test_that("fisher_combine uses correct degrees of freedom", {
  result <- fisher_combine(0.1, 0.2, 0.3, 0.4)

  expect_equal(dof(result), 8)  # 2 * 4
  expect_equal(result$n_tests, 4)
})

test_that("fisher_combine p-value follows chi-squared distribution", {
  result <- fisher_combine(0.05, 0.05, 0.05)

  stat <- -2 * sum(log(c(0.05, 0.05, 0.05)))
  expected_p <- pchisq(stat, df = 6, lower.tail = FALSE)
  expect_equal(pval(result), expected_p)
})

test_that("fisher_combine accepts hypothesis_test objects", {
  t1 <- wald_test(estimate = 2, se = 1)
  t2 <- wald_test(estimate = 1.5, se = 0.8)

  result <- fisher_combine(t1, t2)

  expected_stat <- -2 * (log(pval(t1)) + log(pval(t2)))
  expect_equal(test_stat(result), expected_stat)
  expect_equal(result$n_tests, 2)
})

test_that("fisher_combine stores component p-values", {
  result <- fisher_combine(0.01, 0.05, 0.10)

  expect_equal(result$component_pvals, c(0.01, 0.05, 0.10))
})

test_that("fisher_combine validates p-values", {
  expect_error(fisher_combine(0.5, -0.1), "P-values must be in \\(0, 1\\]")
  expect_error(fisher_combine(0.5, 1.5), "P-values must be in \\(0, 1\\]")
})

test_that("fisher_combine has correct class", {
  result <- fisher_combine(0.1, 0.2)

  expect_s3_class(result, "fisher_combined_test")
  expect_s3_class(result, "hypothesis_test")
})

# =============================================================================
# confint (duality)
# =============================================================================

test_that("confint.wald_test computes correct 95% CI", {
  w <- wald_test(estimate = 10, se = 2)
  ci <- confint(w)

  z_crit <- qnorm(0.975)
  expect_equal(ci["lower"], c(lower = 10 - z_crit * 2))
  expect_equal(ci["upper"], c(upper = 10 + z_crit * 2))
})

test_that("confint.wald_test respects confidence level", {
  w <- wald_test(estimate = 5, se = 1)

  ci_95 <- confint(w, level = 0.95)
  ci_99 <- confint(w, level = 0.99)

  # 99% CI should be wider
  expect_lt(ci_99["lower"], ci_95["lower"])
  expect_gt(ci_99["upper"], ci_95["upper"])
})

test_that("confint.z_test computes correct CI", {
  x <- rep(100, 25)  # mean = 100, n = 25
  z <- z_test(x, mu0 = 95, sigma = 10)
  ci <- confint(z)

  se <- 10 / sqrt(25)  # = 2
  z_crit <- qnorm(0.975)
  expect_equal(ci["lower"], c(lower = 100 - z_crit * se))
  expect_equal(ci["upper"], c(upper = 100 + z_crit * se))
})

test_that("confint errors for unsupported test types", {
  fisher_result <- fisher_combine(0.1, 0.2)
  expect_error(confint(fisher_result), "not implemented")
})

# =============================================================================
# adjust_pval (higher-order function)
# =============================================================================

test_that("adjust_pval applies Bonferroni correction to single test", {
  w <- wald_test(estimate = 2, se = 1)  # p ~ 0.046
  adj <- adjust_pval(w, method = "bonferroni", n = 10)

  expect_equal(pval(adj), min(pval(w) * 10, 1))
  expect_equal(adj$original_pval, pval(w))
  expect_equal(adj$adjustment_method, "bonferroni")
})

test_that("adjust_pval requires n for single test", {
  w <- wald_test(estimate = 2, se = 1)
  expect_error(adjust_pval(w), "'n' \\(total number of tests\\) must be specified")
})

test_that("adjust_pval handles list of tests", {
  tests <- list(
    wald_test(estimate = 3, se = 1),
    wald_test(estimate = 2, se = 1),
    wald_test(estimate = 1, se = 1)
  )

  adjusted <- adjust_pval(tests, method = "bonferroni")

  expect_length(adjusted, 3)
  for (i in seq_along(adjusted)) {
    expect_s3_class(adjusted[[i]], "adjusted_test")
    expect_equal(adjusted[[i]]$n_tests, 3)
  }
})

test_that("adjust_pval infers n from list length", {
  tests <- list(
    wald_test(estimate = 2, se = 1),
    wald_test(estimate = 1.5, se = 0.8)
  )

  adjusted <- adjust_pval(tests, method = "bonferroni")

  # With n=2, Bonferroni multiplies by 2
  expect_equal(pval(adjusted[[1]]), min(pval(tests[[1]]) * 2, 1))
})

test_that("adjust_pval preserves test statistic and dof", {
  w <- wald_test(estimate = 2.5, se = 0.8)
  adj <- adjust_pval(w, method = "holm", n = 5)

  expect_equal(test_stat(adj), test_stat(w))
  expect_equal(dof(adj), dof(w))
})

test_that("adjust_pval adds adjusted_test to class hierarchy", {
  w <- wald_test(estimate = 2, se = 1)
  adj <- adjust_pval(w, method = "BH", n = 10)

  expect_s3_class(adj, "adjusted_test")
  expect_s3_class(adj, "wald_test")
  expect_s3_class(adj, "hypothesis_test")
})

test_that("adjust_pval with method='none' returns original p-value", {
  w <- wald_test(estimate = 2, se = 1)
  adj <- adjust_pval(w, method = "none", n = 10)

  expect_equal(pval(adj), pval(w))
})

# =============================================================================
# Composition and closure properties
# =============================================================================

test_that("adjusted tests can be combined with fisher_combine", {
  tests <- list(
    wald_test(estimate = 2, se = 1),
    wald_test(estimate = 1.5, se = 0.8)
  )

  adjusted <- adjust_pval(tests, method = "bonferroni")
  combined <- fisher_combine(adjusted[[1]], adjusted[[2]])

  expect_s3_class(combined, "fisher_combined_test")
  # Combined p-values should use the adjusted p-values
  expected_stat <- -2 * sum(log(vapply(adjusted, pval, numeric(1))))
  expect_equal(test_stat(combined), expected_stat)
})

# =============================================================================
# print method
# =============================================================================

test_that("print.hypothesis_test prints expected output", {
  ht <- hypothesis_test(stat = 2.0, p.value = 0.05, dof = 1,
                        superclasses = "my_test")
  expect_output(print(ht), "Hypothesis test")
  expect_output(print(ht), "my_test")
  expect_output(print(ht), "Test statistic:")
  expect_output(print(ht), "P-value:")
  expect_output(print(ht), "Degrees of freedom:")
  expect_output(print(ht), "Significant at 5% level:")
})

test_that("print.hypothesis_test returns x invisibly", {
  ht <- hypothesis_test(stat = 2.0, p.value = 0.05, dof = 1)
  result <- withVisible(print(ht))
  expect_false(result$visible)
  expect_identical(result$value, ht)
})

test_that("fisher_combine rejects non-numeric non-test inputs", {
  expect_error(fisher_combine("not_a_pvalue"),
               "must be hypothesis_test objects or numeric")
})

test_that("all test types work with is_significant_at", {
  z <- z_test(rnorm(30, mean = 1), mu0 = 0, sigma = 1)
  w <- wald_test(estimate = 2, se = 1)
  l <- lrt(null_loglik = -100, alt_loglik = -95, dof = 2)
  f <- fisher_combine(0.01, 0.02, 0.03)

  # All should return logical
  expect_type(is_significant_at(z, 0.05), "logical")
  expect_type(is_significant_at(w, 0.05), "logical")
  expect_type(is_significant_at(l, 0.05), "logical")
  expect_type(is_significant_at(f, 0.05), "logical")
})

# =============================================================================
# score_test
# =============================================================================

test_that("score_test computes correct univariate statistic and p-value", {
  s <- score_test(score = 2.0, fisher_info = 1.0)
  expect_s3_class(s, "score_test")
  expect_s3_class(s, "hypothesis_test")
  expect_equal(test_stat(s), 4.0)
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
  n <- 100; sigma <- 2; xbar <- 5.5; mu0 <- 5.0
  se <- sigma / sqrt(n)
  score_val <- n * (xbar - mu0) / sigma^2
  info_val <- n / sigma^2
  w <- wald_test(estimate = xbar, se = se, null_value = mu0)
  s <- score_test(score = score_val, fisher_info = info_val)
  expect_equal(test_stat(s), test_stat(w), tolerance = 1e-10)
  expect_equal(pval(s), pval(w), tolerance = 1e-10)
})

test_that("score_test multivariate computes correct statistic", {
  score_vec <- c(2.0, 1.0)
  info_mat <- matrix(c(2.0, 0.5, 0.5, 1.0), 2, 2)
  s <- score_test(score = score_vec, fisher_info = info_mat)
  expected <- as.numeric(t(score_vec) %*% solve(info_mat) %*% score_vec)
  expect_equal(test_stat(s), expected)
  expect_equal(dof(s), 2)
  expect_equal(pval(s), pchisq(expected, df = 2, lower.tail = FALSE))
})

test_that("score_test multivariate with diagonal matches sum of univariates", {
  s1 <- score_test(score = 2.0, fisher_info = 1.0)
  s2 <- score_test(score = 3.0, fisher_info = 2.0)
  s_multi <- score_test(score = c(2.0, 3.0), fisher_info = diag(c(1.0, 2.0)))
  expect_equal(test_stat(s_multi), test_stat(s1) + test_stat(s2), tolerance = 1e-10)
})

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
  expect_equal(pval(cc), pval(w), tolerance = 1e-14)
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
  s <- score_test(score = 2.0, fisher_info = 1.0)
  f <- fisher_combine(0.01, 0.05, 0.10)
  expect_s3_class(complement_test(z), "complemented_test")
  expect_s3_class(complement_test(l), "complemented_test")
  expect_s3_class(complement_test(s), "complemented_test")
  expect_s3_class(complement_test(f), "complemented_test")
})

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
  it <- intersection_test(0.01, 0.80)
  expect_false(is_significant_at(it, 0.05))
  it2 <- intersection_test(0.01, 0.02, 0.03)
  expect_true(is_significant_at(it2, 0.05))
})

test_that("intersection_test composes with fisher_combine", {
  it <- intersection_test(0.01, 0.02)
  w <- wald_test(estimate = 2.0, se = 1.0)
  combined <- fisher_combine(it, w)
  expect_s3_class(combined, "fisher_combined_test")
})

test_that("intersection_test rejects bad inputs", {
  expect_error(intersection_test("bad"), "must be")
})

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

test_that("wald_test rejects neither se nor vcov", {
  expect_error(wald_test(estimate = 1), "exactly one")
})

test_that("confint.wald_test errors on multivariate case", {
  V <- diag(c(1, 1))
  w <- wald_test(estimate = c(1, 2), vcov = V)
  expect_error(confint(w), "multivariate")
})

test_that("empty confidence set prints, lower, upper work", {
  # Test with a value far from 0 so nothing is in the CI
  cs <- invert_test(
    function(theta) wald_test(estimate = 100, se = 0.01, null_value = theta),
    grid = seq(0, 1, by = 0.1)
  )
  expect_equal(length(cs$set), 0)
  expect_true(is.na(lower(cs)))
  expect_true(is.na(upper(cs)))
  expect_output(print(cs), "Empty set")
})

test_that("fisher_combine works with a single test", {
  result <- fisher_combine(0.05)
  expect_s3_class(result, "hypothesis_test")
  expect_equal(dof(result), 2)
})

test_that("intersection_test and union_test work with a single argument", {
  it <- intersection_test(0.03)
  expect_equal(pval(it), 0.03)
  ut <- union_test(0.03)
  expect_s3_class(ut, "hypothesis_test")
})

# --- Input validation tests ---

test_that("z_test rejects empty data", {
  expect_error(z_test(numeric(0), sigma = 1), "at least one")
})

test_that("z_test rejects non-positive sigma", {
  expect_error(z_test(1:5, sigma = 0), "positive")
  expect_error(z_test(1:5, sigma = -1), "positive")
})

test_that("wald_test rejects non-positive se", {
  expect_error(wald_test(estimate = 1, se = 0), "positive")
  expect_error(wald_test(estimate = 1, se = -0.5), "positive")
})

test_that("wald_test rejects singular vcov", {
  V <- matrix(c(1, 1, 1, 1), 2, 2)  # singular
  expect_error(wald_test(estimate = c(1, 2), vcov = V), "singular")
})

test_that("score_test rejects non-positive fisher_info", {
  expect_error(score_test(score = 1, fisher_info = 0), "positive")
  expect_error(score_test(score = 1, fisher_info = -1), "positive")
})

test_that("score_test rejects singular fisher_info matrix", {
  M <- matrix(c(1, 1, 1, 1), 2, 2)
  expect_error(score_test(score = c(1, 2), fisher_info = M), "singular")
})

test_that("lrt rejects non-positive dof", {
  expect_error(lrt(null_loglik = -100, alt_loglik = -90, dof = 0), "positive")
})

test_that("lrt warns on negative statistic", {
  expect_warning(
    result <- lrt(null_loglik = -90, alt_loglik = -100, dof = 1),
    "Negative"
  )
  # p-value should be 1 (no evidence against null)
  expect_equal(pval(result), 1)
})

# --- confint.z_test one-sided tests ---

test_that("confint.z_test respects alternative='greater'", {
  z <- z_test(rep(5, 30), mu0 = 0, sigma = 1, alternative = "greater")
  ci <- confint(z)
  expect_equal(ci[["upper"]], Inf)
  expect_true(is.finite(ci[["lower"]]))
})

test_that("confint.z_test respects alternative='less'", {
  z <- z_test(rep(-5, 30), mu0 = 0, sigma = 1, alternative = "less")
  ci <- confint(z)
  expect_equal(ci[["lower"]], -Inf)
  expect_true(is.finite(ci[["upper"]]))
})

test_that("confint.z_test two-sided is finite on both sides", {
  z <- z_test(rep(5, 30), mu0 = 0, sigma = 1)
  ci <- confint(z)
  expect_true(is.finite(ci[["lower"]]))
  expect_true(is.finite(ci[["upper"]]))
})

# --- Boolean ops: stat and dof are NA ---

test_that("intersection_test has NA stat and dof", {
  it <- intersection_test(0.01, 0.03)
  expect_true(is.na(test_stat(it)))
  expect_true(is.na(dof(it)))
  expect_equal(it$n_tests, 2)
})

test_that("union_test has NA stat and dof", {
  ut <- union_test(0.01, 0.03)
  expect_true(is.na(test_stat(ut)))
  expect_true(is.na(dof(ut)))
  expect_equal(ut$n_tests, 2)
})

# --- union_test floating-point precision ---

test_that("union_test preserves extreme p-values without FP cancellation", {
  p_extreme <- 1e-16
  ut <- union_test(p_extreme, 0.5)
  expect_equal(pval(ut), p_extreme)
})

# --- adjust_pval preserves original test fields ---

test_that("adjust_pval preserves original test fields for confint", {
  w <- wald_test(estimate = 5.0, se = 1.2)
  adj <- adjust_pval(w, method = "bonferroni", n = 10)
  ci <- confint(adj)
  expect_true(is.finite(ci[["lower"]]))
  expect_true(is.finite(ci[["upper"]]))
  expect_equal(adj$estimate, 5.0)
  expect_equal(adj$se, 1.2)
})

test_that("wald_test rejects non-matrix vcov", {
  expect_error(wald_test(estimate = c(1, 2), vcov = c(1, 2)), "matrix")
})

test_that("union_test rejects invalid argument types", {
  expect_error(union_test("not_a_test"), "hypothesis_test objects or numeric")
})

test_that("adjust_pval list preserves fields", {
  tests <- list(
    wald_test(estimate = 2.5, se = 0.8),
    wald_test(estimate = 1.2, se = 0.5)
  )
  adjusted <- adjust_pval(tests, method = "bonferroni")
  expect_equal(adjusted[[1]]$estimate, 2.5)
  expect_equal(adjusted[[1]]$se, 0.8)
  expect_equal(adjusted[[2]]$estimate, 1.2)
  # confint should work on adjusted tests
  ci <- confint(adjusted[[1]])
  expect_true(is.finite(ci[["lower"]]))
})
