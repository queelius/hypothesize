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
  expected_stat <- -2 * sum(log(sapply(adjusted, pval)))
  expect_equal(test_stat(combined), expected_stat)
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
