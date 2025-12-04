
# hypothesize

A consistent API for hypothesis testing in R, designed around principles
from *Structure and Interpretation of Computer Programs* (SICP): -
**Data Abstraction**: Tests are objects with a clean accessor
interface - **Closure Property**: Combining tests yields tests -
**Higher-Order Functions**: Transform tests into new tests

## Installation

``` r
# install.packages("devtools")
devtools::install_github("queelius/hypothesize")
```

## The Interface

Every hypothesis test implements the same generic methods:

``` r
# Create a Wald test
w <- wald_test(estimate = 2.5, se = 0.8, null_value = 0)

pval(w)                    # p-value
#> [1] 0.00178
test_stat(w)               # test statistic
#> [1] 9.77
dof(w)                     # degrees of freedom
#> [1] 1
is_significant_at(w, 0.05) # significance check
#> [1] TRUE
```

## Primitive Tests

### Z-Test (simplest case)

``` r
# Test if population mean equals 100 (known sigma = 15)
set.seed(42)
x <- rnorm(30, mean = 105, sd = 15)
z_test(x, mu0 = 100, sigma = 15)
#> Hypothesis test ( z_test )
#> -----------------------------
#> Test statistic:  2.2 
#> P-value:  0.0277 
#> Degrees of freedom:  Inf 
#> Significant at 5% level:  TRUE
```

### Wald Test (general parameters)

``` r
# Test if a regression coefficient equals zero
wald_test(estimate = 1.8, se = 0.7)
#> Hypothesis test ( wald_test )
#> -----------------------------
#> Test statistic:  6.61 
#> P-value:  0.0101 
#> Degrees of freedom:  1 
#> Significant at 5% level:  TRUE
```

### Likelihood Ratio Test (model comparison)

``` r
# Compare nested models
lrt(null_loglik = -150, alt_loglik = -140, dof = 3)
#> Hypothesis test ( likelihood_ratio_test )
#> -----------------------------
#> Test statistic:  20 
#> P-value:  0.00017 
#> Degrees of freedom:  3 
#> Significant at 5% level:  TRUE
```

## Combining Tests (Closure Property)

Fisher’s method combines independent p-values—and returns a hypothesis
test:

``` r
# Three studies, none individually significant
fisher_combine(0.08, 0.12, 0.06)
#> Hypothesis test ( fisher_combined_test )
#> -----------------------------
#> Test statistic:  14.9 
#> P-value:  0.0209 
#> Degrees of freedom:  6 
#> Significant at 5% level:  TRUE
```

## Transforming Tests (Higher-Order Functions)

Adjust p-values for multiple testing:

``` r
tests <- list(
  wald_test(estimate = 2.5, se = 1.0),
  wald_test(estimate = 1.8, se = 0.9),
  wald_test(estimate = 1.2, se = 0.7)
)

# Original p-values
sapply(tests, pval)
#> [1] 0.0124 0.0455 0.0865

# Bonferroni-adjusted
sapply(adjust_pval(tests, method = "bonferroni"), pval)
#> [1] 0.0373 0.1365 0.2594
```

## Duality: Tests ↔ Confidence Intervals

``` r
w <- wald_test(estimate = 5.0, se = 1.2)
confint(w)
#> lower upper 
#>  2.65  7.35
confint(w, level = 0.99)
#> lower upper 
#>  1.91  8.09
```

## Extending the Package

Create new test types by calling `hypothesis_test()`:

``` r
# Custom chi-squared goodness-of-fit wrapper
chisq_gof <- function(observed, expected) {
  stat <- sum((observed - expected)^2 / expected)
  df <- length(observed) - 1
  hypothesis_test(
    stat = stat,
    p.value = pchisq(stat, df, lower.tail = FALSE),
    dof = df,
    superclasses = "chisq_gof_test"
  )
}

chisq_gof(observed = c(45, 35, 20), expected = c(40, 40, 20))
#> Hypothesis test ( chisq_gof_test )
#> -----------------------------
#> Test statistic:  1.25 
#> P-value:  0.535 
#> Degrees of freedom:  2 
#> Significant at 5% level:  FALSE
```

## Learn More

See `vignette("introduction", package = "hypothesize")` for a full
tutorial.
