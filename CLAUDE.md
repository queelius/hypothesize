# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`hypothesize` is an R package providing a consistent API for hypothesis testing, designed around principles from *Structure and Interpretation of Computer Programs* (SICP):

1. **Data Abstraction**: Tests are S3 objects with accessor functions (`pval()`, `test_stat()`, `dof()`)
2. **Closure Property**: Combining tests yields tests (`fisher_combine()`)
3. **Higher-Order Functions**: Transform tests into new tests (`adjust_pval()`)

## Development Commands

```r
# Generate documentation from roxygen2 comments
devtools::document()

# Run tests (83 tests covering all functions)
devtools::test()

# Run tests with coverage
covr::package_coverage()

# Check package (R CMD check)
devtools::check()

# Build vignettes
devtools::build_vignettes()

# Install from source
devtools::install()
```

## Architecture

### Core Design Pattern

```
hypothesis_test (base constructor)
       │
       ├── z_test (simplest primitive)
       ├── wald_test (general parameters)
       ├── lrt (model comparison)
       ├── fisher_combined_test (from fisher_combine)
       └── adjusted_test (from adjust_pval)
```

### Generic Methods (the API)

All hypothesis test objects implement:
- `pval(x)` — extract p-value
- `test_stat(x)` — extract test statistic
- `dof(x)` — extract degrees of freedom
- `is_significant_at(x, alpha)` — check significance

Additional methods for specific types:
- `confint(x)` — extract confidence interval (wald_test, z_test)

### Key Functions

| Function | Purpose | SICP Principle |
|----------|---------|----------------|
| `hypothesis_test()` | Base constructor | Data abstraction |
| `z_test()` | One-sample z-test | Primitive |
| `wald_test()` | Test parameter = value | Primitive |
| `lrt()` | Compare nested models | Primitive |
| `fisher_combine()` | Combine p-values | Closure property |
| `adjust_pval()` | Multiple testing correction | Higher-order function |
| `confint()` | CI from test | Duality |

### Extension Pattern

To add a new hypothesis test:

```r
my_test <- function(data, null_value) {
  stat <- compute_statistic(data, null_value)
  p.value <- compute_pvalue(stat)
  hypothesis_test(
    stat = stat,
    p.value = p.value,
    dof = length(data) - 1,
    superclasses = "my_test",
    data = data,
    null_value = null_value
  )
}
```

The new test automatically inherits all generic methods.

## Key Files

- `R/hypothesize.R` — All package code (~700 lines)
- `tests/testthat/test-hypothesis-tests.R` — 83 tests
- `vignettes/introduction.Rmd` — Pedagogical vignette

## Package Dependencies

- **Imports**: `stats` (pchisq, pnorm, qnorm, p.adjust)
- **Suggests**: `testthat`, `knitr`, `rmarkdown`
