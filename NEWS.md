# hypothesize 0.10.0

## New Features

* `z_test()`: One-sample z-test, the simplest hypothesis test primitive
* `fisher_combine()`: Combine independent p-values using Fisher's method (demonstrates closure property)
* `adjust_pval()`: Multiple testing correction as a higher-order function
* `confint()` methods for `wald_test` and `z_test` (test-CI duality)

## Bug Fixes

* Fixed `wald_test()` p-value calculation: now correctly uses chi-squared(1) distribution for the squared z-statistic

## Improvements

* All test constructors store their inputs for introspection
* Enhanced roxygen2 documentation with mathematical formulas and examples
* Added 83 testthat tests
* Added introductory vignette explaining SICP-inspired design principles
* Updated package description to reflect new functionality

# hypothesize 0.9

* Initial release with `hypothesis_test()`, `lrt()`, and `wald_test()`
