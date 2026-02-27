## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Ubuntu 24.04, R 4.3.x
* GitHub Actions (ubuntu-latest, windows-latest, macOS-latest)

## Downstream dependencies

This is a new CRAN submission. There are no downstream dependencies.

## Changes in v0.11.0

* Added Boolean algebra over hypothesis tests: `complement_test()` (NOT),
  `intersection_test()` (AND), `union_test()` (OR via De Morgan's law)
* Added `invert_test()` for test-confidence duality (grid-based CI inversion)
* Extended `wald_test()` and `score_test()` to multivariate case (vector
  estimates with variance-covariance matrices)
* Added `confidence_set` class with `lower()`, `upper()`, and `print()` methods
* New vignette: `boolean-algebra` covering the Boolean algebra and duality
