## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Ubuntu 24.04, R 4.3.x
* GitHub Actions (ubuntu-latest, windows-latest, macOS-latest)
* win-builder (R-devel)

## Downstream dependencies

There are no downstream dependencies.

## Resubmission

This is a resubmission. The previous version (0.11.0) was submitted but
this version (1.0.0) adds:

* `lrt()` now accepts standard `logLik` objects with automatic dof derivation
* Input validation on all primitive test constructors
* `confint.z_test()` respects one-sided alternatives
* `adjust_pval()` preserves all original test fields
* Numerical stability fix in `union_test()` for extreme p-values
* All `sapply()` replaced with `vapply()`; ASCII-only R source
* 227 tests with 100% line coverage
