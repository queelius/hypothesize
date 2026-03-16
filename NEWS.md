# hypothesize 1.0.0

## Improvements since 0.11.0

* `lrt()` now accepts standard R `logLik` objects (from `stats::logLik()`),
  deriving degrees of freedom automatically from their `df` attributes.
* `confint.z_test()` now returns one-sided confidence bounds for one-sided
  tests (`alternative = "less"` or `"greater"`).
* `adjust_pval()` preserves all original test fields (estimate, se, etc.),
  so `confint()` works on adjusted tests.
* `union_test()` computes `min(p)` directly instead of via De Morgan chain,
  avoiding floating-point cancellation at extreme p-values.
* `intersection_test()` and `union_test()` use `NA` for `stat` and `dof`
  (these operations have no natural test statistic).
* Input validation on all primitive constructors: `z_test()` rejects empty
  data and non-positive sigma; `wald_test()` rejects zero/negative se and
  singular vcov; `score_test()` rejects zero/negative fisher_info;
  `lrt()` validates positive dof and warns on negative LRT statistics.
* All `sapply()` replaced with `vapply()` for type safety.
* All examples use `set.seed()` for reproducibility.
* R source is ASCII-only.
* 227 tests, 100% line coverage.

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
