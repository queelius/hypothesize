# Design: Boolean Algebra Extension for hypothesize

**Date**: 2026-02-26
**Author**: Alexander Towell
**Status**: Approved

## Motivation

hypothesize v0.10.0 provides three SICP principles: data abstraction
(uniform accessor API), closure property (fisher_combine), and higher-order
functions (adjust_pval). This extension deepens the algebra by adding:

1. A Boolean algebra over tests: AND (intersection), OR (union), NOT (complement)
2. The test-confidence duality as a first-class operation (invert_test)
3. The score test primitive, completing the Wald/LRT/Score trinity
4. Multivariate polymorphism for wald_test and score_test

The guiding principle is **algebraic completeness with a minimal basis**.
We add the smallest set of primitives from which users can compose
arbitrary test logic, rather than trying to provide every test.

## The Algebra

```
Primitives:     z_test, wald_test, lrt, score_test
Combinators:    fisher_combine (soft-OR), intersection_test (AND), union_test (OR)
Transformers:   adjust_pval, complement_test (NOT), invert_test (duality)
Accessors:      pval, test_stat, dof, is_significant_at, confint, lower, upper
```

De Morgan's law holds by construction:

```
union_test(t1, t2) = complement_test(intersection_test(complement_test(t1), complement_test(t2)))
```

This is not a coincidence or an approximation. union_test is *defined*
this way. The implementation IS the De Morgan law.

## New Functions

### 1. score_test(score, fisher_info, null_value = NULL)

Completes the Wald/LRT/Score trinity. The score (Lagrange multiplier) test
evaluates both the score function and Fisher information at the null value,
unlike the Wald test which evaluates at the MLE.

**Univariate case:**
```
S = U(theta_0)^2 / I(theta_0) ~ chi-sq(1)
```

**Multivariate case** (same function, polymorphic):
```
S = U(theta_0)' I(theta_0)^{-1} U(theta_0) ~ chi-sq(k)
```

The function detects scalar vs. vector input and dispatches accordingly.
No separate multivariate function needed.

Parameters:
- `score`: numeric scalar or vector. The score function U(theta_0).
- `fisher_info`: numeric scalar or matrix. Fisher information I(theta_0).
- `null_value`: optional. Stored for documentation, not used in computation.

Returns: hypothesis_test subclass "score_test".

### 2. complement_test(test)

The NOT operation. Transforms p -> 1 - p.

A complement test rejects when the original test fails to reject. This
connects to equivalence testing: if the original test checks "is theta
different from theta_0?", the complement checks "is theta close to theta_0?".

The complement is a higher-order transformer (like adjust_pval) that
preserves the full class hierarchy. A complemented Wald test is
simultaneously a complemented_test, a wald_test, and a hypothesis_test.

Parameters:
- `test`: a hypothesis_test object.

Returns: hypothesis_test subclass "complemented_test" with additional fields:
- `original_pval`: the pre-complement p-value
- `original_test`: the input test object

### 3. intersection_test(...)

The AND combinator. Rejects iff ALL component tests reject.

p-value: max(p_1, ..., p_k)

This is the intersection-union test (IUT; Berger, 1982). No multiplicity
correction is needed -- the max operation is inherently conservative.

Use case: testing a composite null where all sub-hypotheses must be false.
For example, bioequivalence testing requires showing that a drug's effect
is both "not too low" AND "not too high".

Accepts hypothesis_test objects or raw numeric p-values (like fisher_combine).

Parameters:
- `...`: hypothesis_test objects or numeric p-values.

Returns: hypothesis_test subclass "intersection_test" with additional fields:
- `n_tests`: number of component tests
- `component_pvals`: vector of individual p-values

### 4. union_test(...)

The OR combinator. Rejects iff ANY component test rejects.

Implemented via De Morgan's law:
```r
union_test <- function(...) {
  tests <- list(...)
  complement_test(do.call(intersection_test, lapply(tests, complement_test)))
}
```

p-value: min(p_1, ..., p_k) (falls out from the De Morgan construction).

Note: the uncorrected min(p) is anti-conservative for multiplicity. Users
who need multiplicity control can apply adjust_pval() to the component
tests before combining, or use fisher_combine() which handles this
differently. The documentation will explain this clearly.

The implementation being literally the De Morgan law is a pedagogical
feature, not a limitation.

Accepts hypothesis_test objects or raw numeric p-values.

Parameters:
- `...`: hypothesis_test objects or numeric p-values.

Returns: hypothesis_test subclass "union_test" with additional fields:
- `n_tests`: number of component tests
- `component_pvals`: vector of individual p-values

### 5. invert_test(test_fn, grid, alpha = 0.05)

The test-confidence duality as a first-class operation. Takes a test
constructor function and returns the confidence set: the set of null
values that are not rejected at level alpha.

This is the most SICP function in the package. It takes a function as
input (higher-order) and produces a structured result. Any test constructor
-- including user-defined ones -- can be inverted. This means every test
automatically gets confidence sets for free.

Parameters:
- `test_fn`: a function that takes a single argument (the null value)
  and returns a hypothesis_test object.
- `grid`: numeric vector of candidate null values to test.
- `alpha`: significance level (default 0.05).

Returns: S3 object of class "confidence_set" with:
- `set`: numeric vector of non-rejected null values
- `alpha`: the significance level used
- `level`: 1 - alpha (the confidence level)
- `test_fn`: the input function (stored for reference)
- `grid`: the input grid

New accessors:
- `lower(cs)`: minimum of the confidence set (lower bound)
- `upper(cs)`: maximum of the confidence set (upper bound)
- `print.confidence_set()`: formatted display
- `confint.confidence_set()`: returns c(lower, upper) for compatibility

### 6. wald_test() extension (multivariate)

The existing wald_test gains an optional `vcov` parameter for the
multivariate case:

```r
wald_test(estimate, se = NULL, vcov = NULL, null_value = 0)
```

When `vcov` is provided (and `estimate` is a vector):
```
W = (theta_hat - theta_0)' Sigma^{-1} (theta_hat - theta_0) ~ chi-sq(k)
```

The `se` and `vcov` parameters are mutually exclusive. The `z` field is
only stored for the univariate case. The `dof` is `length(estimate)`.

## New S3 Classes

| Class | Parent | Key extra fields |
|-------|--------|-----------------|
| score_test | hypothesis_test | score, fisher_info, null_value |
| complemented_test | [original class] + hypothesis_test | original_pval, original_test |
| intersection_test | hypothesis_test | n_tests, component_pvals |
| union_test | hypothesis_test | n_tests, component_pvals |
| confidence_set | (standalone) | set, alpha, level, test_fn, grid |

## What We Are NOT Adding

- **bootstrap_test**: Too opinionated for an infrastructure package.
  Bootstrap has many variants; choosing one means excluding others.
  Downstream packages should build bootstrap tests and return
  hypothesis_test objects.
- **t_test**: stats::t.test exists. Users can wrap it with
  hypothesis_test() in three lines.
- **Stouffer's method**: poolr does this comprehensively. Users who
  need it can build a combinator that returns hypothesis_test.
- **Multivariate-specific functions**: Polymorphism in wald_test and
  score_test handles this without new API surface.

## Pedagogical Documentation Plan

The vignette should be restructured to show the algebra building up:

1. **Primitives**: z_test, wald_test, score_test, lrt -- the four ways
   to test a hypothesis. The trinity and their asymptotic equivalence.
2. **Combinators**: fisher_combine (pool evidence), intersection_test
   (all must reject), union_test (any must reject). Boolean algebra
   with De Morgan's law.
3. **Transformers**: adjust_pval (correct for multiplicity),
   complement_test (negate a test). complement_test's connection to
   equivalence testing.
4. **Duality**: invert_test turns any test into a confidence set.
   confint as a special case. The deep connection between testing
   and estimation.
5. **Multivariate**: wald_test and score_test with vectors. The
   independence decomposition (when multivariate = composition of
   univariate). When correlation forces you to use vcov.

Each section demonstrates a principle from SICP:
- Primitives = primitive expressions
- Combinators = means of combination (closure property)
- Transformers = means of abstraction (higher-order functions)
- Duality = the power of the abstraction barrier

## Export Count

Current: 10 exported functions.
After: 18 exported functions (5 new functions + 3 new accessors).
Still deliberately small.

## Testing Strategy

- score_test: verify against wald_test and lrt for equivalent problems
  (asymptotic equivalence). Multivariate case verified against
  univariate sum for independent parameters.
- complement_test: verify p + complement_p = 1. Double complement is
  identity. Preserves class hierarchy.
- intersection_test: verify p = max(component p-values). Verify closure
  property (result works with pval, test_stat, fisher_combine, etc.).
- union_test: verify De Morgan identity holds. Verify p = min(component
  p-values). Verify agrees with complement(intersection(complement(...))).
- invert_test: verify that wald_test inversion matches confint.wald_test.
  Verify that z_test inversion matches confint.z_test. Verify with
  user-defined test constructor.
- wald_test multivariate: verify diagonal vcov matches sum of univariate
  Wald statistics. Verify against known multivariate examples.

## Dependencies

No new dependencies. Everything uses base R and stats (pchisq, qnorm, solve).
