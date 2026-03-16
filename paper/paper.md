---
title: 'hypothesize: A Consistent, Composable API for Hypothesis Testing in R'
tags:
  - R
  - hypothesis testing
  - statistical inference
  - composable software
  - multiple testing
  - Boolean algebra
  - test inversion
authors:
  - name: Alexander Towell
    orcid: 0000-0001-6443-9897
    affiliation: 1
affiliations:
  - name: Southern Illinois University Edwardsville
    index: 1
date: 16 March 2026
bibliography: paper.bib
---

# Summary

`hypothesize` is an R package that provides a consistent, composable application
programming interface (API) for hypothesis testing. Every test -- z-tests, Wald
tests, score tests, likelihood ratio tests, and their combinations -- is
represented as an S3 object implementing the same accessor generics: `pval()`,
`test_stat()`, `dof()`, and `is_significant_at()`. The package is designed
around three principles drawn from *Structure and Interpretation of Computer
Programs* [@abelson1996structure]: data abstraction (tests are opaque objects
accessed through a uniform interface), the closure property (combining tests
yields tests), and higher-order functions (transforming a test yields a test).

The package exports 21 functions organized in four layers: primitive test
constructors (`z_test()`, `wald_test()`, `lrt()`, `score_test()`), combinators
that compose tests (`fisher_combine()`, `intersection_test()`, `union_test()`),
transformers that modify tests (`adjust_pval()`, `complement_test()`), and a
duality layer that bridges tests and confidence sets (`confint()`,
`invert_test()`). The Boolean operations form a complete algebra over hypothesis
tests where De Morgan's laws hold by construction. The package depends only on
the base R `stats` package [@R2025].

# Statement of Need

R provides hypothesis testing through many separate functions -- `t.test()`,
`chisq.test()`, `wilcox.test()` -- each returning an `htest` object with
inconsistent slot names and no mechanism for composition. A researcher who needs
to combine evidence across independent tests, apply multiple-testing corrections,
or build a pipeline that treats test results as first-class values must write
ad-hoc glue code to extract p-values, apply corrections, and track provenance.
Package developers face the same problem: every new test requires reimplementing
printing, significance checking, and confidence interval extraction.

`hypothesize` addresses this gap for two audiences. Applied researchers
who perform multi-stage inference gain a composable workflow where each step
returns an object that the next step can consume. Package developers who
implement domain-specific tests can call `hypothesis_test()` to produce
objects that inherit all generic methods, ensuring consistency without
duplicating code. The package provides the structural abstraction for test
results, not the statistical tests themselves.

# State of the Field

Several R packages address aspects of hypothesis testing infrastructure.
`infer` [@couch2021infer] provides a tidyverse-compatible grammar for
simulation-based inference but does not define a reusable object class for
downstream packages. `coin` [@hothorn2008coin] offers a comprehensive S4
framework for permutation tests, tightly coupled to that specific testing
paradigm. `broom` [@robinson2014broom] and `statsExpressions`
[@patil2021statsexpressions] standardize test *output* into tidy data frames
but do not support composition at the object level. `poolr` [@cinar2021poolr]
provides methods for combining p-values (Fisher's, Stouffer's, and others)
but returns numeric results rather than composable test objects. `rstatix`
[@kassambara2023rstatix] wraps common tests for interactive use but does not
offer a constructor API for new test types.

`hypothesize` differs from these tools in three ways: (1) it defines a minimal
S3 class that any package can construct and return, (2) it supports algebraic
composition through Fisher's method, Boolean operations, and p-value adjustment,
and (3) it has no dependencies beyond base R, making it suitable as an
infrastructure dependency.

# Software Design

The package is implemented in a single R source file (approximately 1250 lines
including documentation) and exports 21 functions. The central abstraction is
the `hypothesis_test` S3 class, a named list with mandatory fields `stat`,
`p.value`, and `dof`, plus an extensible `...` mechanism for test-specific
metadata. Subclasses (`z_test`, `wald_test`, `likelihood_ratio_test`,
`score_test`, `fisher_combined_test`, `adjusted_test`, `complemented_test`,
`intersection_test`, `union_test`) are created by prepending to the class
vector, following standard S3 conventions.

## The Trinity of Likelihood-Based Tests

The package implements the three members of the "holy trinity" of
likelihood-based tests [@cox2006principles]: the Wald test, the likelihood
ratio test, and the score (Lagrange multiplier) test. Both `wald_test()` and
`score_test()` are polymorphic -- they accept scalar parameters with standard
errors or vector parameters with variance-covariance matrices, dispatching to
the appropriate chi-squared test internally. The `lrt()` function accepts both
raw numeric log-likelihoods and standard R `logLik` objects (as returned by
`stats::logLik()` for `lm`, `glm`, and hundreds of other model classes),
deriving the degrees of freedom automatically from the `df` attributes when
both inputs are `logLik` objects.

## Boolean Algebra over Hypothesis Tests

The package defines three operations that form a complete Boolean algebra over
hypothesis tests:

- **NOT** (`complement_test()`): negates a test by transforming its p-value as
  $p \to 1 - p$, connecting to equivalence testing [@wellek2010testing].
- **AND** (`intersection_test()`): rejects only when all component tests reject,
  using the intersection-union test [@berger1982multiparameter] with p-value
  $\max(p_1, \ldots, p_k)$.
- **OR** (`union_test()`): rejects when any component test rejects, with p-value
  $\min(p_1, \ldots, p_k)$.

De Morgan's laws hold by construction: `union_test(a, b)` is algebraically
equivalent to `complement_test(intersection_test(complement_test(a),
complement_test(b)))`. The implementation computes `min(p)` directly rather
than through the De Morgan chain to avoid floating-point cancellation at
extreme p-values.

## Test-Confidence Duality

The `invert_test()` function operationalizes the duality between hypothesis
tests and confidence sets [@casella2024statistical]: a $(1-\alpha)$ confidence
set contains exactly those parameter values $\theta_0$ for which the test of
$H_0: \theta = \theta_0$ would not reject at level $\alpha$. Because
`invert_test()` requires only the `pval()` accessor, it works with any test --
including user-defined tests -- without specialized implementations. For the
standard Wald and z-test cases, analytical `confint()` methods provide exact
intervals without grid search.

## Implementation Quality

The test suite contains 227 tests achieving 100% line coverage. The package
passes `R CMD check --as-cran` with no errors, warnings, or notes. All
`sapply()` calls have been replaced with `vapply()` for type safety, all
exported functions have documented examples with `set.seed()` for
reproducibility, and all primitive test constructors validate their inputs
at the boundary (zero standard errors, singular covariance matrices, empty
data, non-positive degrees of freedom).

# Research Impact Statement

The `hypothesize` package is used as infrastructure within the author's
reliability analysis ecosystem, where `maskedcauses` and related packages
return `hypothesis_test` objects from likelihood ratio tests for masked
series system model selection. It is also used in pedagogical settings at
Southern Illinois University Edwardsville to teach the algebraic structure
of hypothesis testing. The package is available on CRAN and through the
author's r-universe repository (<https://queelius.r-universe.dev>).
Its Zenodo archive carries DOI 10.5281/zenodo.18765626 for long-term
citation.

# AI Usage Disclosure

Claude (Anthropic) was used to assist with drafting documentation, test
scaffolding, and this manuscript. All code logic, API design decisions,
and mathematical content were authored and verified by the human author.
The AI-generated text was reviewed, edited, and validated against the source
code and statistical literature before inclusion.

# Acknowledgements

The author thanks the R Core Team for the R language and the `stats` package
on which `hypothesize` depends, and the SICP authors for the design
principles that motivated this work.

# References
