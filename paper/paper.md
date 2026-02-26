---
title: 'hypothesize: A Consistent, Composable API for Hypothesis Testing in R'
tags:
  - R
  - hypothesis testing
  - statistical inference
  - composable software
  - multiple testing
authors:
  - name: Alexander Towell
    orcid: 0000-0001-6443-9897
    affiliation: 1
affiliations:
  - name: Southern Illinois University Edwardsville
    index: 1
date: 24 February 2026
bibliography: paper.bib
---

# Summary

`hypothesize` is an R package that provides a consistent, composable application
programming interface (API) for hypothesis testing. Every test --- z-tests, Wald
tests, likelihood ratio tests, and Fisher combined tests --- is represented as an
S3 object implementing the same accessor generics: `pval()`, `test_stat()`,
`dof()`, and `is_significant_at()`. The package is designed around three principles
drawn from *Structure and Interpretation of Computer Programs*
[@abelson1996structure]: data abstraction (tests are opaque objects accessed through
a uniform interface), the closure property (combining tests via Fisher's method
[@fisher1925methods] yields a test), and higher-order functions (transforming a
test with `adjust_pval()` yields a test). These principles allow hypothesis tests to be composed, adjusted,
and extended without special-case logic.

The package depends only on the base R `stats` package [@R2025] and exports a
deliberately small API: three test constructors (`z_test()`, `wald_test()`,
`lrt()`), one combinator (`fisher_combine()`), one transformer (`adjust_pval()`),
and four accessor generics. Downstream package authors who produce hypothesis test
results can return `hypothesis_test` objects, giving their users a familiar
interface without re-implementing printing, significance checking, or p-value
adjustment.

# Statement of Need

R provides hypothesis testing through many separate functions --- `t.test()`,
`chisq.test()`, `wilcox.test()` --- each returning an `htest` object with
inconsistent slot names and no mechanism for composition. A researcher who needs
to combine evidence across independent tests, apply multiple-testing corrections,
or build a pipeline that treats test results as first-class values must write
ad-hoc glue code to extract p-values, apply corrections, and track provenance.
Package developers face the same problem: every new test requires reimplementing
printing, significance checking, and confidence interval extraction.

`hypothesize` addresses this gap for two audiences. First, applied researchers
who perform multi-stage inference --- for example, testing several model
comparisons and then combining or adjusting the results --- gain a composable
workflow where each step returns an object that the next step can consume.
Second, package developers who implement domain-specific tests can call
`hypothesis_test()` to produce objects that inherit all generic methods,
ensuring consistency across packages without duplicating code. The package
deliberately keeps its scope narrow: it provides the structural abstraction for
test results, not the statistical tests themselves. By doing so, it occupies a
niche that existing tools leave open.

# State of the Field

Several R packages address aspects of hypothesis testing infrastructure, but
none combine a composable object system with a minimal dependency footprint.

The `infer` package [@couch2021infer] implements a tidyverse-compatible grammar
for statistical inference, expressing hypothesis tests as a four-verb pipeline
(`specify`, `hypothesize`, `generate`, `calculate`). Its strength is pedagogical
clarity and integration with the tidyverse ecosystem, but it focuses on simulation-
based inference and does not provide a reusable object class that downstream
packages can return from their own test functions.

The `coin` package [@hothorn2008coin] provides a comprehensive S4 framework for
conditional inference procedures, including permutation tests, two-sample tests,
and independence tests. It is a powerful tool for nonparametric testing, but its
S4 class hierarchy is tightly coupled to the permutation testing framework and
is not designed as a general-purpose return type for arbitrary hypothesis tests.

The `broom` package [@robinson2014broom] takes the complementary approach of
tidying the *output* of existing test functions into data frames. It standardizes
column names across test types, making downstream manipulation easier. However,
`broom` operates on results after the fact; it does not provide an object
abstraction that supports composition (combining or adjusting tests) at the
object level.

The `statsExpressions` package [@patil2021statsexpressions] unifies access to
parametric, nonparametric, robust, and Bayesian tests through a consistent
function interface and produces tidy data frames with pre-formatted statistical
expressions for plotting. Like `broom`, it standardizes output rather than
providing a composable object class.

The `poolr` package [@cinar2021poolr] provides a comprehensive set of methods
for combining p-values --- Fisher's, Stouffer's, Tippett's, and others --- with
corrections for dependence among tests. It is the closest competitor to the
`fisher_combine()` function in `hypothesize`. However, `poolr` focuses on the
combination step itself and returns numeric results; it does not define a
composable test-object class that downstream packages can construct and return.

The `rstatix` package [@kassambara2023rstatix] provides pipe-friendly wrappers
for common statistical tests with consistent tidy output, overlapping with the
"consistent interface" motivation of `hypothesize`. Its focus is on wrapping
existing tests for interactive use rather than providing a constructor API for
new test types.

`hypothesize` differs from these tools in three ways: (1) it defines a minimal
S3 class that any package can construct and return, (2) it supports algebraic
composition through `fisher_combine()` and `adjust_pval()`, and (3) it has no
dependencies beyond base R, making it suitable as an infrastructure dependency.

# Software Design

The package is implemented in a single R source file (approximately 700 lines
including documentation) and exports 10 functions. The central abstraction is
the `hypothesis_test` S3 class, a named list with mandatory fields `stat`,
`p.value`, and `dof`, plus an extensible `...` mechanism for test-specific
metadata. Subclasses (`z_test`, `wald_test`, `likelihood_ratio_test`,
`fisher_combined_test`, `adjusted_test`) are created by prepending to the class
vector, following standard S3 conventions.

Two design decisions merit note. First, `fisher_combine()` accepts either raw
numeric p-values or `hypothesis_test` objects, enabling use both in scripting
contexts and in programmatic pipelines. Second, `adjust_pval()` wraps `stats::p.adjust()` --- supporting
Bonferroni, Holm, Benjamini--Hochberg [@benjamini1995controlling], and other
corrections --- and returns an `adjusted_test` object that preserves the original
p-value, adjustment method, and the full class hierarchy of the input test. This means an adjusted Wald test is simultaneously an `adjusted_test`, a
`wald_test`, and a `hypothesis_test`, so method dispatch works at all levels.

The test suite contains 83 tests covering all exported functions, including
composition scenarios (adjusted tests fed into `fisher_combine()`). The package
passes `R CMD check` with no errors, warnings, or notes.

# Research Impact Statement

The `hypothesize` package is used as infrastructure within the author's
reliability analysis ecosystem, where `maskedcauses` and related packages
return `hypothesis_test` objects from likelihood ratio tests for masked
series system model selection. It is also used in pedagogical settings at
Southern Illinois University Edwardsville to teach the algebraic structure
of hypothesis testing. The package is available through the author's r-universe repository
(<https://queelius.r-universe.dev>) and has been submitted to CRAN.
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
